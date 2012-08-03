-module(oc_chef_wm_base).

%% Complete webmachine callbacks
-export([content_types_accepted/2,
         content_types_provided/2,
         finish_request/2,
         forbidden/2,
         is_authorized/2,
         malformed_request/2,
         ping/2,
         post_is_create/2,
         service_available/2]).

%% Default functions available to mixin
-export([auth_info/2,
         validate_request/3]).

%% Helpers for webmachine callbacks
-export([authorized_by_org_membership_check/2,
         create_from_json/5,
         delete_object/3,
         init/2,
         log_request/2,
         update_from_json/4]).

%% Can't use callback specs to generate behaviour_info because webmachine.hrl
%% contains a function definition.

%% -callback validate_request(atom(), #wm_reqdata{}, any()) -> {#wm_reqdata{}, any()}.
%% -callback malformed_request_message(any(), #wm_reqdata{}, any()) -> {[{binary(), [binary()]}]}.
%% -callback request_type() -> string().
%% -callback auth_info(#wm_reqdata{}, any()) -> {not_found | binary(), #wm_reqdata{}, any()}.

%% This is the max size allowed for incoming request bodies.
-define(MAX_SIZE, 1000000).

-include("chef_wm.hrl").

init(ResourceMod, Config) ->
    {ok, init_base_state(ResourceMod, Config)}.

ping(Req, State) ->
    {pong, Req, State}.

init_base_state(ResourceMod, InitParams) ->
    #base_state{reqid_header_name = ?gv(reqid_header_name, InitParams),
                batch_size = ?gv(batch_size, InitParams),
                auth_skew = ?gv(auth_skew, InitParams),
                db_type = ?gv(db_type, InitParams),
                resource_mod = ResourceMod}.

%% @doc Determines if service is available.
%%
%% Also initializes chef_db_context and reqid fields of base_state.
%% And handle other base_state init that depends on `Req'.
service_available(Req, State) ->
    %% TODO: query overload here and send 503 also can consult
    %% config/darklaunch to determine if we are in maint mode.
    OrgName = list_to_binary(wrq:path_info(organization_id, Req)),
    State0 = set_req_contexts(Req, State),
    State1 = State0#base_state{organization_name = OrgName},
    spawn_stats_hero_worker(Req, State1),
    {_GetHeader, State2} = chef_wm_util:get_header_fun(Req, State1),
    {true, Req, State2}.

validate_request(_Verb, Req, State) ->
    {Req, State}.

auth_info(Req, State) ->
    {not_found, Req, State}.

post_is_create(Req, State) ->
    {true, Req, State}.

malformed_request(Req, #base_state{resource_mod=Mod,
                                   auth_skew=AuthSkew}=State) ->
    {GetHeader, State1} = chef_wm_util:get_header_fun(Req, State),
    try
        chef_authn:validate_headers(GetHeader, AuthSkew),
        OrgId = fetch_org_guid(Req, State1),
        Req1 = body_not_too_big(Req),
        {Req2, State2} = Mod:validate_request(wrq:method(Req1), Req1,
                                              State1#base_state{organization_guid = OrgId}),
        {false, Req2, State2}
    catch
        throw:{org_not_found, Org} ->
            Msg = iolist_to_binary([<<"organization '">>, Org, <<"' does not exist.">>]),
            Req3 = wrq:set_resp_body(ejson:encode({[{<<"error">>, [Msg]}]}), Req),
            {{halt, 404}, Req3, State1#base_state{log_msg = org_not_found}};
        throw:bad_clock ->
            Msg1 = malformed_request_message(bad_clock, Req, State),
            Req3 = wrq:set_resp_body(ejson:encode(Msg1), Req),
            {{halt, 401}, Req3, State1#base_state{log_msg = bad_clock}};
                throw:bad_headers ->
            Msg1 = malformed_request_message(bad_headers, Req, State),
            Req3 = wrq:set_resp_body(ejson:encode(Msg1), Req),
            {{halt, 401}, Req3, State1#base_state{log_msg = bad_headers}};
        throw:bad_sign_desc ->
            Msg1 = malformed_request_message(bad_sign_desc, Req, State),
            Req3 = wrq:set_resp_body(ejson:encode(Msg1), Req),
            {{halt, 400}, Req3, State1#base_state{log_msg = bad_sign_desc}};
        throw:{too_big, Msg} ->
            error_logger:info_msg("json too large (~p)", [Msg]),
            Req3 = wrq:set_resp_body(ejson:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 413}, Req3, State1#base_state{log_msg = too_big}};
        throw:Why ->
            Msg = malformed_request_message(Why, Req, State),
            NewReq = wrq:set_resp_body(ejson:encode(Msg), Req),
            {true, NewReq, State1#base_state{log_msg = Why}}
    end.


%% @doc Handle common malformed request tasks with resource-specific callbacks
%%
%% This function does a sanity check on the authn headers (not verifying signing, but does
%% all checks it can without talking to a db).  It also checks for org existence and halts
%% with 404 if the org is not found.  This doesn't strictly belong in malformed_request, but
%% right now all of our resources need this check and end up needing it before
%% resource_exists will get called so we do it here.
%%
%% The caller provides `ValidateFun' which will be given `Req' and `State' as args and
%% should return a `{Req, State}' tuple or throw.  The `ErrorMsgFun' will be called as
%% `ErrorMsgFun(Reason, Req, State)' where `Reason' is the term thrown by `ValidateFun'.
%%

malformed_request_message(bad_clock, Req, State) ->
    {GetHeader, _State1} = chef_wm_util:get_header_fun(Req, State),
    User = case GetHeader(<<"X-Ops-UserId">>) of
               undefined -> <<"">>;
               UID -> UID
           end,
    Msg = iolist_to_binary([<<"Failed to authenticate as ">>, User,
                            <<". Synchronize the clock on your host.">>]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message(bad_sign_desc, _Req, _State) ->
    Msg = <<"Unsupported authentication protocol version">>,
    {[{<<"error">>, [Msg]}]};
malformed_request_message({missing_headers, Missing}, _Req, _State) ->
    Msg = iolist_to_binary([
                            <<"missing required authentication header(s) ">>,
                            bin_str_join(Missing, <<", ">>)]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message({bad_headers, Bad}, _Req, _State) ->
    Msg = iolist_to_binary([
                            <<"bad header(s) ">>,
                            bin_str_join(Bad, <<", ">>)]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message({invalid_json, _}, _Req, _State) ->
    %% in theory, there might be some sort of slightly useful error detail from ejson, but
    %% thus far nothing specific enough to beat out this. Also, would not passing internal
    %% library error messages out to the user when possible.
    {[{<<"error">>, [<<"invalid JSON">>]}]};
malformed_request_message({mismatch, {FieldName, _Pat, _Val}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", FieldName, "' invalid"])]}]};
malformed_request_message({missing, FieldName}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", FieldName, "' missing"])]}]};
malformed_request_message({both_missing, Field1, Field2}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Both fields '", Field1, "' and '", Field2,
				       "' are missing, at least one must be passed"])]}]};
malformed_request_message({client_name_mismatch}, _Req, _State) ->
    {[{<<"error">>, [<<"name and clientname must match">>]}]};
malformed_request_message({bad_client_name, Name, Pattern}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Invalid cookbook name '", Name,
				       "' using regex: '", Pattern, "'."])]}]};

%% Not sure if we want to be this specific, or just want to fold this into an 'invalid JSON'
%% case.  At any rate, here it is.
malformed_request_message({bad_string_list, {Field, _Value}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", Field, "' is not a list of strings"])]}]};
malformed_request_message({bad_ejson_proplist, {Field, _Value}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", Field, "' is not a hash"])]}]};
malformed_request_message({url_json_name_mismatch, {_UrlName, _Mismatch, Type}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary([Type, <<" name mismatch.">>])]}]};
malformed_request_message({bad_run_list, {Field, _Value}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", Field, "' is not a valid run list"])]}]};
malformed_request_message({bad_run_lists, {Field, _Value}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", Field, "' contains invalid run lists"])]}]};
malformed_request_message(invalid_num_versions, _Req, _State) ->
    {[{<<"error">>, [<<"You have requested an invalid number of versions (x >= 0 || 'all')">>]}]};
malformed_request_message(Reason, Req, #base_state{resource_mod=Mod}=State) ->
    Mod:malformed_request_message(Reason, Req, State).

forbidden(Req, #base_state{resource_mod=Mod}=State) ->
    case Mod:auth_info(Req, State) of
        {{halt, Code}, Req1, State1} ->
            {{halt, Code}, Req1, State1};
        {container, ContainerId, Req1, State1} ->
            invert_perm(check_permission(container, ContainerId, Req1, State1));
        {object, ObjectId, Req1, State1} ->
            invert_perm(check_permission(object, ObjectId, Req1, State1))
    end.

invert_perm({true, Req, State}) ->
    {false, Req, State};
invert_perm({false, Req, State}) ->
    {true, Req, State};
invert_perm(Other) ->
    Other.

check_permission(AuthzObjectType, AuthzId, Req, #base_state{reqid=ReqId,
                                                            requestor=Requestor}=State) ->
    #chef_requestor{authz_id = RequestorId} = Requestor,
    Perm = http_method_to_authz_perm(wrq:method(Req)),
    case ?SH_TIME(ReqId, chef_authz, is_authorized_on_resource,
                  (RequestorId, AuthzObjectType, AuthzId, actor, RequestorId, Perm)) of
        true ->
            {true, Req, State};
        false ->
            Msg = iolist_to_binary(["missing ", atom_to_binary(Perm, utf8),
                                    " permission"]),
            JsonMsg = ejson:encode({[{<<"error">>, [Msg]}]}),
            %% eventually we might want to customize the error message, for now, we'll go
            %% with the generic message.
            %% {Req1, State1} = ErrorMsgFun(Req, State),
            Req1 = wrq:set_resp_body(JsonMsg, Req),
            {false, Req1, State#base_state{log_msg = {Perm, forbidden}}};
        Error ->
            error_logger:error_msg("is_authorized_on_resource failed (~p, ~p, ~p): ~p~n",
                                   [Perm, {AuthzObjectType, AuthzId}, RequestorId, Error]),
            {{halt, 500}, Req, State#base_state{log_msg={error, is_authorized_on_resource}}}
    end.


%%%
%%% part of being authorized is being a member of the org; otherwise we fail out early.
%%%
is_authorized(Req, State) ->
    case verify_request_signature(Req, State) of
        {true, Req1, State1} ->
            case authorized_by_org_membership_check(Req1,State1) of
                {false, Req2, State2} ->
                    {{halt, 403}, Req2, State2};
                {true, Req2, State2} ->
                    {true, Req2, State2}
            end;
        {false, ReqOther, StateOther} ->
            %% FIXME: the supported version is determined by the chef_authn application
            %% also, see: https://wiki.corp.opscode.com/display/CORP/RFC+Authentication+Version+Negotiation
            {"X-Ops-Sign version=\"1.0\"", ReqOther, StateOther}
    end.

%%%
%%% Clients are inherently a member of the org, but users are not.
%%% If we add a user to the org, and then disassociate them, there will be acls left behind
%%% granting permissions on the org objects, so we must check user association and
%%% permissions
%%%
authorized_by_org_membership_check(Req, #base_state{requester_type=client}=State) ->
    {true, Req, State};
authorized_by_org_membership_check(Req, State = #base_state{organization_name = OrgName,
                                                            chef_db_context = DbContext}) ->
    {UserName, BypassesChecks} = get_user(Req, State),
    case BypassesChecks of
        true -> {true, Req, State};
        _ ->
            case chef_db:is_user_in_org(DbContext, UserName, OrgName) of
                true ->
                    {true, Req, State};
                false ->
                    Msg = forbidden_message(not_member_of_org, UserName, OrgName),
                    {false, wrq:set_resp_body(ejson:encode(Msg), Req),
                     State#base_state{log_msg = user_not_in_org}};
                Error ->
                    Msg = forbidden_message(unverified_org_membership, UserName, OrgName),
                    {false, wrq:set_resp_body(ejson:encode(Msg), Req),
                     State#base_state{log_msg = {user_not_in_org_error, Error}}}
            end
    end.

forbidden_message(not_member_of_org, User, Org) ->
    Msg = iolist_to_binary([<<"'">>, User, <<"' not associated with organization '">>,
                            Org, <<"'">>]),
    {[{<<"error">>, [Msg]}]};
forbidden_message(unverified_org_membership, User, Org) ->
    Msg = iolist_to_binary([<<"Failed to verify user '">>, User,
                            <<"' as a member of organization '">>,
                            Org, <<"'">>]),
    {[{<<"error">>, [Msg]}]}.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

finish_request(Req, #base_state{reqid = ReqId}=State) ->
    try
        Code = wrq:response_code(Req),
        log_request(Req, State),
        stats_hero:report_metrics(ReqId, Code),
        stats_hero:stop_worker(ReqId),
        case Code of
            500 ->
                %% sanitize response body
                Msg = <<"internal service error">>,
                Json = ejson:encode({[{<<"error">>, [Msg]}]}),
                Req1 = wrq:set_resp_header("Content-Type",
                                           "application/json", Req),
                {true, wrq:set_resp_body(Json, Req1), State};
            _ ->
                {true, Req, State}
        end
    catch
        X:Y ->
            error_logger:error_report({X, Y, erlang:get_stacktrace()})
    end.

-spec verify_request_signature(#wm_reqdata{}, #base_state{}) ->
                                      {boolean(), #wm_reqdata{}, #base_state{}}.
%% @doc Perform request signature verification (authenticate)
%%
%% Fetches user or client certificate and uses it verify the signature
%% on the request.  If the request cannot be verified, then the
%% returned `#wm_reqdata{}' record will have a response body
%% explaining why.
verify_request_signature(Req,
                         #base_state{organization_name = OrgName,
                                     auth_skew = AuthSkew,
                                     chef_db_context = DbContext}=State) ->
    UserName = wrq:get_req_header("x-ops-userid", Req),
    case chef_db:fetch_requestor(DbContext, OrgName, UserName) of
        {not_found, What} ->
            NotFoundMsg = verify_request_message({not_found, What},
                                                 UserName, OrgName),
            {false, wrq:set_resp_body(ejson:encode(NotFoundMsg), Req),
             State#base_state{log_msg = {not_found, What}}};
        #chef_requestor{type = RequestorType0, key_data = KeyData0}=Requestor ->
            %% If the request originated from the webui, we do authn using the webui public
            %% key, not the user's key.
            {RequestorType, KeyData} = select_user_or_webui_key(Req, KeyData0, RequestorType0),
            Body = body_or_default(Req, <<>>),
            HTTPMethod = iolist_to_binary(atom_to_list(wrq:method(Req))),
            Path = iolist_to_binary(wrq:path(Req)),
            {GetHeader, State1} = chef_wm_util:get_header_fun(Req, State),
            case chef_authn:authenticate_user_request(GetHeader, HTTPMethod,
                                                      Path, Body, KeyData,
                                                      AuthSkew) of
                {name, _} ->
                    {true, Req,
                     %% FIXME: teach users of this code to get requestor type from requestor
                     %% record.
                     State1#base_state{requester_type = RequestorType,
                                       requestor = Requestor}};
                {no_authn, Reason} ->
                    Msg = verify_request_message(Reason, UserName, OrgName),
                    Json = ejson:encode(Msg),
                    Req1 = wrq:set_resp_body(Json, Req),
                    {false, Req1, State1#base_state{log_msg = Reason}}
            end
    end.
-spec create_from_json(Req :: #wm_reqdata{}, State :: #base_state{},
                       RecType :: chef_object_name()| chef_cookbook_version,
                       ContainerId ::object_id() | {authz_id, AuthzId::object_id()},
                       ObjectEjson :: ejson_term()) ->
                              {true | {halt, 409 | 500}, #wm_reqdata{}, #base_state{}}.
%% @doc Implements the from_json callback for POST requests to create Chef
%% objects. `RecType' is the name of the object record being created
%% (e.g. `chef_node'). `ContainerId' is the AuthzID of the container for the object being
%% created (e.g. node container authz ID for creating a node). The `ObjectEjson' is the
%% validated and normalized EJSON that was parsed from the request body.
create_from_json(#wm_reqdata{} = Req,
                 #base_state{chef_db_context = DbContext,
                             reqid = ReqId,
                             organization_guid = OrgId,
                             requestor = #chef_requestor{authz_id = ActorId},
                             db_type = DbType} = State,
                 RecType, AuthzInfo, ObjectEjson) ->
    {ok, AuthzId} = maybe_create_authz(ReqId, ActorId, AuthzInfo),

    %% ObjectEjson should already be normalized. Record creation does minimal work and does
    %% not add or update any fields.
    ObjectRec = chef_object:new_record(RecType, OrgId, AuthzId, ObjectEjson,
                                       DbType),
    Id = chef_object:id(ObjectRec),
    Name = chef_object:name(ObjectRec),
    TypeName = chef_object:type_name(ObjectRec),
    %% We send the object data to solr for indexing *first*. If it fails, we'll error out on
    %% a 500 and client can retry. If we succeed and the db call fails or conflicts, we can
    %% safely send a delete to solr since this is a new object with a unique ID unknown to
    %% the world.
    ok = chef_object_db:add_to_solr(TypeName, Id, OrgId,
                                 chef_object:ejson_for_indexing(ObjectRec, ObjectEjson)),
    CreateFun = chef_db:create_fun(ObjectRec),
    case chef_db:CreateFun(DbContext, ObjectRec, ActorId) of
        {conflict, _} ->
            %% ignore return value of solr delete, this is best effort.
            chef_object_db:delete_from_solr(ObjectRec),

            LogMsg = {RecType, name_conflict, Name},
            ConflictMsg = conflict_message(TypeName, Name),
            {{halt, 409}, chef_wm_util:set_json_body(Req, ConflictMsg),
             State#base_state{log_msg = LogMsg}};
        ok ->
            LogMsg = {created, Name},
            Uri = oc_chef_wm_routes:route(TypeName, Req, [{name, Name}]),
            {true,
             chef_wm_util:set_uri_of_created_resource(Uri, Req),
             State#base_state{log_msg = LogMsg}};
        What ->
            %% ignore return value of solr delete, this is best effort.
            chef_object_db:delete_from_solr(ObjectRec),
            {{halt, 500}, Req, State#base_state{log_msg = What}}
    end.

-spec update_from_json(#wm_reqdata{}, #base_state{}, chef_object() | #chef_cookbook_version{}, ejson_term()) ->
                              {true, #wm_reqdata{}, #base_state{}} |
                              {{halt, 400 | 404 | 500}, #wm_reqdata{}, #base_state{}}.
%% @doc Implements the from_json callback for PUT requests to update Chef
%% objects. `OrigObjectRec' should be the existing and unmodified `chef_object()'
%% record. `ObjectEjson' is the parsed EJSON from the request body.
update_from_json(#wm_reqdata{} = Req, #base_state{chef_db_context = DbContext,
                                  organization_guid = OrgId,
                                  requestor = #chef_requestor{authz_id = ActorId},
                             db_type = DbType}=State, OrigObjectRec, ObjectEjson) ->
    ObjectRec = chef_object:update_from_ejson(OrigObjectRec, ObjectEjson, DbType),
    %% Send object to solr for indexing *first*. If the update fails, we will have sent
    %% incorrect data, but that should get corrected when the client retries. This is a
    %% compromise.
    ok = chef_object_db:add_to_solr(chef_object:type_name(ObjectRec),
                                    chef_object:id(ObjectRec),
                                    OrgId,
                                    chef_object:ejson_for_indexing(ObjectRec, ObjectEjson)),

    %% Ignore updates that don't change anything. If the user PUTs identical data, we skip
    %% going to the database and skip updating updated_at. This allows us to avoid RDBMS
    %% specific behavior around updates with unchanged data and race conditions around
    %% updated_at having resolution only to seconds. It also allows us treat updated_at as
    %% an indicator of when the data actually changed.
    case OrigObjectRec =:= ObjectRec of
        true ->
            State1 = State#base_state{log_msg = ignore_update_for_duplicate},
            {true, chef_wm_util:set_json_body(Req, ObjectEjson), State1};
        false ->
            UpdateFun = chef_db:update_fun(ObjectRec),
            case chef_db:UpdateFun(DbContext, ObjectRec, ActorId) of
                ok ->
                    {true, chef_wm_util:set_json_body(Req, ObjectEjson), State};
                not_found ->
                    %% We will get this if no rows were affected by the query. This could
                    %% happen if the object is deleted in the middle of handling this
                    %% request. In this case, we return 404 just as we would if the client
                    %% retried.
                    State1 = State#base_state{log_msg = not_found},
                    Msg = chef_wm_util:not_found_message(chef_object:type_name(ObjectRec),
                                                           chef_object:name(ObjectRec)),
                    Req1 = chef_wm_util:set_json_body(Req, Msg),
                    {{halt, 404}, Req1, State1};
                {conflict, _} ->
                    Name = chef_object:name(ObjectRec),
                    TypeName = chef_object:type_name(ObjectRec),
                    RecType = erlang:element(1,ObjectRec),
                    LogMsg = {RecType, name_conflict, Name},
                    ConflictMsg = conflict_message(TypeName, Name),
                    {{halt, 409}, chef_wm_util:set_json_body(Req, ConflictMsg),
                     State#base_state{log_msg = LogMsg}};
                {error, {checksum_missing, Checksum}} ->
                    % Catches the condition where the user attempts to reference a checksum that
                    % as not been uploaded.
                    % This leaves it open to be generified
                    % Not sure if we want to explicitly assume what is getting passed
                    % is chef_cookbook_version
                    LogMsg = {checksum_missing, Checksum},
                    ErrorMsg = error_message(checksum_missing, Checksum),
                    {{halt, 400}, chef_wm_util:set_json_body(Req, ErrorMsg),
                     State#base_state{log_msg = LogMsg}};
                Why ->
                    State1 = State#base_state{log_msg = Why},
                    {{halt, 500}, Req, State1}
            end
    end.

-spec delete_object(chef_db:db_context(),
                    chef_object() | #chef_cookbook_version{},
                    object_id()) -> ok.
delete_object(DbContext, Object, RequestId) ->
    oc_chef_object_db:delete(DbContext, Object, RequestId).

conflict_message(cookbook_version, _Name) ->
    {[{<<"error">>, [<<"Cookbook already exists">>]}]};
conflict_message(role, _Name) ->
    {[{<<"error">>, [<<"Role already exists">>]}]};
conflict_message(node, _Name) ->
    %% Msg = iolist_to_binary([<<"A node named '">>, Name, <<"' already exists.">>]),
    Msg = <<"Node already exists">>,
    {[{<<"error">>, [Msg]}]};
conflict_message(data_bag_item, {BagName, ItemName}) ->
    Msg = <<"Data Bag Item '", ItemName/binary, "' already exists in Data Bag '",
            BagName/binary, "'.">>,
    {[{<<"error">>, [Msg]}]};
conflict_message(data_bag, _Name) ->
    %% {[{<<"error">>, [<<"Data Bag '", Name/binary, "' already exists">>]}]}.
    {[{<<"error">>, [<<"Data bag already exists">>]}]};
conflict_message(environment, _Name) ->
    {[{<<"error">>, [<<"Environment already exists">>]}]};
conflict_message(client, _Name) ->
    {[{<<"error">>, [<<"Client already exists">>]}]}.

error_message(checksum_missing, Checksum) ->
    {[{<<"error">>, [iolist_to_binary([<<"Manifest has checksum ">>, Checksum,
                                       <<" but it hasn't yet been uploaded">>])]}]}.

verify_request_message({not_found, org}, _User, Org) ->
    Msg = iolist_to_binary([<<"organization '">>, Org, <<"' does not exist.">>]),
    {[{<<"error">>, [Msg]}]};
verify_request_message({not_found, _}, User, _Org) ->
    Msg = iolist_to_binary([<<"Failed to authenticate as '">>, User, <<"'. ">>,
                            <<"Ensure that your node_name and client key ">>,
                            <<"are correct.">>]),
    {[{<<"error">>, [Msg]}]};
verify_request_message(bad_sig, User, _Org) ->
    Msg = iolist_to_binary([<<"Invalid signature for user or client '">>,
                            User,<<"'">>]),
    {[{<<"error">>, [Msg]}]};
verify_request_message(ErrorType, User, Org)  when
      ErrorType =:= not_associated_with_org orelse
      ErrorType =:= unverified_org_membership ->
    Msg = iolist_to_binary([<<"'">>, User,
                            <<"' is not associated with organization '">>,
                            Org, <<"'">>]),
    {[{<<"error">>, [Msg]}]}.

body_or_default(Req, Default) ->
    case wrq:req_body(Req) of
        undefined -> Default;
        Body -> Body
    end.

set_req_contexts(Req, #base_state{reqid_header_name = HeaderName} = State) ->
    ReqId = read_req_id(HeaderName, Req),
    AuthzContext = chef_authz:make_context(ReqId),
    DbContext = chef_db:make_context(ReqId),
    State#base_state{chef_authz_context = AuthzContext,
                     chef_db_context = DbContext,
                     reqid = ReqId}.

read_req_id(ReqHeaderName, Req) ->
    case wrq:get_req_header(ReqHeaderName, Req) of
        undefined ->
            base64:encode(term_to_binary(make_ref()));
        HV ->
            iolist_to_binary(HV)
    end.

spawn_stats_hero_worker(Req, #base_state{resource_mod = Mod,
                                         organization_name = OrgName,
                                         reqid = ReqId}) ->
    RequestLabel = Mod:request_type(),
    Config = [{request_id, ReqId},
              {org_name, OrgName},
              %% FIXME: pull this out into app config
              {my_app, <<"chefAPI">>},
              {request_label, RequestLabel},
              {request_action, atom_to_list(wrq:method(Req))},
              %% FIXME: make this list a define/app config
              {upstream_prefixes, [<<"rdbms">>, <<"couch">>, <<"authz">>, <<"solr">>]}],
    stats_hero_worker_sup:new_worker(Config).

log_request(Req, #base_state{reqid = ReqId, log_msg = Msg, organization_name = Org}) ->
    Status = wrq:response_code(Req),
    Tuples = [{req_id, ReqId},
              {status, Status},
              {org_name, Org},
              {method, wrq:method(Req)},
              {path, wrq:raw_path(Req)},
              {user, wrq:get_req_header("x-ops-userid", Req)},
              {msg, {raw, Msg}}],
    PerfTuples = stats_hero:snapshot(ReqId, agg),
    Level = log_level(Status),
    fast_log:Level(erchef, Tuples ++ PerfTuples).

log_level(Code) when Code >= 500 ->
    err;
log_level(_) ->
    info.

bin_str_join(L, Sep) ->
    bin_str_join(L, Sep, []).

bin_str_join([H], _Sep, Acc) ->
    lists:reverse([<<"'">>, H, <<"'">>|Acc]);
bin_str_join([H | T], Sep, Acc) ->
    bin_str_join(T, Sep, [Sep, <<"'">>, H, <<"'">> | Acc]).

fetch_org_guid(_Req, #base_state{organization_guid = Id}) when is_binary(Id) ->
    Id;
fetch_org_guid(Req, #base_state{organization_guid = undefined,
                                chef_db_context = DbContext}) ->
    OrgName = list_to_binary(wrq:path_info(organization_id, Req)),
    case chef_db:fetch_org_id(DbContext, OrgName) of
        not_found -> throw({org_not_found, OrgName});
        Guid -> Guid
    end.

http_method_to_authz_perm('DELETE') ->
    delete;
http_method_to_authz_perm('GET') ->
    read;
http_method_to_authz_perm('POST') ->
    create;
http_method_to_authz_perm('PUT') ->
    update.

%%% @doc Return appropriate public key based on request source
%%%
%%% Requests coming from the webui, marked by the 'X-Ops-Request-Source' header read the
%%% webui public key and use that for authn. Otherwise this function just passes through the
%%% "KeyData" arg which is the user or client public key.
%%%
%%% The webui public key is fetched from the chef_keyring service. The 'X-Ops-WebKey-Tag'
%%% header specifies which key id to use, or we use the 'default' key if it is missing.
%%%
select_user_or_webui_key(Req, KeyData, RequestorType) ->
    %% Request origin is determined by the X-Ops-Request-Source header.  This is still secure
    %% because the request needs to have been signed with the webui private key.
    case wrq:get_req_header("x-ops-request-source", Req) of
        "web" ->
            WebKeyTag =
                case wrq:get_req_header("x-ops-webkey-tag", Req) of
                    undefined ->
                        default;
                    "" ->
                        default;
                    Tag ->
                        try
                            list_to_existing_atom(Tag)
                        catch
                            %% The proplist for webui_pub_key_list has been parsed, so the
                            %% key should exist as an atom
                            throw:badarg ->
                                error_logger:error_report({"unknown webkey tag", Tag,
                                                           erlang:get_stacktrace()}),
                                %% alternately, we could just use the default key instead of failing;
                                %% but I prefer noisy errors
                                throw({badarg, "unknown webkey tag", Tag})
                        end
                end,
            case chef_keyring:get_key(WebKeyTag) of
                {ok, Key} ->
                    {webui, Key};
                {error, unknown_key} ->
                    Msg = io_lib:format("Failed finding key ~w", [WebKeyTag]),
                    error_logger:error_report({no_such_key, Msg, erlang:get_stacktrace()}),
                    throw({no_such_key, WebKeyTag})
            end;
        _Else ->
            {RequestorType, KeyData}
    end.

%% Tells whether this user is the superuser.
is_superuser(UserName) ->
    case application:get_env(oc_chef_wm, superusers) of
        {ok,Superusers} -> lists:member(UserName, Superusers);
        undefined -> false
    end.

%% Get the username from the request (and tell whether it is a superuser)
get_user(Req, #base_state{superuser_bypasses_checks = SuperuserBypassesChecks}) ->
    UserName = list_to_binary(wrq:get_req_header("x-ops-userid", Req)),
    BypassesChecks = SuperuserBypassesChecks andalso is_superuser(UserName),
    {UserName, BypassesChecks}.

-spec body_not_too_big(#wm_reqdata{}) -> #wm_reqdata{}.
%% Verify that the request body is not larger than ?MAX_SIZE bytes. Throws `{too_big, Msg}`
%% if the request body is too large.
body_not_too_big(Req) ->
    body_not_too_big(wrq:method(Req), wrq:set_max_recv_body(?MAX_SIZE, Req)).

body_not_too_big(Method, Req) when Method =:= 'POST';
                                   Method =:= 'PUT' ->
    try
        %% Force a read of request body. Webmachine memoizes this in the process
        %% dictionary. Webmachine will read in chunks and call exit/1 if the body exceeds
        %% the max set above. It would be nice if there was something other than a string to
        %% match against. TODO: patch webmachine.
        wrq:req_body(Req),
        Req
    catch
        exit:"request body too large" ->
            Msg = iolist_to_binary([<<"JSON must be no more than ">>,
                                    integer_to_list(?MAX_SIZE),
                                    <<" bytes.">>]),
            throw({too_big, Msg})
    end;
body_not_too_big(_Method, Req) ->
    Req.

-spec maybe_create_authz(ReqId::binary(), ActorId::object_id(),
                         ContainerId::object_id() | {authz_id, AuthzId::object_id()})
    -> {ok, object_id()} | {error, _}.
%% @doc Helper function to deal with creating an AuthzId where needed.  If an AuthzId is
%% passed in the just use that other wise create a new one based on the container AuthzId
maybe_create_authz(_ReqId, _ActorId, {authz_id, AuthzId}) ->
    {ok, AuthzId};
maybe_create_authz(ReqId, ActorId, ContainerId) when is_binary(ContainerId) ->
    %% Note: potential race condition.  If we don't have perms, the create will fail.
    %% Although we checked rights above, they could have changed.
    ?SH_TIME(ReqId, chef_authz, create_object_with_container_acl, (ActorId, ContainerId)).
