%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(oc_chef_wm_base).

%% Complete webmachine callbacks
-export([forbidden/2,
         is_authorized/2,
         service_available/2]).

%% Helpers for webmachine callbacks
-export([authorized_by_org_membership_check/2]).

%% "Grab Bag" functions that will also need to be implemented by other base resources
-export([assemble_principal_ejson/3,
         check_cookbook_authz/3,
         delete_object/3,
         object_creation_hook/2,
         stats_hero_label/1,
         stats_hero_upstreams/0]).

%% Can't use callback specs to generate behaviour_info because webmachine.hrl
%% contains a function definition.

%% -callback validate_request(atom(), #wm_reqdata{}, any()) -> {#wm_reqdata{}, any()}.
%% -callback malformed_request_message(any(), #wm_reqdata{}, any()) -> {[{binary(), [binary()]}]}.
%% -callback request_type() -> string().
%% -callback auth_info(#wm_reqdata{}, any()) -> {not_found | binary(), #wm_reqdata{}, any()}.

%% This is the max size allowed for incoming request bodies.
-define(MAX_SIZE, 1000000).

-include_lib("chef_wm/include/chef_wm.hrl").

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

forbidden(Req, #base_state{resource_mod = Mod} = State) ->
    case Mod:auth_info(Req, State) of
        {{halt, 403}, Req1, State1} ->
            {Req2, State2} = set_forbidden_msg(Req1, State1),
            {true, Req2, State2};
        {{halt, Code}, Req1, State1} ->
            {{halt, Code}, Req1, State1};
        {{create_in_container, Container}, Req1, State1} ->
            create_in_container(Container, Req1, State1);
        {{container, Container}, Req1, State1} ->
            ContainerId = fetch_container_id(Container, Req1, State1),
            invert_perm(check_permission(container, ContainerId, Req1, State1));
        {{Type, ObjectId}, Req1, State1} when Type =:= object;
                                              Type =:= actor ->
            invert_perm(check_permission(Type, ObjectId, Req1, State1));
        {AuthTuples, Req1, State1} when is_list(AuthTuples)->
            %% NOTE: multi_auth_check does not handle create_in_container yet, and expects
            %% each auth tuple to have a permission.  This code path is currently only used
            %% by the depsolver endpoint.
            case multi_auth_check(AuthTuples, Req1, State1) of
                true ->
                    %% All auth checks out, so we're not forbidden
                    {false, Req1, State1};
                {false, {_AuthzObjectType, _AuthzId, Permission}} ->
                    %% NOTE: No specific message for the auth check that failed (but this is
                    %% the same behavior we had before)
                    {Req2, State2} = set_forbidden_msg(Permission, Req1, State1),
                    {true, Req2, State2};
                {Error, {AuthzObjectType, AuthzId, Permission}} ->
                    #base_state{requestor_id=RequestorId} = State1,
                    %% TODO: Extract this logging message, as it is used elsewhere, too
                    error_logger:error_msg("is_authorized_on_resource failed (~p, ~p, ~p): ~p~n",
                                           [Permission, {AuthzObjectType, AuthzId}, RequestorId, Error]),
                    {{halt, 500}, Req, State1#base_state{log_msg={error, is_authorized_on_resource}}}
            end;
        {authorized, Req1, State1} ->
            {false, Req1, State1}
    end.

%% @doc Performs multiple authorization checks in sequence.  If all pass, returns true.  The
%% first check that is false or returns an error, however, halts short-circuits any further
%% checks and returns the result along with the auth_tuple() of the failing authorization
%% check (useful for error message generation).
-spec multi_auth_check(AuthChecks :: [auth_tuple()],
                       Req :: wm_req(),
                       State :: #base_state{}) -> true |
                                                  {false,
                                                   FailingTuple :: auth_tuple()} |
                                                  {Error :: term(),
                                                   FailingTuple :: auth_tuple()}.
multi_auth_check([], _Req, _State) ->
    %% Nothing left to check, must be OK
    true;
multi_auth_check([CurrentTuple|Rest], Req, State) ->
    case auth_check(CurrentTuple, Req, State) of
        true ->
            %% That one checked out; check the rest
            multi_auth_check(Rest, Req, State);
        false ->
            %% That one failed; no need to continue
            {false, CurrentTuple};
        Error ->
            %% That one REALLY failed; send it out for use in error messages
            {Error, CurrentTuple}
    end.

%% @doc Perform a simple authorization check.  Only indicates whether the requested
%% permission is allowed or not; does no manipulation of either Req or State.
%%
%% No function head for the `create_in_container` check, because that's not needed at this
%% time.  Further refactorings may change this.
%% -spec auth_check(AuthCheck :: auth_tuple(),
%%                  Req :: wm_req(),
%%                  State :: #base_state{}) -> true | false | Error :: term().
auth_check({container, Container, Permission}, Req, State) ->
    ContainerId = fetch_container_id(Container, Req, State),
    has_permission(container, ContainerId, Permission, Req, State);
auth_check({object, ObjectId, Permission}, Req, State) ->
    has_permission(object, ObjectId, Permission, Req, State).

%% Called by forbidden/2 when the resource module wants to create a
%% new Chef Object within the container specified by the return value
%% of the resource module's auth_info function. We attempt to create
%% the authz object and return 403 if this fails due to lack of CREATE
%% permission. Otherwise, the created AuthzId is stored in the
%% resource_state record using set_authz_id/2 (which knows how to deal
%% with the different resource_state records).
create_in_container(client=Container, Req,
                    #base_state{requestor=#chef_client{validator=true},
                                resource_state=#client_state{client_data=Data}
                               }=State) ->
    %% This function head is an abomination and an affront to all that is good and pure.
    %%
    %% HOWEVER, if we actually add validators to ACL of the client container, then they
    %% automatically get those same permissions on any subsequently-created clients, due to
    %% how we currently inherit the container ACL as a "template" for new items.
    %%
    %% It used to be the case that validators needed both CREATE and READ permission on
    %% the clients container (CREATE to actually create a client, READ to subsequently grab
    %% the ACL of the client container in order to merge it into the ACL of the new client).
    %% However, it appears that this requirement has been relaxed in recent history, such that
    %% READ is not explicitly required to read an ACL; membership in the ACL (any permission)
    %% is sufficient.
    %%
    %% This means that we only really need to handle the CREATE case. To do this, we
    %% effectively replace the validator's AuthzId with that of the Authz superuser (for this
    %% one operation!) and use it to do the creation.
    %%
    %% NOTE: having a CREATE permission on an already created object that isn't a container
    %% is, in fact, meaningless, but there's no sense in storing additional data.  We're
    %% going to loop back through later and fix up that situation.  Having extra "noise" in
    %% the ACLs is confusing anyway, and this is an area of our API where we should not be
    %% confusing.
    %%
    %% TODO: we really should differentiate between "container permissions" and "permission
    %% templates".  We'll take a look at this in an upcoming version of Bifrost.
    %%
    %% Oh, and validators shouldn't be able to create other validators (to mirror the behavior
    %% of the Open Source Chef Server), so we need to check the contents of the
    %% client-to-be's data, which is a proplist, and so not very amenable to pattern
    %% matching :(
    CreatingAValidator = ej:get({<<"validator">>}, Data),

    case CreatingAValidator of
        true ->
            %% NOT IN MY HOUSE!
            {true, Req, State};  %% answers the question "is this operation forbidden?"
        false ->
            %% We'll pass the atom 'superuser' as a way to indicate to
            %% downstream code that this should be done by the Authz
            %% superuser (NOT the Chef API platform superuser!)

            do_create_in_container(Container, Req, State, superuser)
    end;
create_in_container(Container, Req, #base_state{requestor_id = RequestorId} = State) ->
    %% Here, the requestor isn't a validator client, so they should go through the normal
    %% auth checking process.
    do_create_in_container(Container, Req, State, RequestorId).

%% @doc Perform the actual creation of a new entity.
do_create_in_container(Container, Req,
                       #base_state{chef_authz_context = AuthzContext,
                                   organization_guid = OrgId,
                                   resource_state = RS} = State,
                       EffectiveRequestorId) ->
    case oc_chef_authz:create_entity_if_authorized(AuthzContext, OrgId,
                                                   EffectiveRequestorId, Container) of
        {ok, AuthzId} ->
            State1 = State#base_state{resource_state = set_authz_id(AuthzId, RS)},
            %% return forbidden: false
            {false, Req, State1};
        {error, forbidden} ->
            {Req1, State1} = set_forbidden_msg(Req, State),
            %% return forbidden: true
            {true, Req1, State1}
    end.

%% Called by forbidden/2 when the resource module wants to do authz based on the ACL of the
%% specified `Container'.
%% TODO - Can we just dispense with the Req parameter since it isn't used??
fetch_container_id(Container, _Req, #base_state{chef_authz_context = AuthzContext,
                                                organization_guid = OrgId}) ->
    oc_chef_authz:get_container_aid_for_object(AuthzContext, OrgId, Container).

invert_perm({true, Req, State}) ->
    {false, Req, State};
invert_perm({false, Req, State}) ->
    {true, Req, State};
invert_perm(Other) ->
    Other.

%% @doc Performs simple permission check
%% -spec has_permission(AuthzObjectType :: authz_object(),
%%                      AuthzId :: object_id(),
%%                      Permission :: permission(),
%%                      Req :: wm_req(),
%%                      State :: #base_state{}) -> true | false | Error :: term().
has_permission(AuthzObjectType, AuthzId, Permission, _Req,
               #base_state{reqid=ReqId, requestor_id=RequestorId}) ->
    ?SH_TIME(ReqId, oc_chef_authz, is_authorized_on_resource,
                  (RequestorId, AuthzObjectType, AuthzId, actor, RequestorId, Permission)).

%% NOTE: derives the permission check from the HTTP verb of the Request
check_permission(AuthzObjectType, AuthzId, Req,
                 #base_state{requestor_id=RequestorId}=State) ->
    Perm = http_method_to_authz_perm(Req),
    case has_permission(AuthzObjectType, AuthzId, Perm, Req, State) of
        true ->
            {true, Req, State};
        false ->
            {Req1, State1} = set_forbidden_msg(Req, State),
            {false, Req1, State1};
        Error ->
            error_logger:error_msg("is_authorized_on_resource failed (~p, ~p, ~p): ~p~n",
                                   [Perm, {AuthzObjectType, AuthzId}, RequestorId, Error]),
            {{halt, 500}, Req, State#base_state{log_msg={error, is_authorized_on_resource}}}
    end.

%% part of being authorized is being a member of the org; otherwise we
%% fail out early.
is_authorized(Req, State) ->
    case chef_wm_base:verify_request_signature(Req, State) of
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
            {"X-Ops-Sign version=\"1.0\" version=\"1.1\"", ReqOther, StateOther}
    end.

%% Clients are inherently a member of the org, but users are not.  If
%% we add a user to the org, and then disassociate them, there will be
%% acls left behind granting permissions on the org objects, so we
%% must check user association and permissions
authorized_by_org_membership_check(Req, #base_state{requestor=#chef_client{}}=State) ->
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
                    {false, wrq:set_resp_body(chef_json:encode(Msg), Req),
                     State#base_state{log_msg = user_not_in_org}};
                Error ->
                    Msg = forbidden_message(unverified_org_membership, UserName, OrgName),
                    {false, wrq:set_resp_body(chef_json:encode(Msg), Req),
                     State#base_state{log_msg = {user_not_in_org_error, Error}}}
            end
    end.

set_forbidden_msg(Perm, Req, State) when is_atom(Perm)->
    Msg = iolist_to_binary(["missing ", atom_to_binary(Perm, utf8), " permission"]),
    JsonMsg = chef_json:encode({[{<<"error">>, [Msg]}]}),
    Req1 = wrq:set_resp_body(JsonMsg, Req),
    {Req1, State#base_state{log_msg = {Perm, forbidden}}}.

%% Assumes the permission can be derived from the HTTP verb of the request; this is the
%% original behavior of this function, prior to the addition of set_forbidden_msg/3.
%%
%% TODO: Reconcile these in a future refactoring.
set_forbidden_msg(Req, State) ->
    Perm = http_method_to_authz_perm(Req),
    set_forbidden_msg(Perm, Req, State).

forbidden_message(not_member_of_org, User, Org) ->
    Msg = iolist_to_binary([<<"'">>, User, <<"' not associated with organization '">>,
                            Org, <<"'">>]),
    {[{<<"error">>, [Msg]}]};
forbidden_message(unverified_org_membership, User, Org) ->
    Msg = iolist_to_binary([<<"Failed to verify user '">>, User,
                            <<"' as a member of organization '">>,
                            Org, <<"'">>]),
    {[{<<"error">>, [Msg]}]}.

-spec delete_object(chef_db:db_context(),
                    chef_object() | #chef_cookbook_version{},
                    object_id()) -> ok.
delete_object(DbContext, Object, RequestId) ->
    oc_chef_object_db:delete(DbContext, Object, RequestId).

set_req_contexts(Req, #base_state{reqid_header_name = HeaderName} = State) ->
    ReqId = read_req_id(HeaderName, Req),
    {GetHeader, State1} = chef_wm_util:get_header_fun(Req, State),
    AuthzContext = oc_chef_authz:make_context(ReqId),
    Darklaunch = xdarklaunch_req:parse_header(GetHeader),
    DbContext = chef_db:make_context(ReqId, Darklaunch),
    State1#base_state{chef_authz_context = AuthzContext,
                     chef_db_context = DbContext,
                     darklaunch = Darklaunch,
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
                                         reqid = ReqId,
                                         metrics_config = MetricsConfig}) ->
    RequestLabel = Mod:request_type(),
    Config = [{request_id, ReqId},
              {org_name, OrgName},
              {my_app, ?gv(root_metric_key, MetricsConfig)},
              {request_label, RequestLabel},
              {request_action, atom_to_list(wrq:method(Req))},
              {label_fun, ?gv(stats_hero_label_fun, MetricsConfig)},
              {upstream_prefixes, ?gv(stats_hero_upstreams, MetricsConfig)}],
    stats_hero_worker_sup:new_worker(Config).

http_method_to_authz_perm(#wm_reqdata{}=Req) ->
    http_method_to_authz_perm(wrq:method(Req));
http_method_to_authz_perm('DELETE') ->
    delete;
http_method_to_authz_perm('GET') ->
    read;
http_method_to_authz_perm('POST') ->
    create;
http_method_to_authz_perm('PUT') ->
    update.

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

set_authz_id(Id, #client_state{}=Cl) ->
    Cl#client_state{client_authz_id = Id};
set_authz_id(Id, #cookbook_state{}=C) ->
    C#cookbook_state{authz_id = Id};
set_authz_id(Id, #environment_state{}=E) ->
    E#environment_state{environment_authz_id = Id};
set_authz_id(Id, #node_state{}=N) ->
    N#node_state{node_authz_id = Id};
set_authz_id(Id, #role_state{}=R) ->
    R#role_state{role_authz_id = Id};
set_authz_id(Id, #sandbox_state{}=S) ->
    S#sandbox_state{sandbox_authz_id = Id};
set_authz_id(Id, #data_state{}=D) ->
    D#data_state{data_bag_authz_id = Id}.

%%------------------------------------------------------------------------------
%% GRAB BAG FUNCTIONS AHEAD!!
%%------------------------------------------------------------------------------
%%
%% The following functions require the use of Authz, but in ways that are not currently
%% amenable to our behaviour / mixin based approach.  The most expedient thing at present is
%% to export these functions and call them directly via ?BASE_RESOURCE in the endpoints
%% where they are required.
%%
%% As such, the Open Source implementation of the base resource will need corresponding
%% "no-op" versions.

%% @doc Check the READ authz permissions on a list of cookbooks in parallel.  Checks ALL
%% cookbooks to return a complete error message, since if the user had read permission on
%% all cookbooks, we'd be making all the HTTP requests anyway.
-spec check_cookbook_authz(Cookbooks :: [#chef_cookbook_version{}],
                           Req :: wm_req(),
                           State :: #base_state{}) ->
                                  ok | {error, {[any(),...]}}.
check_cookbook_authz(Cookbooks, Req, State) ->
    ShouldSkip = case application:get_env(oc_chef_wm, custom_acls_depsolver) of
                     {ok, Value} ->
                         Value =:= false;
                     _ -> %% use standard behaviour
                         false
                 end,
    check_cookbook_authz(ShouldSkip, Cookbooks, Req, State).

-spec check_cookbook_authz(SkipCheck :: boolean(),
                           Cookbooks :: [#chef_cookbook_version{}],
                           Req :: wm_req(),
                           State :: #base_state{}) ->
                                  ok | {error, {[any()]}}.
check_cookbook_authz(true, _Cookbooks, _Req, _State) ->
    %% skip cookbook authz check
    ok;
check_cookbook_authz(false, Cookbooks, _Req, #base_state{reqid = ReqId,
                                                        requestor_id = RequestorId}) ->
    Resources = [{object, AuthzId, Name}
                 || #chef_cookbook_version{name = Name, authz_id = AuthzId} <- Cookbooks],
    case ?SH_TIME(ReqId, oc_chef_authz, bulk_actor_is_authorized, (ReqId, RequestorId, Resources, read)) of
        ok -> ok;
        {error, {Name, Why}} ->
            Report = {check_cookbook_authz, {Name, Why, ReqId}},
            error_logger:error_report(Report),
            error(Report);
        NoAuthzList ->
            Names = [ Name || {false, Name} <- NoAuthzList ],
            {error, {[{<<"message">>, <<"Read permission is not granted for one or more cookbooks">>},
                      {<<"unauthorized_cookbooks">>, Names}]}}
    end.

%% This version should work for Open Source:
%% check_cookbook_authz(_Cookbooks, _Req, _State) -> ok.

is_user_in_org(Type, DbContext, Name, OrgName) ->
    case Type of
        <<"client">> ->
            true;
        <<"user">> ->
            case chef_db:is_user_in_org(DbContext, Name, OrgName) of
                true ->
                    true;
                false ->
                    false;
                Error ->
                    throw(Error)
            end
    end.

assemble_principal_ejson(#principal_state{name = Name,
                                          public_key = PublicKey,
                                          type = Type,
                                          authz_id = AuthzId} = _Principal,
                         OrgName, DbContext) ->
    Member = is_user_in_org(Type, DbContext, Name, OrgName),
    {[{<<"name">>, Name},
      {<<"public_key">>, PublicKey},
      {<<"type">>, Type},
      {<<"authz_id">>, AuthzId},
      {<<"org_member">>, Member}]}.

%% These are modules that we instrument with stats_hero and aggregate into common prefix via
%% stats_hero_label.
-type metric_module() :: oc_chef_authz | chef_s3 | chef_sql | chef_solr | chef_otto.

%% @doc Given a `{Mod, Fun}' tuple, generate a stats hero metric with a prefix appropriate
%% for stats_hero aggregation. An error is thrown if `Mod' is unknown. This is where we
%% encode the mapping of module to upstream label.
-spec stats_hero_label({Mod::metric_module(), Fun::atom()}) -> <<_:16,_:_*8>>.
stats_hero_label({chef_sql, Fun}) ->
    chef_metrics:label(rdbms, {chef_sql, Fun});
stats_hero_label({oc_chef_authz, Fun}) ->
    chef_metrics:label(authz, {oc_chef_authz, Fun});
stats_hero_label({chef_solr, Fun}) ->
    chef_metrics:label(solr, {chef_solr, Fun});
stats_hero_label({chef_otto, Fun}) ->
    chef_metrics:label(couchdb, {chef_otto, Fun});
stats_hero_label({chef_s3, Fun}) ->
    chef_metrics:label(s3, {chef_s3, Fun});
stats_hero_label({chef_depsolver, Fun}) ->
    chef_metrics:label(depsolver, {chef_depsolver, Fun});
stats_hero_label({BadPrefix, Fun}) ->
    erlang:error({bad_prefix, {BadPrefix, Fun}}).

%% @doc The prefixes that stats_hero should use for aggregating timing data over each
%% request.
stats_hero_upstreams() ->
    [<<"authz">>, <<"couchdb">>, <<"depsolver">>, <<"rdbms">>, <<"s3">>, <<"solr">>].




object_creation_hook(#chef_client{}=Client,
                     #base_state{chef_authz_context=AuthContext,
                                 organization_guid = OrgId}) ->
    %% NOTE: client create authorization is handled entirely by having CREATE on the clients
    %% container. We use system privilege to add the client to the clients group without
    %% regard for the ACL on the clients group.
    client_cleanup(Client, AuthContext, OrgId, superuser);
object_creation_hook(Object, _State) ->
    %% Everything else passes through unaffected
    Object.

%% @doc Perform needed post-creation cleanup on Client objects.
%% Clients must be added to the clients group, and newly-created
%% validators must have themselves removed from their ACL.
%%
%% See oc_chef_authz:add_client_to_clients_group/4 for more
%% information on the `RequestorId` argument
-spec client_cleanup(#chef_client{},
                     AuthContext :: chef_authz:chef_authz_context(),
                     OrgId :: object_id(),
                     RequestorId :: superuser) -> #chef_client{} |
                                                                {error, term()}.
client_cleanup(#chef_client{authz_id=ClientAuthzId,
                            validator=IsValidator}=Client,
               AuthContext,
               OrgId,
               RequestorId) ->
    case oc_chef_authz:add_client_to_clients_group(AuthContext, OrgId, ClientAuthzId, RequestorId) of
        ok ->
            case IsValidator of
                true ->
                    %% Validators have no permissions on anything; remove it from its own ACL
                    oc_chef_authz:remove_actor_from_actor_acl(ClientAuthzId, ClientAuthzId);
                false ->
                    ok %% No need to remove anything otherwise
            end,

            %% Return the client
            Client;
        {error, Error} ->
            {error, Error}
    end.
