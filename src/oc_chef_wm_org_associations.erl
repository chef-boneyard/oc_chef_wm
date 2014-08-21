%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@getchef.com>
%% Copyright 2014 Chef Software, Inc. All Rights Reserved.
%%
%% This module handles the following
%%
% GET   /organizations/:orgname/users
%        - list of org users.
% POST  /organizations/:orgname/users -
%        - immediately associate a user to an org. Superuser only.
% GET   /organizations/:orgname/users/:username
%        - get user details for an associated user.
% DELETE /organizations/:orgname/users/:username
%        - disassociate a user from an org. Org Admin.

-module(oc_chef_wm_org_associations).

-include_lib("chef_wm/include/chef_wm.hrl").
-include("oc_chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        post_is_create/2,
                        malformed_request/2,
                        ping/2]}]).

-mixin([{oc_chef_wm_base, [forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).

-export([auth_info/2,
         init/1,
         init_resource_state/1,
         conflict_message/1,
         malformed_request_message/3,
         create_path/2,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         delete_resource/2,
         from_json/2,
         resource_exists/2,
         to_json/2]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #association_state{}}.

request_type() ->
    "associations".

allowed_methods(Req, State) ->
    Allowed = case chef_wm_util:object_name(user, Req) of
        undefined ->
            ['GET', 'POST'];
        _ ->
            ['GET', 'DELETE']
    end,
    {Allowed, Req, State}.

validate_request('POST', Req, #base_state{chef_db_context = DbContext} = State) ->
    case wrq:req_body(Req) of
        undefined ->
          throw({error, missing_body});
    Body ->
        % TODO this will move into oc_chef_org_user_association:parse_binary_object
        EJ = chef_json:decode(Body),
        case ej:valid(oc_chef_org_user_association:org_user_association_spec(), EJ) of
            ok ->
                UserName = ej:get({<<"username">>}, EJ),
                User = chef_db:fetch(#chef_user{username = UserName}, DbContext),
                {Req, State#base_state{resource_state = #association_state{user = User,
                                                                           user_name = UserName,
                                                                           data = EJ}}};
        BadSpec ->
              throw(BadSpec)
        end
    end;
validate_request(_Method, Req, #base_state{chef_db_context = DbContext} = State) ->
    case chef_wm_util:object_name(user, Req) of
        undefined ->
            {Req, State};
        UserName ->
            % GET or DELETE for a named user has to verify that the user exists  and that requestor
            % has appopriate access to that actor - so we'll load up user here to verify it exists
            % and snag authz info.
            User = chef_db:fetch(#chef_user{username = UserName}, DbContext),
            {Req, State#base_state{resource_state = #association_state{user = User,
                                                                       user_name = UserName }}}
    end.

%% If we reach this point and required a user object, no matter the method, if the user
%% doesn't exist we'll bail.
auth_info(Req, #base_state{resource_state = #association_state{user = not_found,
                                                               user_name = UserName}} = State) ->
    Message = chef_wm_util:not_found_message(user, UserName),
    Req1 = chef_wm_util:set_json_body(Req, Message),
    {{halt, 404}, Req1, State#base_state{log_msg = user_not_found}};
auth_info(Req, #base_state{resource_state = #association_state{user = User} } = State) ->
    case wrq:method(Req) of
        'POST' ->
            % Only the superuser can force-create an org-user association
            {superuser_only, Req, State};
        _ ->
            {auth_type_for_user_org(User, undefined_org_authz_id), Req, State}
    end.

auth_type_for_user_org(#chef_user{ authz_id = UserAuthzId }, _OrgAuthzId) ->
    % TODO this will vary a bit for DELETE. Must be self or have update ace to org (and not delete ace
    % which will be default).  Pending org authz checks.
    {actor, UserAuthzId};
auth_type_for_user_org(undefined, _OrgAuthzId) ->
    % Operations occurring at the org level require access to the
    % org object itself
    % TODO - org authz id lookup pending:
    % {actor, OrgAuthzId}
    authorized.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #association_state{user = #chef_user{username = Name}}} = State) ->
    {binary_to_list(Name), Req, State}.

to_json(Req, #base_state{organization_guid = OrgId,
                         chef_db_context = DbContext,
                         resource_state = #association_state{user = undefined} = State} ) ->
    % Because we're not using the standard form of "[a,b,c]"  in our response in order to
    % keep compatible, using chef_wm_base:list_object_json does more than we can use -
    % instead capture the result directly here.
    case chef_db:list(#oc_chef_org_user_association{org_id = OrgId}, DbContext) of
        Names when is_list(Names) ->
            % TODO  cleanup - move to object oc_chef_org_user_association
            EJson = [ {[{ <<"user">>, {[{<<"username">>, Name}]} }]} || Name <- Names ],
            {chef_json:encode(EJson), Req, State};
        Error ->
            {{halt, 500}, Req, State#base_state{log_msg = Error }}
    end;
to_json(Req, #base_state{organization_name = OrgName,
                         chef_db_context = DbContext,
                         resource_state = #association_state{user = #chef_user{username = UserName} = User}} = State ) ->
    % Verify that the user is actually in the org, then send the results back.
    case chef_db:is_user_in_org(DbContext, UserName, OrgName) of
        true ->
            EJson = chef_user:assemble_user_ejson(User, OrgName),
            Json = chef_json:encode(EJson),
            {Json, Req, State};
        false ->
            Message = chef_wm_util:not_found_message(association, UserName),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = user_not_found}}
    end.


from_json(Req, State) ->
    RequestorId = oc_chef_authz:superuser_id(),
    oc_chef_associations:wm_associate_user(Req, State, RequestorId).

delete_resource(Req, #base_state{organization_guid = OrgId,
                                 chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 resource_state = #association_state{user = #chef_user{id = UserId }}} = State ) ->
    % Note that we can avoid an explicit membership check for the user to be deleted,
    % because safe_delete will return not_found if nothing is deleted.
    case oc_chef_object_db:safe_delete(DbContext,
                                       #oc_chef_org_user_association{org_id = OrgId, user_id = UserId},
                                       RequestorId) of
        ok ->
            remove_user(Req, State);
        not_found ->
            {{halt, 404}, Req, State#base_state{log_msg = association_not_found}};
        {error, What} ->
            {{halt, 500}, Req, State#base_state{log_msg = What}}
    end.

remove_user(Req, #base_state{organization_name = OrgName,
                                resource_state = #association_state{ user = #chef_user{username = UserName} = User}  } = State ) ->
    RequestorId = oc_chef_authz:superuser_id(),
    case oc_chef_associations:deprovision_removed_user(State, User, RequestorId) of
        ok ->
            EJ = chef_user:assemble_user_ejson(User, OrgName),
            {true, chef_wm_util:set_json_body(Req, EJ), State#base_state{log_msg = {removed, UserName, from, OrgName}}};
        {warning, Warnings} ->
            lager:error("Warnings in deprovision of ~p from ~p: ~p", [UserName, OrgName, Warnings]),
            EJ = chef_user:assemble_user_ejson(User, OrgName),
            {true, chef_wm_util:set_json_body(Req, EJ), State#base_state{log_msg = {warning_in_deprovision, Warnings}}};
        {error, Error} ->
            lager:error("Error in deprovision of ~p from ~p: ~p", [UserName, OrgName, Error]),
            {{halt, 500}, Req, State#base_state{log_msg = {error_in_deprovision, Error}}}
     end.

conflict_message(_Any) ->
    {[{<<"error">>, [<<"The association already exists">>]}]}.

malformed_request_message(Any, _Req, _state) ->
    error({unexpected_malformed_request_message, Any}).
