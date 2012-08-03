%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(chef_wm_named_role).

-include("chef_wm.hrl").

-mixin([{?BASE_RESOURCE, [content_types_accepted/2,
                          content_types_provided/2,
                          finish_request/2,
                          forbidden/2,
                          is_authorized/2,
                          malformed_request/2,
                          ping/2,
                          service_available/2]}]).

%% chef_wm behavior callbacks
-export([auth_info/2,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         delete_resource/2,
         from_json/2,
         init/1,
         resource_exists/2,
         to_json/2]).

-behaviour(chef_wm).

init(Config) ->
    ?BASE_RESOURCE:init(?MODULE, Config).

request_type() ->
    "roles".

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

validate_request('GET', Req, State) ->
    {Req, State#base_state{resource_state = #role_state{}}};
validate_request('DELETE', Req, State) ->
    {Req, State#base_state{resource_state = #role_state{}}};
validate_request('PUT', Req, State) ->
    Name = chef_wm_util:object_name(role, Req),
    Body = wrq:req_body(Req),
    {ok, Role} = chef_role:parse_binary_json(Body, {update, Name}),
    {Req, State#base_state{resource_state = #role_state{role_data = Role}}}.

%% Memoize the container id so we don't hammer the database
auth_info(Req, #base_state{chef_db_context = DbContext,
                           resource_state = RoleState,
                           organization_name=OrgName}=State) ->
    RoleName = chef_wm_util:object_name(role, Req),
    case chef_db:fetch_role(DbContext, OrgName, RoleName) of
        not_found ->
            Message = chef_wm_util:not_found_message(role, RoleName),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = role_not_found}};
        #chef_role{authz_id = AuthzId} = Role ->
            RoleState1 = RoleState#role_state{chef_role = Role},
            State1 = State#base_state{resource_state = RoleState1},
            {object, AuthzId, Req, State1}
    end.

%% Org is checked for in malformed_request/2, role is checked for in forbidden/2;
%% if we get this far, it exists.
resource_exists(Req, State) ->
    {true, Req, State}.

to_json(Req, #base_state{resource_state = #role_state{
                           chef_role = #chef_role{
                             serialized_object = JSON}}} = State) ->
    {chef_db_compression:decompress(JSON), Req, State}.

from_json(Req, #base_state{resource_state = #role_state{chef_role = Role,
                                                        role_data = RoleData}} = State) ->
    ?BASE_RESOURCE:update_from_json(Req, State, Role, RoleData).

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 resource_state = #role_state{
                                     chef_role = Role},
                                 requestor = #chef_requestor{
                                     authz_id = RequestorId}} = State) ->

    ok = ?BASE_RESOURCE:delete_object(DbContext, Role, RequestorId),

    Json = chef_db_compression:decompress(Role#chef_role.serialized_object),
    {true, wrq:set_resp_body(Json, Req), State}.

%% Private utility functions
malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
