
-module(hack_add_org).

-include_lib("chef_wm/include/chef_wm.hrl").
-include("oc_chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2,
                        post_is_create/2]}]).

-mixin([{chef_wm_base, [{list_objects_json/2,
                         to_json}]}]).

-mixin([{?BASE_RESOURCE, [is_authorized/2,
                          service_available/2]}]).

-record(state, {} ).

%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         request_type/0,
         validate_request/3,
         forbidden/2,
         delete_resource/2

        ]).

-export([
         allowed_methods/2,
         create_path/2,
         from_json/2
       ]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    % Whatever.
    {ok, #state{}}.

request_type() ->
    % ok fine
    "organization".

forbidden(Req, State) ->
    {false, Req, State}.

allowed_methods(Req, State) ->
    {['POST', 'DELETE'], Req, State}.


validate_request(_, Req, #base_state{} = State) ->
    {Req, State}.


auth_info(Req, State) ->
    {authorized, Req, State}.

create_path(Req, State) ->
    {"whatever", Req, State}.

% Just slam an org record into the orgs table.
from_json(Req, #base_state{organization_authz_id = OrgAuthzId, organization_name = OrgName, organization_guid = OrgId} = State) ->
    SQL = <<"INSERT INTO orgs (id, authz_id, name, full_name, assigned_at, last_updated_by, created_at, updated_at) "
            "VALUES ($1, $2, $3, $4, CURRENT_TIMESTAMP, $5, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>,
    case sqerl:execute(SQL, [OrgId, OrgAuthzId, OrgName, OrgName, OrgAuthzId]) of
        {ok, _} ->
            { true, chef_wm_util:set_json_body(Req, {[{<<"name">>,<<"wtf">>},{<<"result">>,<<"ok">>}]}), State };
        Other ->
            io:fwrite("FAILED TO ORG ~p~n", [Other]),
            { {halt, 500 }, Req, State}
    end.

% LEt's blindly delete this org from the orgs table.
delete_resource(Req, #base_state{organization_guid = OrgId} = State) ->
    case sqerl:execute(<<"DELETE FROM orgs WHERE id = $1">>, [OrgId]) of
        {ok, _} ->
            { true, chef_wm_util:set_json_body(Req, {[{<<"name">>,<<"wtf">>},{<<"result">>,<<"ok">>}]}), State };
        Other ->
            io:fwrite("FAILED TO ORG ~p~n", [Other]),
            { {halt, 500 }, Req, State}
    end.

