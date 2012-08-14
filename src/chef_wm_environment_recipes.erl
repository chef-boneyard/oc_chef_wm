%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @copyright 2012 Opscode, Inc.
%% @doc Resource module for Environment Roles endpoint

-module(chef_wm_environment_recipes).

%% chef_wm behaviour callbacks
-include("chef_wm.hrl").
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0]).
%% Exporting a no-op version of validate_request/3 from chef_wm_base, below

%% Mix in platform-specific Webmachine callback implementations
-mixin([{?BASE_RESOURCE, [forbidden/2,
                          is_authorized/2,
                          service_available/2]}]).

%% Mix in universal Webmachine callback implementations, common to all
%% Chef platforms
-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        validate_request/3,
                        ping/2]}]).

%% Webmachine callbacks implented in this module (i.e., not mixed-in)
-export([allowed_methods/2,
         to_json/2]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #environment_state{}}.

request_type() ->
    "environment_recipes".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

%% TODO: This is the same as in named_environment_resource and
%% environment_cookbooks_resource... consider consolidating
auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_name = OrgName,
                           resource_state = EnvState} = State) ->
    Name = chef_wm_util:object_name(environment, Req),
    case chef_db:fetch_environment(DbContext, OrgName, Name) of
        #chef_environment{authz_id = AuthzId} = Env ->
            {{object, AuthzId}, Req, State#base_state{
                                       resource_state = EnvState#environment_state{
                                                          chef_environment = Env}}};
        not_found ->
            Message = chef_wm_util:not_found_message(environment, Name),
            {{halt, 404},
             chef_wm_util:set_json_body(Req, Message),
             State#base_state{log_msg = env_not_found}}
    end.

%% @doc Generate a JSON array of qualified recipe names, sorted alphabetically.  "Default"
%% recipes are presented as only the cookbook name.
%%
%% Example:
%%
%% [
%%   "bar::recipe",
%%   "baz::recipe",
%%   "foo"             <-- the recipe "foo::default"
%% ]
%%
to_json(Req, #base_state{chef_db_context = DbContext,
                         organization_name = OrgName,
                         resource_state = #environment_state{
                           chef_environment = #chef_environment{
                             name = EnvName
                            }}} = State) ->
    Results = chef_db:fetch_environment_filtered_recipes(DbContext, OrgName, EnvName),
    {ejson:encode(Results), Req, State}.