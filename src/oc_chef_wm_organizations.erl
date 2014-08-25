%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_wm_organizations).

-include_lib("chef_wm/include/chef_wm.hrl").
%%-include_lib("oc_chef_authz/include/oc_chef_types.hrl").
-include_lib("oc_chef_wm.hrl").
-include_lib("eunit/include/eunit.hrl").

-mixin([{chef_wm_base, [
                        content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2,
                        post_is_create/2,
                        {list_objects_json/2, to_json}
                       ]}]).

-mixin([{oc_chef_wm_base, [forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

-behaviour(chef_wm).

-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3
        ]).

-export([
         allowed_methods/2,
         conflict_message/1,
         create_path/2,
         from_json/2,
         resource_exists/2
        ]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #organization_state{}}.

request_type() ->
    "organizations".

allowed_methods(Req, State) ->
    {['GET', 'POST'], Req, State}.

validate_request('GET', Req, #base_state{organization_guid = OrgId} = State) ->
    {Req, State#base_state{resource_state = #oc_chef_organization{id = OrgId}}};
validate_request('POST', Req, #base_state{resource_state = OrganizationState}= State) ->
    Body = wrq:req_body(Req),
    {ok, EJson} = oc_chef_organization:parse_binary_json(Body),
    {Req, State#base_state{resource_state = OrganizationState#organization_state{organization_data = EJson}}}.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('GET', Req, State) ->
    {{container, organization}, Req, State};
auth_info('POST', Req, State) ->
    {{create_in_container, object}, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #organization_state{organization_data = OrganizationData}} = State) ->
    Name = ej:get({<<"orgname">>}, OrganizationData),
    {binary_to_list(Name), Req, State}.

% IS this redudnant?
%to_json(Req, #base_state{
%                organization_name = OrgName,
%                resource_state = #organization_state{
%                                    organization_data = Org
%                                   }} = State) ->
%    Ejson = oc_chef_organization:assemble_organization_ejson(Org, OrgName),
%    Json = chef_json:encode(Ejson),
%    {Json, Req, State}.


from_json(Req, #base_state{resource_state = #organization_state{organization_data = OrganizationData,
                                                                organization_authz_id = AuthzId},
                           requestor=User } = State) ->
    Org = chef_wm_base:create_from_json(Req, State, oc_chef_organization, {authz_id, AuthzId}, OrganizationData),
    oc_chef_authz_org_creator:create_org(Org, User).

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Organization already exists">>]}]}.
