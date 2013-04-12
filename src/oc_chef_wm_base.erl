%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(oc_chef_wm_base).

%% Mixin webmachine callbacks from auth module.
-mixin([{oc_chef_wm_auth, [forbidden/2,
                           is_authorized/2,
                           service_available/2,
                           %% Helpers for webmachine callbacks
                           authorized_by_org_membership_check/2]}]).

%% "Grab Bag" functions that will also need to be implemented by other base resources
-export([assemble_principal_ejson/3,
         check_cookbook_authz/3,
         delete_object/3,
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

-spec delete_object(chef_db:db_context(),
                    chef_object() | #chef_cookbook_version{},
                    object_id()) -> ok.
delete_object(DbContext, Object, RequestId) ->
    oc_chef_object_db:delete(DbContext, Object, RequestId).

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
                                  ok | {error, {[any()]}} |
                                  {timeout, Msg :: binary()}.
check_cookbook_authz(Cookbooks, Req, State) ->
    %% How long should we allow for each individual Authz request?
    Timeout = chef_config:config_option(oc_chef_wm, authz_timeout, pos_integer),

    %% How many Authz requests should be in flight at any given time?
    Fanout = chef_config:config_option(oc_chef_wm, authz_fanout, pos_integer),

    %% Return 'ok' if the user has read permission on the cookbook, the `Name' of the
    %% cookbook if not
    CheckFun = fun(#chef_cookbook_version{name = Name, authz_id = AuthzId}) ->
                       case oc_chef_wm_auth:has_permission(object, AuthzId, read, Req, State) of
                           true  -> ok;
                           false -> Name
                       end
               end,

    TimeoutHandler = fun(#chef_cookbook_version{name = Name}) ->
                             {timeout, Name}
                     end,

    %% If the user is authorized for all cookbooks, this is what the result would look like.
    AllOk = lists:duplicate(length(Cookbooks), ok),

    case chef_parallel:parallelize_all_with_timeout(Cookbooks, CheckFun, Fanout, Timeout, TimeoutHandler) of
        AllOk ->
            ok;
        SomeFailed ->
            %% Filter to get just the names, and sort
            Names = lists:sort([N || N <- SomeFailed, is_binary(N)]),
            Timeouts = [N || {timeout, N} <- SomeFailed],

            case Timeouts of
                [] ->
                    %% Just failed due to missing permissions; this is the "normal" error
                    {error, {[{<<"message">>, <<"Read permission is not granted for one or more cookbooks">>},
                              {<<"unauthorized_cookbooks">>, Names}]}};
                _ ->
                    %% We had some timeouts!
                    {timeout, <<"Timeout when checking cookbook permissions">>}
            end
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
-type metric_module() :: oc_chef_authz | chef_s3 | chef_sql | chef_solr | chef_otto | chef_sql_core.

%% @doc Given a `{Mod, Fun}' tuple, generate a stats hero metric with a prefix appropriate
%% for stats_hero aggregation. An error is thrown if `Mod' is unknown. This is where we
%% encode the mapping of module to upstream label.
-spec stats_hero_label({Mod::metric_module(), Fun::atom()}) -> <<_:16,_:_*8>>.
stats_hero_label({chef_sql_core, Fun}) ->
    chef_metrics:label(rdbms, {chef_sql_core, Fun});
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
stats_hero_label({BadPrefix, Fun}) ->
    erlang:error({bad_prefix, {BadPrefix, Fun}}).

%% @doc The prefixes that stats_hero should use for aggregating timing data over each
%% request.
stats_hero_upstreams() ->
    [<<"authz">>, <<"couchdb">>, <<"rdbms">>, <<"s3">>, <<"solr">>].

