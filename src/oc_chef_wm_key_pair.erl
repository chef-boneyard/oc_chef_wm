%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% Copyright 2013-2014 Opscode, Inc. All Rights Reserved.

-module(oc_chef_wm_key_pair).

-include_lib("chef_wm/include/chef_wm.hrl").
-include_lib("oc_chef_wm.hrl").
-include_lib("eunit/include/eunit.hrl").

-mixin([{chef_wm_base, [
                        content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        ping/2
                       ]}]).

-mixin([{oc_chef_wm_base, [service_available/2]}]).

-export([
         init/1,

         auth_info/2,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3,

         allowed_methods/2,
         process_post/2,
         to_json/2
        ]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, no_state}.

request_type() ->
    "key_pair".

allowed_methods(Req, State) ->
    {['GET', 'POST'], Req, State}.

validate_request(_, Req, State) ->
    {Req, State}.

auth_info(Req, State) ->
  {authorized, Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

process_post(Req, State) ->
    case chef_keygen_cache:get_key_pair() of
        {PubKey, PrivKey} ->
            KeyPair = {[{<<"public_key">>, PubKey},
                        {<<"private_key">>, PrivKey}]},
            {true, chef_wm_util:set_json_body(Req, KeyPair), State};
        keygen_timeout ->
            {{halt, 503}, Req, State}
    end.

to_json(Req, State) ->
    RawStatus = chef_keygen_cache:status(),
    Status = {[ {to_bin(K), V} || {K, V} <- RawStatus ]},
    JSON = chef_json:encode(Status),
    {JSON, Req, State}.

to_bin(K) when is_atom(K) ->
    erlang:atom_to_binary(K, utf8).
