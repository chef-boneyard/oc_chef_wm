%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_wm_containers_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("chef_objects/include/chef_types.hrl").

-record(context, {reqid :: binary(),
                  otto_connection,
                  darklaunch = undefined}).

-compile(export_all).

init_per_suite(Config) ->
    Config2 = setup_helper:start_server(Config),

    %% create the test client
    %% {Pubkey, _PrivKey} = chef_wm_util:generate_keypair("name", "reqid"),
    ClientRecord = chef_object:new_record(chef_client,
                                          <<"00000000000000000000000000000000">>,
                                          <<"00000000000000000000000000000001">>,
                                          {[{<<"name">>, <<"test-client">>},
                                            {<<"validator">>, true},
                                            {<<"admin">>, true},
                                            {<<"public_key">>, <<"stub-pub">>}]}),
    chef_db:create(ClientRecord,
                   #context{reqid = <<"fake-req-id">>},
                   <<"00000000000000000000000000000001">>),
    Config2.

end_per_suite(Config) ->
    Config2 = setup_helper:stop_server(Config),
    Config2.

all() ->
    [works].

works(_) ->
    ok.
