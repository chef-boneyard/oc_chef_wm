%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_wm_containers_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("chef_objects/include/chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(context, {reqid :: binary(),
                  otto_connection,
                  darklaunch = undefined}).

-compile(export_all).

-define(ORG_ID, <<"00000000000000000000000000000000">>).
-define(AUTHZ_ID, <<"00000000000000000000000000000001">>).
-define(CLIENT_NAME, <<"test-client">>).

init_per_suite(Config) ->
    Config2 = setup_helper:start_server(Config),

    %% create the test client
    %% {Pubkey, _PrivKey} = chef_wm_util:generate_keypair("name", "reqid"),
    ClientRecord = chef_object:new_record(chef_client,
                                          ?ORG_ID,
                                          ?AUTHZ_ID,
                                          {[{<<"name">>, ?CLIENT_NAME},
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
    [list_when_no_containers, create_container, delete_container].

list_when_no_containers(_) ->
    Result = ibrowse:send_req("http://localhost:8000/organizations/org/containers",
           [{"x-ops-userid", "test-client"},
            {"accept", "application/json"}],
                     get),
    ?assertMatch({ok, "200", _, _} , Result),
    ok.

create_container(_) ->
    Result = ibrowse:send_req("http://localhost:8000/organizations/org/containers",
           [{"x-ops-userid", "test-client"},
            {"accept", "application/json"},
            {"content-type", "application/json"}
           ],
                     post,
                             ejson:encode({[{"container", "foo"}]})),
    ?assertMatch({ok, "200", _, _} , Result),
    ok.

delete_container(_) ->
        Result = ibrowse:send_req("http://localhost:8000/organizations/org/containers/foo",
           [{"x-ops-userid", "test-client"},
            {"accept", "application/json"}
           ],
                     delete),
    ?assertMatch({ok, "200", _, _} , Result),
    ok.
    
