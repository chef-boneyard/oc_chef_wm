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
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),

    %% set up and start database

    DbName = "oc_chef_wm_itests",
    DbDataDir = filename:join([PrivDir, "data"]),
    DbLogDir = filename:join([PrivDir, "db.log"]),
    ECSchema = filename:join([DataDir, "deps", "enterprise-chef-server-schema"]),
    OSCSchema = filename:join([ECSchema, "deps", "chef-server-schema"]),

    ct:pal("ECSchema: ~p~n", [ECSchema]),

    CMDS = [
            ["initdb -D", DbDataDir],
            ["pg_ctl -D", DbDataDir, "-l", DbLogDir, "start"],
            ["sleep 1"],
            ["createdb", DbName],
            ["cd", OSCSchema, "&& sqitch --engine pg --db-name", DbName, "deploy"],
            ["cd", ECSchema, "&& sqitch --engine pg --db-name", DbName, "deploy"]
           ],

    error_logger:info_msg("db_start:~n~s~n", [run_cmds(CMDS)]),

    application:set_env(stats_hero, udp_socket_pool_size, 200),
    application:set_env(stats_hero, estatsd_host, "127.0.0.1"),
    application:set_env(stats_hero, estatsd_port, 9466),

    application:set_env(oc_chef_wm, api_version, "11.0.0"),
    application:set_env(oc_chef_wm, server_flavor, "ec"),
    application:set_env(oc_chef_wm, ip, "127.0.0.1"),
    application:set_env(oc_chef_wm, port, 8000),
    application:set_env(oc_chef_wm, reqid_header_name, "X-Request-Id"),
    application:set_env(oc_chef_wm, auth_skew, 900),
    application:set_env(oc_chef_wm, bulk_fetch_batch_size, 5),
    application:set_env(oc_chef_wm, superusers, [<<"pivotal">>]),
    application:set_env(oc_chef_wm, root_metric_key, "chefAPI"),
    application:set_env(oc_chef_wm, authz_timeout, 1000),
    application:set_env(oc_chef_wm, authz_fanout, 20),

    application:set_env(chef_wm, local_key_gen, {true, 1024}),

    application:set_env(sqerl, db_type, pgsql),
    application:set_env(sqerl, db_host, "127.0.0.1"),
    application:set_env(sqerl, db_port, 5432),
    application:set_env(sqerl, db_user, os:getenv("USER")),
    application:set_env(sqerl, db_pass, "pass-ignored"),
    application:set_env(sqerl, db_name, "oc_chef_wm_itests"),
    application:set_env(sqerl, idle_check, 10000),
    application:set_env(sqerl, prepared_statements, {oc_chef_sql, statements, [pgsql]}),
    application:set_env(sqerl, column_transforms,
                        [{<<"created_at">>,
                          {sqerl_transformers, convert_YMDHMS_tuple_to_datetime}},
                         {<<"updated_at">>,
                          {sqerl_transformers, convert_YMDHMS_tuple_to_datetime}}]),

    application:set_env(pooler, pools,
                        [[{name, sqerl},
                          {max_count, 20},
                          {init_count, 20},
                          {start_mfa, {sqerl_client, start_link, []}}],
                         [{name, chef_depsolver},
                          {max_count, 5},
                          {init_count, 5},
                          {start_mfa, {chef_depsolver_worker, start_link, []}}]]),

    application:set_env(webmachine, log_handlers,
                        [{oc_wm_request_logger,
                          [
                           {file, filename:join([PrivDir, "requests.log"])},
                           {file_size, 100},  %% Size in MB
                           {files, 1},
                           {annotations, [req_id, org_name, msg, darklaunch, perf_stats]}
                          ]
                         }]),

    RequiredApps = [
                    sasl,
                    crypto,
                    stats_hero,
                    fast_log,
                    pooler,
                    public_key,
                    ssl,
                    epgsql,
                    sqerl,
                    ibrowse,
                    ejson,
                    inets,
                    mochiweb,
                    webmachine,
                    darklaunch,
                    folsom,
                    chef_objects,
                    rabbit_common,
                    amqp_client,
                    gen_bunny,

                    couchbeam,
                    chef_index,
                    oc_chef_authz,

                    oc_chef_wm
                   ],
    lists:foreach(fun(App) -> 
                          ct:pal("Starting ~p~n", [App]),
                          ok = application:start(App)
                  end, RequiredApps),

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
    Config.

end_per_suite(Config) ->
    PrivDir = ?config(priv_dir, Config),    
    DbDataDir = filename:join([PrivDir, "data"]),
    CMDS = [
            ["pg_ctl -D", DbDataDir, "-m fast", "stop"]
           ],
    error_logger:info_msg("db_stop:~n~s~n", [run_cmds(CMDS)]),
    Config.

all() ->
    [works].

works(_) ->
    timer:sleep(1200000),
    ok.

%% helper funs
run_cmds(CMDS) ->
    [ begin
          CC = space_join(C),
          [CC, "\n", os:cmd(CC), "\n\n"]
      end || C <- CMDS ].

space_join(L) ->
    [ [Elt, " "] || Elt <- L ].
