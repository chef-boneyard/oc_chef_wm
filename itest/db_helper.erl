%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(db_helper).

-include_lib("common_test/include/ct.hrl").

-export([
         start_db/1,
         stop_db/1
        ]).

start_db(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),

    DbName = "oc_chef_wm_itests",
    DbDataDir = filename:join([PrivDir, "data"]),
    DbLogDir = filename:join([PrivDir, "db.log"]),
    ECSchema = filename:join([DataDir, "deps", "enterprise-chef-server-schema"]),
    OSCSchema = filename:join([ECSchema, "deps", "chef-server-schema"]),
    DbPort = 5432,

    CMDS = [
            ["initdb -D", DbDataDir],
            ["pg_ctl -D", DbDataDir, "-l", DbLogDir, "start"],
            ["sleep 1"],
            ["createdb", DbName],
            ["cd", OSCSchema, "&& sqitch --engine pg --db-name", DbName, "deploy"],
            ["cd", ECSchema, "&& sqitch --engine pg --db-name", DbName, "deploy"]
           ],

    error_logger:info_msg("db_start:~n~s~n", [run_cmds(CMDS)]),
    
    [{db_name, DbName},
     {db_port, DbPort},
     {db_data, DbDataDir},
     {db_user, os:getenv("USER")},
     {db_pass, "pass-ignored"}].

stop_db(Config) ->
    DbDataDir = ?config(db_data, Config),
    CMDS = [
            ["pg_ctl -D", DbDataDir, "-m fast", "stop"]
           ],
    error_logger:info_msg("db_stop:~n~s~n", [run_cmds(CMDS)]),
    ok.


%% helper funs
run_cmds(CMDS) ->
    [ begin
          CC = space_join(C),
          [CC, "\n", os:cmd(CC), "\n\n"]
      end || C <- CMDS ].

space_join(L) ->
    [ [Elt, " "] || Elt <- L ].
