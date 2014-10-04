%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Kevin Smith
%%
%% @copyright 2011-2014 Chef Software, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(oc_chef_wm_app).

-behaviour(application).

%% Application callbacks
-export([remsh_welcome/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    oc_chef_wm_sup:start_link().

stop(_State) ->
    ok.

%% @doc Print an informative message about how to use a remote shell attached to a live
%% oc_erchef node. The idea is to call this from a wrapper script used to start a remote
%% shell, like:
%% `erl -name user1@127.0.0.7 -setcookie erchef -remsh erchef@127.0.0.1 -s oc_chef_wm_app remsh_welcome'.
%%
remsh_welcome() ->
    Msg =
        "~n~n==> Welcome to the oc_erchef remote shell <==~n~n"
        "    TO EXIT: hit ctrl-g followed by q~n"
        "~n"
        "    DO NOT use q() or init:stop(), as these will stop the oc_erchef node~n~n",
    io:format(Msg),
    ok.
