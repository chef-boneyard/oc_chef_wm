%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_authz).

-include("oc_chef_authz/include/oc_chef_authz.hrl").

-export([
         create_entity_if_authorized/4,
         get_container_aid_for_object/3,
         is_authorized_on_resource/6,
         make_context/2
        ]).

create_entity_if_authorized(_Context, _OrgId, _Creator, _ObjectType) ->
    {ok, <<"00000000000000000000000000000000">>}.

get_container_aid_for_object(_Context, _OrgId, _ObjectType) ->
    <<"00000000000000000000000000000000">>.

is_authorized_on_resource(_ReqestorId, _ResourceType, _ResourceId, _ActorType, _ActorId, _AccessMethod) ->
    true.

make_context(_ReqId, _Darklaunch) ->
    {mock, tuple}.
