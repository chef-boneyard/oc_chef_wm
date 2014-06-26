%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@getchef.com>
%% @copyright 2014 Chef Software Inc

%% @doc Resource module for erchef users endpoint

-module(oc_chef_wm_authenticate_user).

-include_lib("chef_wm/include/chef_wm.hrl").
-include("oc_chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2 ]}]).

-mixin([{oc_chef_wm_base, [forbidden/2,
                          is_authorized/2,
                          service_available/2]}]).


-behavior(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         process_post/2]).

init(Config) ->
  chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
  {ok, #user_state{}}.

request_type() ->
  "users".

allowed_methods(Req, State) ->
  {['POST'], Req, State}.

validate_request('POST', Req, #base_state{resource_state = UserState} = State) ->
  case wrq:req_body(Req) of
      Body  when Body == undefined orelse Body == <<>> ->
          throw({error, missing_body});
      Body ->
          UserData = ejson:decode(Body),
          chef_user:validate_user_name(UserData),
          case ej:valid(valid_user_data(), UserData) of
              ok ->
                  UserState1 = UserState#user_state{user_data = UserData},
                  {Req, State#base_state{superuser_bypasses_checks = true, resource_state = UserState1}};
              Bad ->
                  throw(Bad)
          end
  end.

valid_user_data() ->
    {[{<<"password">>, {string_match, chef_regex:regex_for(non_blank_string)}} ]}.

auth_info(Req, #base_state{ resource_state = #user_state{user_data = UserData}} = State) ->
    %% Disallow any attempts to authenticate with credentials of
    %% the platform superuser
    TargetName = chef_user:username_from_ejson(UserData),
    case oc_chef_wm_base:is_superuser(TargetName) of
        true ->
            {{halt, 403}, Req, State};
        _ ->
            %% But only permit the identified superuser
            %% to make this request (eg, only via
            %% webui)
            {superuser_only, Req, State}
    end.

process_post(Req, #base_state{chef_db_context = Ctx,
                              resource_state = #user_state{user_data = UserData}} = State) ->
    Name = chef_user:username_from_ejson(UserData),
    Password = ej:get({<<"password">>}, UserData),
    User = chef_db:fetch(#chef_user{username = Name}, Ctx),

    % Under opscode-account, we checked only for any value here, not for a specific value.
    User1 = case wrq:get_qs_value("local", Req) of
        undefined -> User;
        _ -> User#chef_user{external_authentication_uid = null}
    end,
    case verify_user(Password, User1, Ctx) of
        {false, Code} ->
            {{halt, Code}, chef_wm_util:set_json_body(Req, auth_fail_message(Code)), State};
        EJson ->
            maybe_upgrade_password(Password, User, State),
            {true, chef_wm_util:set_json_body(Req, EJson), State}
    end.

verify_user(Password, #chef_user{external_authentication_uid = null} = User, _Ctx) ->
    PasswordData = chef_user:password_data(User),
    case chef_password:verify(Password, PasswordData) of
        true ->
            user_json(<<"linked">>, User);
        false ->
            {false, 401}
    end;
verify_user(Password, #chef_user{username = UserName, external_authentication_uid = ExtAuthUid} = User, Ctx) ->
    case oc_chef_wm_authn_ldap:authenticate(UserName, Password) of
        {error, connection} ->
            {false, 502};
        {error, unauthorized} ->
            {false, 401};
        AuthUserEJ->
            case chef_db:fetch(#chef_user{external_authentication_uid = ExtAuthUid}, Ctx) of
                Result when is_list(Result) andalso length(Result) > 0 ->
                    user_json(<<"linked">>, User);
                _ ->
                    user_json(<<"unlinked">>, AuthUserEJ)
            end
    end;
verify_user(_Password, _Other, _Ctx) ->
    {false, 401}.

maybe_upgrade_password(Password, User, #base_state{requestor_id = Requestor, chef_db_context = Ctx}) ->
    PasswordData = chef_user:password_data(User),
    case chef_password:upgrade(Password, PasswordData) of
        PasswordData ->
            ok;
        NewPasswordData ->
            User2 = chef_user:set_password_data(User, NewPasswordData),
            chef_db:update(User2, Ctx, Requestor)
    end.

user_json(Status, #chef_user{} = User) ->
    UserEJ0 = chef_user:assemble_user_ejson(User, undefined),
    UserEJ1 = ej:delete({<<"public_key">>}, UserEJ0),
    user_json(Status, UserEJ1);
user_json(Status, UserEJ) ->
    {[{<<"status">>, Status},
      {<<"user">>, UserEJ}]}.

malformed_request_message({error, missing_body}, _Req, _State) ->
    chef_wm_util:error_message_envelope(<<"invalid user authentication request">>);
malformed_request_message(#ej_invalid{}, _Req, _State) ->
    chef_wm_util:error_message_envelope(<<"invalid user authentication request">>).

auth_fail_message(401) ->
    chef_wm_util:error_message_envelope(<<"Failed to authenticate: Username and password incorrect">>);
auth_fail_message(502) ->
    chef_wm_util:error_message_envelope(<<"Authentication server is unavailable.">>).
