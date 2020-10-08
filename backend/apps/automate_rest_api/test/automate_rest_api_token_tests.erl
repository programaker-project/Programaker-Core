%%% Automate bridge token creation, API tests.
%%% @end

-module(automate_rest_api_token_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").

-define(APPLICATION, automate_rest_api).
-define(PREFIX, "automate_rest_api_token_tests_").

%%====================================================================
%% Test API
%%====================================================================

session_manager_test_() ->
    {setup
    , fun setup/0
    , fun stop/1
    , fun tests/1
    }.

%% @doc App infrastructure setup.
%% @end
setup() ->
    NodeName = node(),

    Port = get_test_port(),
    application:set_env(automate_rest_api, port, Port, [{persistent, true}]),

    %% Use a custom node name to avoid overwriting the actual databases
    net_kernel:start([testing, shortnames]),

    {ok, _} = application:ensure_all_started(?APPLICATION),

    {Port, NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({_Port, _NodeName}) ->
    ok = application:stop(?APPLICATION),

    ok.


tests({Port, _}) ->
    %% Token creation
    [ { "[Token API][Creation] Simple token creation", fun() -> simple_token_creation(Port) end }
    , { "[Token API][Creation] Different user cannot create token", fun() -> different_user_cannot_create_token(Port) end }
      %% TODO: , { "[Token API][Creation] Create token on group bridge", fun() -> create_token_on_group_brige(Port) end }
      %% TODO: , { "[Token API][Creation] Different user cannot create token on group bridge", fun() -> different_user_cannot_create_token_on_group_brige(Port) end }
      %% Token listing
      %% TODO: , { "[Token API][Listing] Create token and list", fun() -> create_token_and_list(Port) end }
      %% TODO: , { "[Token API][Listing] Create token and list on group", fun() -> create_token_and_list_on_group(Port) end }
      %% TODO: , { "[Token API][Listing] Create token and list on group without permissions", fun() -> create_token_and_list_on_group_no_permission(Port) end }

      %% Token revokation
      %% TODO: , { "[Token API][Revokation] Simple token creation and revokation", fun() -> simple_token_revokation(Port) end }
      %% TODO: , { "[Token API][Revokation] Different user cannot revoke token", fun() -> different_user_cannot_revoke_token(Port) end }
      %% TODO: , { "[Token API][Revokation] Create token on group bridge and revoke it", fun() -> revoke_token_on_group_brige(Port) end }
      %% TODO: , { "[Token API][Revokation] Different user cannot revoke token on group bridge", fun() -> different_user_cannot_revoke_token_on_group_brige(Port) end }
    ].


%%====================================================================
%% Tests
%%====================================================================
simple_token_creation(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    TokenName = <<"Simple token">>,
    Username = <<?PREFIX, "user-1--", Uuid/binary>>,
    Password = <<?PREFIX, "pass-1">>,
    Mail = <<?PREFIX, "mail-1--",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge-1">>,
    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, BridgeId} = automate_service_port_engine:create_service_port({user, UserId}, BridgeName),
    {ok, {UserToken, UserId} } = automate_storage:login_user(Username, Password),

    Uri = fmt_list("http://localhost:~p/api/v0/bridges/by-id/~s/tokens", [ Port, BridgeId ]),
    io:fwrite("Uri: ~p~n", [Uri]),
    Result = httpc:request(post
                          , { Uri
                            , [ { "Authorization", binary:bin_to_list(UserToken) } ]
                            , "application/json"
                            , jiffy:encode(#{ name => TokenName })
                            }
                          , [], []),

    io:fwrite("Result: ~p~n", [Result]),
    {ok, {Status, _Headers, Body}} = Result,

    io:fwrite("Body: ~p~n", [Body]),

    ?assertMatch({_, 201, _StatusMessage}, Status).


different_user_cannot_create_token(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    Username = <<?PREFIX, "user-2--", Uuid/binary>>,
    Password = <<?PREFIX, "pass-2">>,
    Mail = <<?PREFIX, "mail-2--",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge-2">>,

    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, BridgeId} = automate_service_port_engine:create_service_port({user, UserId}, BridgeName),

    DiffUsername = <<?PREFIX, "diff-user-2--", Uuid/binary>>,
    DiffPassword = <<?PREFIX, "diff-pass-2">>,
    DiffMail = <<?PREFIX, "diff-mail-2--",  Uuid/binary, "@mail.com">>,

    {ok, DiffUserId} = automate_storage:create_user(DiffUsername, DiffPassword, DiffMail, ready),
    {ok, {DiffUserToken, DiffUserId} } = automate_storage:login_user(DiffUsername, DiffPassword),

    TokenName = <<"Simple token">>,
    Uri = fmt_list("http://localhost:~p/api/v0/bridges/by-id/~s/tokens", [ Port, BridgeId ]),
    io:fwrite("Uri: ~p~n", [Uri]),

    Result = httpc:request(post
                          , { Uri
                            , [ { "Authorization", binary:bin_to_list(DiffUserToken) } ]
                            , "application/json"
                            , jiffy:encode(#{ name => TokenName })
                            }
                          , [], []),
    io:fwrite("Result: ~p~n", [Result]),
    {ok, {Status, _Headers, Body}} = Result,

    io:fwrite("Body: ~p~n", [Body]),

    ?assertMatch({_, 401, _StatusMessage}, Status).


%%====================================================================
%% Tests
%%====================================================================
fmt_list(FmtStr, Params) ->
    lists:flatten(io_lib:format(FmtStr, Params)).

get_test_port() ->
    12345.
