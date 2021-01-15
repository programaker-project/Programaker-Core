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
    , { "[Token API][Creation] Create token on group bridge", fun() -> create_token_on_group_bridge(Port) end }
    , { "[Token API][Creation] Different user cannot create token on group bridge", fun() -> different_user_cannot_create_token_on_group_bridge(Port) end }

      %% Token listing
    , { "[Token API][Listing] Create token and list", fun() -> create_token_and_list(Port) end }
    , { "[Token API][Listing] Create token and different user cannot list", fun() -> create_token_and_different_user_cannot_list(Port) end }
    , { "[Token API][Listing] Create token and list on group", fun() -> create_token_and_list_on_group(Port) end }
    , { "[Token API][Listing] Create token and list on group without permissions", fun() -> create_token_and_list_on_group_no_permission(Port) end }

      %% Token revocation
    , { "[Token API][Revocation] Simple token creation and revocation", fun() -> simple_token_revocation(Port) end }
    , { "[Token API][Revocation] Different user cannot revoke token", fun() -> different_user_cannot_revoke_token(Port) end }
    , { "[Token API][Revocation] Create token on group bridge and revoke it", fun() -> revoke_token_on_group_bridge(Port) end }
    , { "[Token API][Revocation] Different user cannot revoke token on group bridge", fun() -> different_user_cannot_revoke_token_on_group_bridge(Port) end }
    ].


%%====================================================================
%% Tests
%%====================================================================
simple_token_creation(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    TokenName = <<"simple-token">>,
    Username = <<?PREFIX, "user", Uuid/binary>>,
    Password = <<?PREFIX, "pass">>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge">>,
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
    Username = <<?PREFIX, "user", Uuid/binary>>,
    Password = <<?PREFIX, "pass">>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge">>,

    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, BridgeId} = automate_service_port_engine:create_service_port({user, UserId}, BridgeName),

    DiffUsername = <<?PREFIX, "diff-user", Uuid/binary>>,
    DiffPassword = <<?PREFIX, "diff-pass">>,
    DiffMail = <<?PREFIX, "diff-mail",  Uuid/binary, "@mail.com">>,

    {ok, DiffUserId} = automate_storage:create_user(DiffUsername, DiffPassword, DiffMail, ready),
    {ok, {DiffUserToken, DiffUserId} } = automate_storage:login_user(DiffUsername, DiffPassword),

    TokenName = <<"simple-token">>,
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


create_token_on_group_bridge(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    TokenName = <<"simple-token">>,
    Username = <<?PREFIX, "user", Uuid/binary>>,
    Password = <<?PREFIX, "pass">>,
    GroupName = <<?PREFIX, "group", Uuid/binary>>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge">>,
    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, {UserToken, UserId} } = automate_storage:login_user(Username, Password),
    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, UserId, false),

    {ok, BridgeId} = automate_service_port_engine:create_service_port({group, GroupId}, BridgeName),

    Uri = fmt_list("http://localhost:~p/api/v0/bridges/by-id/~s/tokens?as_group=~s", [ Port, BridgeId, GroupId ]),
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


different_user_cannot_create_token_on_group_bridge(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    TokenName = <<"simple-token">>,
    Username = <<?PREFIX, "user", Uuid/binary>>,
    Password = <<?PREFIX, "pass">>,
    GroupName = <<?PREFIX, "group", Uuid/binary>>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge">>,
    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, UserId, false),

    {ok, BridgeId} = automate_service_port_engine:create_service_port({group, GroupId}, BridgeName),

    DiffUsername = <<?PREFIX, "diff-user", Uuid/binary>>,
    DiffPassword = <<?PREFIX, "diff-pass">>,
    DiffMail = <<?PREFIX, "diff-mail",  Uuid/binary, "@mail.com">>,

    {ok, DiffUserId} = automate_storage:create_user(DiffUsername, DiffPassword, DiffMail, ready),
    {ok, {DiffUserToken, DiffUserId} } = automate_storage:login_user(DiffUsername, DiffPassword),

    Uri = fmt_list("http://localhost:~p/api/v0/bridges/by-id/~s/tokens?as_group=~s", [ Port, BridgeId, GroupId ]),
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


create_token_and_list(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    TokenName = <<"simple-token">>,
    Username = <<?PREFIX, "user", Uuid/binary>>,
    Password = <<?PREFIX, "pass">>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge">>,
    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, BridgeId} = automate_service_port_engine:create_service_port({user, UserId}, BridgeName),
    {ok, {UserToken, UserId} } = automate_storage:login_user(Username, Password),
    {ok, _TokenKey} = automate_service_port_engine:create_bridge_token(BridgeId, {user, UserId}, TokenName, undefined),

    Uri = fmt_list("http://localhost:~p/api/v0/bridges/by-id/~s/tokens", [ Port, BridgeId ]),
    io:fwrite("Uri: ~p~n", [Uri]),
    Result = httpc:request(get
                          , { Uri
                            , [ { "Authorization", binary:bin_to_list(UserToken) } ]
                            }
                          , [], []),

    io:fwrite("Result: ~p~n", [Result]),
    {ok, {Status, _Headers, Body}} = Result,

    io:fwrite("Body: ~p~n", [Body]),
    ?assertMatch({_, 200, _StatusMessage}, Status),

    Contents = jiffy:decode(Body, [return_maps]),
    ?assertMatch(#{ <<"tokens">> := [#{ <<"name">> := TokenName }] }, Contents).


create_token_and_different_user_cannot_list(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    TokenName = <<"simple-token">>,
    Username = <<?PREFIX, "user", Uuid/binary>>,
    Password = <<?PREFIX, "pass">>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge">>,
    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, BridgeId} = automate_service_port_engine:create_service_port({user, UserId}, BridgeName),
    {ok, _TokenKey} = automate_service_port_engine:create_bridge_token(BridgeId, {user, UserId}, TokenName, undefined),

    DiffUsername = <<?PREFIX, "diff-user", Uuid/binary>>,
    DiffPassword = <<?PREFIX, "diff-pass">>,
    DiffMail = <<?PREFIX, "diff-mail",  Uuid/binary, "@mail.com">>,
    {ok, DiffUserId} = automate_storage:create_user(DiffUsername, DiffPassword, DiffMail, ready),
    {ok, {DiffUserToken, DiffUserId} } = automate_storage:login_user(DiffUsername, DiffPassword),

    Uri = fmt_list("http://localhost:~p/api/v0/bridges/by-id/~s/tokens", [ Port, BridgeId ]),
    io:fwrite("Uri: ~p~n", [Uri]),
    Result = httpc:request(get
                          , { Uri
                            , [ { "Authorization", binary:bin_to_list(DiffUserToken) } ]
                            }
                          , [], []),

    io:fwrite("Result: ~p~n", [Result]),
    {ok, {Status, _Headers, Body}} = Result,

    io:fwrite("Body: ~p~n", [Body]),
    ?assertMatch({_, 401, _StatusMessage}, Status).


create_token_and_list_on_group(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    TokenName = <<"simple-token">>,
    Username = <<?PREFIX, "user", Uuid/binary>>,
    Password = <<?PREFIX, "pass">>,
    GroupName = <<?PREFIX, "group", Uuid/binary>>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge">>,
    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, {UserToken, UserId} } = automate_storage:login_user(Username, Password),
    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, UserId, false),

    {ok, BridgeId} = automate_service_port_engine:create_service_port({group, GroupId}, BridgeName),
    {ok, _TokenKey} = automate_service_port_engine:create_bridge_token(BridgeId, {group, GroupId}, TokenName, undefined),

    Uri = fmt_list("http://localhost:~p/api/v0/bridges/by-id/~s/tokens?as_group=~s", [ Port, BridgeId, GroupId ]),
    io:fwrite("Uri: ~p~n", [Uri]),
    Result = httpc:request(get
                          , { Uri
                            , [ { "Authorization", binary:bin_to_list(UserToken) } ]
                            }
                          , [], []),

    io:fwrite("Result: ~p~n", [Result]),
    {ok, {Status, _Headers, Body}} = Result,

    io:fwrite("Body: ~p~n", [Body]),
    ?assertMatch({_, 200, _StatusMessage}, Status),

    Contents = jiffy:decode(Body, [return_maps]),
    ?assertMatch(#{ <<"tokens">> := [#{ <<"name">> := TokenName }] }, Contents).


create_token_and_list_on_group_no_permission(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    TokenName = <<"simple-token">>,
    Username = <<?PREFIX, "user", Uuid/binary>>,
    Password = <<?PREFIX, "pass">>,
    GroupName = <<?PREFIX, "group", Uuid/binary>>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge">>,
    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, UserId, false),

    {ok, BridgeId} = automate_service_port_engine:create_service_port({group, GroupId}, BridgeName),
    {ok, _TokenKey} = automate_service_port_engine:create_bridge_token(BridgeId, {group, GroupId}, TokenName, undefined),

    DiffUsername = <<?PREFIX, "diff-user", Uuid/binary>>,
    DiffPassword = <<?PREFIX, "diff-pass">>,
    DiffMail = <<?PREFIX, "diff-mail",  Uuid/binary, "@mail.com">>,

    {ok, DiffUserId} = automate_storage:create_user(DiffUsername, DiffPassword, DiffMail, ready),
    {ok, {DiffUserToken, DiffUserId} } = automate_storage:login_user(DiffUsername, DiffPassword),

    Uri = fmt_list("http://localhost:~p/api/v0/bridges/by-id/~s/tokens?as_group=~s", [ Port, BridgeId, GroupId ]),
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


simple_token_revocation(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    TokenName = <<"simple-token">>,
    Username = <<?PREFIX, "user", Uuid/binary>>,
    Password = <<?PREFIX, "pass">>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge">>,
    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, BridgeId} = automate_service_port_engine:create_service_port({user, UserId}, BridgeName),
    {ok, {UserToken, UserId} } = automate_storage:login_user(Username, Password),
    {ok, _TokenKey} = automate_service_port_engine:create_bridge_token(BridgeId, {user, UserId}, TokenName, undefined),

    Uri = fmt_list("http://localhost:~p/api/v0/bridges/by-id/~s/tokens/by-name/~s", [ Port, BridgeId, TokenName ]),
    io:fwrite("Uri: ~p~n", [Uri]),
    Result = httpc:request( delete
                          , { Uri
                            , [ { "Authorization", binary:bin_to_list(UserToken) } ]
                            }
                          , [], []),

    io:fwrite("Result: ~p~n", [Result]),
    {ok, {Status, _Headers, Body}} = Result,

    io:fwrite("Body: ~p~n", [Body]),
    ?assertMatch({_, 200, _StatusMessage}, Status),

    ?assertMatch({ok, []}, automate_service_port_engine:list_bridge_tokens(BridgeId)).


different_user_cannot_revoke_token(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    TokenName = <<"simple-token">>,
    Username = <<?PREFIX, "user", Uuid/binary>>,
    Password = <<?PREFIX, "pass">>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge">>,
    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, BridgeId} = automate_service_port_engine:create_service_port({user, UserId}, BridgeName),
    {ok, _TokenKey} = automate_service_port_engine:create_bridge_token(BridgeId, {user, UserId}, TokenName, undefined),

    DiffUsername = <<?PREFIX, "diff-user", Uuid/binary>>,
    DiffPassword = <<?PREFIX, "diff-pass">>,
    DiffMail = <<?PREFIX, "diff-mail",  Uuid/binary, "@mail.com">>,

    {ok, DiffUserId} = automate_storage:create_user(DiffUsername, DiffPassword, DiffMail, ready),
    {ok, {DiffUserToken, DiffUserId} } = automate_storage:login_user(DiffUsername, DiffPassword),

    Uri = fmt_list("http://localhost:~p/api/v0/bridges/by-id/~s/tokens/by-name/~s", [ Port, BridgeId, TokenName ]),
    io:fwrite("Uri: ~p~n", [Uri]),
    Result = httpc:request( delete
                          , { Uri
                            , [ { "Authorization", binary:bin_to_list(DiffUserToken) } ]
                            }
                          , [], []),

    io:fwrite("Result: ~p~n", [Result]),
    {ok, {Status, _Headers, Body}} = Result,

    io:fwrite("Body: ~p~n", [Body]),
    ?assertMatch({_, 401, _StatusMessage}, Status).


revoke_token_on_group_bridge(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    TokenName = <<"simple-token">>,
    Username = <<?PREFIX, "user", Uuid/binary>>,
    Password = <<?PREFIX, "pass">>,
    GroupName = <<?PREFIX, "group", Uuid/binary>>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge">>,
    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, {UserToken, UserId} } = automate_storage:login_user(Username, Password),

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, UserId, false),

    {ok, BridgeId} = automate_service_port_engine:create_service_port({group, GroupId}, BridgeName),
    {ok, _TokenKey} = automate_service_port_engine:create_bridge_token(BridgeId, {group, GroupId}, TokenName, undefined),

    Uri = fmt_list("http://localhost:~p/api/v0/bridges/by-id/~s/tokens/by-name/~s?as_group=~s", [ Port, BridgeId, TokenName, GroupId ]),
    io:fwrite("Uri: ~p~n", [Uri]),
    Result = httpc:request( delete
                          , { Uri
                            , [ { "Authorization", binary:bin_to_list(UserToken) } ]
                            }
                          , [], []),

    io:fwrite("Result: ~p~n", [Result]),
    {ok, {Status, _Headers, Body}} = Result,

    io:fwrite("Body: ~p~n", [Body]),
    ?assertMatch({_, 200, _StatusMessage}, Status),

    ?assertMatch({ok, []}, automate_service_port_engine:list_bridge_tokens(BridgeId)).


different_user_cannot_revoke_token_on_group_bridge(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    TokenName = <<"simple-token">>,
    Username = <<?PREFIX, "user", Uuid/binary>>,
    Password = <<?PREFIX, "pass">>,
    GroupName = <<?PREFIX, "group", Uuid/binary>>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge">>,
    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, UserId, false),

    {ok, BridgeId} = automate_service_port_engine:create_service_port({group, GroupId}, BridgeName),
    {ok, _TokenKey} = automate_service_port_engine:create_bridge_token(BridgeId, {group, GroupId}, TokenName, undefined),

    DiffUsername = <<?PREFIX, "diff-user", Uuid/binary>>,
    DiffPassword = <<?PREFIX, "diff-pass">>,
    DiffMail = <<?PREFIX, "diff-mail",  Uuid/binary, "@mail.com">>,

    {ok, DiffUserId} = automate_storage:create_user(DiffUsername, DiffPassword, DiffMail, ready),
    {ok, {DiffUserToken, DiffUserId} } = automate_storage:login_user(DiffUsername, DiffPassword),

    Uri = fmt_list("http://localhost:~p/api/v0/bridges/by-id/~s/tokens/by-name/~s?as_group=~s", [ Port, BridgeId, TokenName, GroupId ]),
    io:fwrite("Uri: ~p~n", [Uri]),
    Result = httpc:request( delete
                          , { Uri
                            , [ { "Authorization", binary:bin_to_list(DiffUserToken) } ]
                            }
                          , [], []),

    io:fwrite("Result: ~p~n", [Result]),
    {ok, {Status, _Headers, Body}} = Result,

    io:fwrite("Body: ~p~n", [Body]),
    ?assertMatch({_, 401, _StatusMessage}, Status).


%%====================================================================
%% Utils
%%====================================================================
fmt_list(FmtStr, Params) ->
    lists:flatten(io_lib:format(FmtStr, Params)).

get_test_port() ->
    12345.
