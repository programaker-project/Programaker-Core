%%% Automate bridge token creation, API tests.
%%% @end

-module(automate_rest_api_user_registration).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").

-define(APPLICATION, automate_rest_api).
-define(PREFIX, "api_reg_test_").

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

    MailTab = ets:new(rest_api_user_registration_tests, [ordered_set, public]),
    application:set_env(automate_mail, mail_gateway, { test_ets, MailTab }, [{persistent, true}]),

    %% Use a custom node name to avoid overwriting the actual databases
    net_kernel:start([testing, shortnames]),

    {ok, _} = application:ensure_all_started(?APPLICATION),

    {Port, MailTab, NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({_Port, MailTab, _NodeName}) ->
    ets:delete(MailTab),
    ok = application:stop(?APPLICATION),

    ok.


tests({Port, MailTab, _}) ->
    %% User registration
    [ { "[User registration] Simple operative", fun() -> simple_registration(Port, MailTab) end }
    , { "[User registration] Handle on verify twice", fun() -> handle_verify_twice(Port, MailTab) end }
    ].


%%====================================================================
%% Tests
%%====================================================================
simple_registration(Port, MailTab) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    Username = binary:replace(<<?PREFIX, Uuid/binary>>, <<"-">>, <<"_">>, [global]),
    Password = <<?PREFIX, "pass">>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@programaker-mail-test.localhost">>,

    RegUri = fmt_list("http://localhost:~p/api/v0/sessions/register", [ Port ]),
    io:fwrite("RegisterUri: ~p~n", [RegUri]),
    RegResult = httpc:request(post
                             , { RegUri
                               , [ ]
                               , "application/json"
                               , jiffy:encode(#{ email => Mail
                                               , username => Username
                                               , password => Password
                                               })
                               }
                             , [], []),

    io:fwrite("RegisterResult: ~p~n", [RegResult]),
    {ok, {RegStatus, _RegHeaders, RegBody}} = RegResult,

    io:fwrite("RegisterBody: ~p~n", [RegBody]),
    ?assertMatch({_, 200, _RegStatusMessage}, RegStatus),

    [{Mail, Username, VerificationCode, _}] = ets:lookup(MailTab, Mail),

    VerifyUri = fmt_list("http://localhost:~p/api/v0/sessions/register/verify", [ Port ]),
    io:fwrite("VerifyUri: ~p~n", [VerifyUri]),
    VerifyResult = httpc:request(post
                                , { VerifyUri
                                  , [ ]
                                  , "application/json"
                                  , jiffy:encode(#{ verification_code => VerificationCode
                                                  })
                                  }
                                , [], []),

    io:fwrite("VerifyResult: ~p~n", [VerifyResult]),
    {ok, {VerifyStatus, _VerifyHeaders, VerifyBody}} = VerifyResult,

    io:fwrite("VerifyBody: ~p~n", [VerifyBody]),
    ?assertMatch({_, 200, _VerifyStatusMessage}, VerifyStatus),

    ok.

handle_verify_twice(Port, MailTab) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    Username = binary:replace(<<?PREFIX, Uuid/binary>>, <<"-">>, <<"_">>, [global]),
    Password = <<?PREFIX, "pass">>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@programaker-mail-test.localhost">>,

    RegUri = fmt_list("http://localhost:~p/api/v0/sessions/register", [ Port ]),
    io:fwrite("RegisterUri: ~p~n", [RegUri]),
    RegResult = httpc:request(post
                             , { RegUri
                               , [ ]
                               , "application/json"
                               , jiffy:encode(#{ email => Mail
                                               , username => Username
                                               , password => Password
                                               })
                               }
                             , [], []),

    io:fwrite("RegisterResult: ~p~n", [RegResult]),
    {ok, {RegStatus, _RegHeaders, RegBody}} = RegResult,

    io:fwrite("RegisterBody: ~p~n", [RegBody]),
    ?assertMatch({_, 200, _RegStatusMessage}, RegStatus),

    [{Mail, Username, VerificationCode, _}] = ets:lookup(MailTab, Mail),

    Verify = fun() ->
                     VerifyUri = fmt_list("http://localhost:~p/api/v0/sessions/register/verify", [ Port ]),
                     io:fwrite("VerifyUri: ~p~n", [VerifyUri]),
                     VerifyResult = httpc:request(post
                                                 , { VerifyUri
                                                   , [ ]
                                                   , "application/json"
                                                   , jiffy:encode(#{ verification_code => VerificationCode
                                                                   })
                                                   }
                                                 , [], []),

                     io:fwrite("VerifyResult: ~p~n", [VerifyResult]),
                     {ok, {VerifyStatus, _VerifyHeaders, VerifyBody}} = VerifyResult,

                     io:fwrite("VerifyBody: ~p~n", [VerifyBody]),
                     {_, 200, _VerifyStatusMessage} = VerifyStatus,
                     #{ <<"success">> := true
                      , <<"session">> := #{ <<"token">> := Token
                                          , <<"user_id">> := _
                                          , <<"username">> := Username
                                          }
                      } = jiffy:decode(VerifyBody, [return_maps]),
                     ?assert(size(Token) > 4)
             end,

    Verify(),

    io:fwrite("Going for the second...~n"),

    Verify(),

    ok.

%%====================================================================
%% Utils
%%====================================================================
fmt_list(FmtStr, Params) ->
    lists:flatten(io_lib:format(FmtStr, Params)).

get_test_port() ->
    12345.
