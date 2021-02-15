%%% @doc
%%% Automate storage username management tests.
%%% @end

-module(automate_storage_username_test).
-include_lib("eunit/include/eunit.hrl").

-define(LIB, automate_storage_utils).
%% Data structures
-include("../../automate_storage/src/records.hrl").

%%====================================================================
%% Test API
%%====================================================================

username_management_test_() ->
    {setup
    , fun setup/0
    , fun stop/1
    , fun tests/1
    }.

%% @doc App infrastructure setup.
%% @end
setup() ->
    ok.

%% @doc App infrastructure teardown.
%% @end
stop(_) ->
    ok.

tests(_SetupResult) ->
    [ {"[Canonicalization] UTF8 lowercasing", fun utf8_lowercasing/0}
    , {"[ValidateUsername] Valid usernames", fun valid_usernames/0}
    , {"[ValidateUsername] InValid usernames", fun invalid_usernames/0}
    ].

utf8_lowercasing() ->
    ?assertEqual(<<"123ñ456ñ">>, ?LIB:canonicalize(<<"123Ñ456ñ">>)).

valid_usernames() ->
    ?assertEqual(true, ?LIB:validate_username(<<"test">>)),
    ?assertEqual(true, ?LIB:validate_username(<<"test_">>)),
    ?assertEqual(true, ?LIB:validate_username(<<"_test">>)),
    ?assertEqual(true, ?LIB:validate_username(<<"_test123">>)),
    ?assertEqual(true, ?LIB:validate_username(<<"test123">>)),
    ?assertEqual(true, ?LIB:validate_username(<<"UPPERCASE">>)),
    ok.

invalid_usernames() ->
    %% 0 characters
    ?assertEqual(false, ?LIB:validate_username(<<"">>)),
    %% 1 characters
    ?assertEqual(false, ?LIB:validate_username(<<"t">>)),
    %% 2 characters
    ?assertEqual(false, ?LIB:validate_username(<<"te">>)),
    %% 51 characters
    ?assertEqual(false, ?LIB:validate_username(<<"a""1234567890""1234567890""1234567890""1234567890""1234567890">>)),
    %% Only numbers with or without dashes
    ?assertEqual(false, ?LIB:validate_username(<<"123456">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"123-456">>)),
    %% Bad types
    ?assertEqual(false, ?LIB:validate_username(undefined)),
    ?assertEqual(false, ?LIB:validate_username(null)),
    %% Invalid characters
    ?assertEqual(false, ?LIB:validate_username(<<"123 456">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"123\n456">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"123?456">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"">>)),
    %% Unicode non-printable marks
    ?assertEqual(false, ?LIB:validate_username(<<"test\xc2\xa0test">>)), % Non printable space

    %% Language names, from https://tour.golang.org/welcome/2
    ?assertEqual(false, ?LIB:validate_username(<<"Português">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Català">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Chinese_中文">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Simplified_chinese_简体">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Traditional_chinese_繁體">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Česky">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Français">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Hebrew_עִבְרִית">>)), % Note Right-To-Left encoding
    ?assertEqual(false, ?LIB:validate_username(<<"Japanese_日本語">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Korean_한국어">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Română">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Русский">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Thai_ภาษาไทย">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Türkçe">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Ukrainian_Українська">>)),
    ?assertEqual(false, ?LIB:validate_username(<<"Uzbek_Ўзбекча">>)),
    ok.
