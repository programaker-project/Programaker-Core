-module(automate_storage).

%% API exports
-export([ create_user/3
        , login_user/2
        , get_session_username/1
        , create_program/2
        , get_program/2
        , lists_programs_from_username/1
        , update_program/3
        , get_program_pid/1
        , register_program_runner/2
        , get_program_from_id/1
        , user_has_registered_service/2
        , get_or_gen_registration_token/2
        , get_internal_user_for_telegram_id/1
        , finish_telegram_registration/2
        , dirty_list_running_programs/0
        ]).
-export([start_link/0]).

%% Structures
-define(REGISTERED_USERS_TABLE, automate_registered_users).
-define(USER_SESSIONS_TABLE, automate_user_sessions).
-define(USER_PROGRAMS_TABLE, automate_user_programs).
-define(RUNNING_PROGRAMS_TABLE, automate_running_programs).
-define(EXISTING_SERVICES_TABLE, automate_existing_services).
-define(REGISTERED_SERVICES_TABLE, automate_registered_services).
-define(SERVICE_REGISTRATION_TOKEN_TABLE, automate_service_registration_token_table).
-define(TELEGRAM_SERVICE_REGISTRATION_TABLE, automate_telegram_service_registration_table).

-include("./records.hrl").

-define(DEFAULT_PROGRAM_TYPE, scratch_program).

%%====================================================================
%% API functions
%%====================================================================
create_user(Username, Password, Email) ->
    UserId = generate_id(),
    CipheredPassword = cipher_password(Password),
    RegisteredUserData = #registered_user_entry{ id=UserId
                                               , username=Username
                                               , password=CipheredPassword
                                               , email=Email
                                               },
    case save_unique_user(RegisteredUserData) of
        ok ->
            { ok, UserId };
        {error, Reason} ->
            { error, Reason }
    end.

login_user(Username, Password) ->
    case get_userid_and_password_from_username(Username) of
        {ok, #registered_user_entry{ id=UserId
                                   , password=StoredPassword
                                   }} ->
            case libsodium_crypto_pwhash:str_verify(StoredPassword, Password) =:= 0 of
                true ->
                    SessionToken = generate_id(),
                    ok = add_token_to_user(UserId, SessionToken),
                    { ok, SessionToken };
                _ ->
                    {error, invalid_user_password}
            end;
        {error, no_user_found} ->
            {error, invalid_user_password};

        {error, Reason} ->
            { error, Reason }
    end.

get_session_username(SessionId) when is_binary(SessionId) ->
    Transaction = fun() ->
                          case mnesia:read(?USER_SESSIONS_TABLE, SessionId) of
                              [] ->
                                  { error, session_not_found };
                              [#user_session_entry{ user_id=UserId } | _] ->
                                  case mnesia:read(?REGISTERED_USERS_TABLE, UserId) of
                                      [] ->
                                          %% TODO log event, this shouldn't happen
                                          { error, session_not_found };
                                      [#registered_user_entry{username=Username} | _] ->
                                          {ok, Username}
                                  end
                          end
                  end,

    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

create_program(Username, ProgramName) ->
    {ok, UserId} = get_userid_from_username(Username),
    ProgramId = generate_id(),
    UserProgram = #user_program_entry{ id=ProgramId
                                     , user_id=UserId
                                     , program_name=ProgramName
                                     , program_type=?DEFAULT_PROGRAM_TYPE
                                     , program_parsed=undefined
                                     , program_orig=undefined
                                     },
    case store_new_program(UserProgram) of
        ok ->
            { ok, ProgramId };
        {error, Reason} ->
            { error, Reason }
    end.


get_program(Username, ProgramName) ->
    retrieve_program(Username, ProgramName).


-spec lists_programs_from_username(string()) -> {'ok', [ { string(), string() } ] }.
lists_programs_from_username(Username) ->
    case retrieve_program_list_from_username(Username) of
        {ok, Programs} ->
            { ok
            , [{Id, Name} || [#user_program_entry{id=Id, program_name=Name}] <- Programs]};
        X ->
            X
    end.

-spec update_program(string(), string(), #stored_program_content{}) -> { 'ok', string() } | { 'error', any() }.
update_program(Username, ProgramName, Content)->
    store_new_program_content(Username, ProgramName, Content).

-spec get_program_pid(string()) -> {'ok', pid()} | {error, not_running}.
get_program_pid(ProgramId) ->
    case get_running_program_id(ProgramId) of
        [#running_program_entry{runner_pid=PID}] ->
            {ok, PID};
        [] ->
            {error, not_running};
        {error, Reason} ->
            {error, Reason}
    end.

-spec register_program_runner(string(), pid()) -> 'ok' | {error, not_running}.
register_program_runner(ProgramId, Pid) ->
    Transaction = fun() ->
                          case mnesia:read(?RUNNING_PROGRAMS_TABLE, ProgramId) of
                              [] ->
                                  mnesia:write(?RUNNING_PROGRAMS_TABLE,
                                               #running_program_entry{ program_id=ProgramId
                                                                     , runner_pid=Pid
                                                                     , variables=#{}
                                                                     , stats=#{}
                                                                     }, write);
                              [Program] ->
                                  mnesia:write(?RUNNING_PROGRAMS_TABLE,
                                               Program#running_program_entry{runner_pid=Pid}, write)
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            io:format("Register result: ~p~n", [Result]),
            Result;
        { aborted, Reason } ->
            io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

get_program_from_id(ProgramId) ->
    Transaction = fun() ->
                          mnesia:read(?USER_PROGRAMS_TABLE, ProgramId)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, [Result] } ->
            Result;
        { aborted, Reason } ->
            io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.


dirty_list_running_programs() ->
    {ok, mnesia:dirty_all_keys(?RUNNING_PROGRAMS_TABLE)}.

user_has_registered_service(Username, ServiceId) ->
    case try_get_user_registered_service(Username, ServiceId) of
        {ok, true} ->
            {ok, true};
        {ok, false} ->
            {ok, false};
        {ok, not_found} ->
            {ok, false};
        {error, Reason} ->
            {error, Reason}
    end.


-spec get_or_gen_registration_token(binary(), binary()) -> {ok, binary()}.
get_or_gen_registration_token(Username, ServiceId) ->
    case get_registration_token(Username, ServiceId) of
        {ok, Token} ->
            {ok, Token};
        {error, not_found} ->
            case gen_registration_token(Username, ServiceId) of
                {ok, Token} ->
                    {ok, Token};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_internal_user_for_telegram_id(binary()) -> {ok, binary()} | {error, not_found}.
get_internal_user_for_telegram_id(TelegramId) ->
    Transaction = fun() ->
                          mnesia:read(?TELEGRAM_SERVICE_REGISTRATION_TABLE, TelegramId)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, [#telegram_service_registration_entry{internal_user_id=UserId}] } ->
            {ok, UserId};
        { atomic, [] } ->
            {error, not_found};
        { aborted, Reason } ->
            io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec finish_telegram_registration(binary(), binary()) -> {ok} | {error, not_found}.
finish_telegram_registration(TelegramUserId, RegistrationToken) ->
    finish_telegram_registration_store(RegistrationToken, TelegramUserId).

%% Exposed startup entrypoint
start_link() ->
    Nodes = [node()],
    mnesia:stop(),
    prepare_nodes(Nodes),
    mnesia:start(),
    build_tables(Nodes),
    ignore.

%%====================================================================
%% Internal functions
%%====================================================================
cipher_password(Plaintext) ->
    %% Example 2: password storage
    Password = Plaintext,
    OpslimitSensitive = libsodium_crypto_pwhash:opslimit_sensitive(), % 8
    MemlimitSensitive = libsodium_crypto_pwhash:memlimit_sensitive(), % 536870912
    HashedPassword = libsodium_crypto_pwhash:str(Password, OpslimitSensitive, MemlimitSensitive),
    HashedPassword.

add_token_to_user(UserId, SessionToken) ->
    StartTime = erlang:system_time(second),
    Transaction = fun() ->
                          mnesia:write(?USER_SESSIONS_TABLE
                                      , #user_session_entry{ session_id=SessionToken
                                                           , user_id=UserId
                                                           , session_start_time=StartTime
                                          }
                                      , write)
                  end,
    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

get_userid_from_username(Username) ->
    MatchHead = #registered_user_entry{ id='$1'
                                      , username='$2'
                                      , password='_'
                                      , email='_'
                                      },
    %% Check that neither the id, username or email matches another
    Guard = {'==', '$2', Username},
    ResultColumn = '$1',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          mnesia:select(?REGISTERED_USERS_TABLE, Matcher)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, [Result] } ->
            {ok, Result};
        { atomic, [] } ->
            {error, no_user_found};
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

get_userid_and_password_from_username(Username) ->
    MatchHead = #registered_user_entry{ id='$1'
                                      , username='$2'
                                      , password='$3'
                                      , email='_'
                                      },
    Guard = {'==', '$2', Username},
    ResultColumn = '$1',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          case mnesia:select(?REGISTERED_USERS_TABLE, Matcher) of
                              [UserId] ->
                                  mnesia:read(?REGISTERED_USERS_TABLE, UserId);
                              [] ->
                                  []
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, [Result] } ->
            {ok, Result};
        { atomic, [] } ->
            {error, no_user_found};
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

store_new_program(UserProgram) ->
    Transaction = fun() ->
                          mnesia:write(?USER_PROGRAMS_TABLE
                                      , UserProgram
                                      , write)
                  end,
    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

retrieve_program(Username, ProgramName) ->
    Transaction = fun() ->
                          %% Find userid with that name
                          UserMatchHead = #registered_user_entry{ id='$1'
                                                                , username='$2'
                                                                , password='_'
                                                                , email='_'
                                                                },
                          UserGuard = {'==', '$2', Username},
                          UserResultColumn = '$1',
                          UserMatcher = [{UserMatchHead, [UserGuard], [UserResultColumn]}],

                          case mnesia:select(?REGISTERED_USERS_TABLE, UserMatcher) of
                              [] ->
                                  [];
                              [UserId] ->

                                  %% Find program with userId and name
                                  ProgramMatchHead = #user_program_entry{ id='$1'
                                                                        , user_id='$2'
                                                                        , program_name='$3'
                                                                        , program_type='_'
                                                                        , program_parsed='_'
                                                                        , program_orig='_'
                                                                        },
                                  ProgramGuard = {'andthen'
                                                 , {'==', '$2', UserId}
                                                 , {'==', '$3', ProgramName}},
                                  ProgramResultColumn = '$1',
                                  ProgramMatcher = [{ProgramMatchHead, [ProgramGuard], [ProgramResultColumn]}],

                                  case mnesia:select(?USER_PROGRAMS_TABLE, ProgramMatcher) of
                                      [] ->
                                          [];

                                      [ProgramId] ->
                                          mnesia:read(?USER_PROGRAMS_TABLE, ProgramId)
                                  end
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, [Result] } ->
            {ok, Result};
        { atomic, [] } ->
            {error, not_found};
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

retrieve_program_list_from_username(Username) ->
    Transaction = fun() ->
                          %% Find userid with that name
                          UserMatchHead = #registered_user_entry{ id='$1'
                                                                , username='$2'
                                                                , password='_'
                                                                , email='_'
                                                                },
                          UserGuard = {'==', '$2', Username},
                          UserResultColumn = '$1',
                          UserMatcher = [{UserMatchHead, [UserGuard], [UserResultColumn]}],

                          case mnesia:select(?REGISTERED_USERS_TABLE, UserMatcher) of
                              [] ->
                                  {error, user_not_found};
                              [UserId] ->

                                  %% Find program with userId and name
                                  ProgramMatchHead = #user_program_entry{ id='$1'
                                                                        , user_id='$2'
                                                                        , program_name='$3'
                                                                        , program_type='_'
                                                                        , program_parsed='_'
                                                                        , program_orig='_'
                                                                        },
                                  ProgramGuard = {'==', '$2', UserId},
                                  ProgramResultsColumn = '$1',
                                  ProgramMatcher = [{ProgramMatchHead, [ProgramGuard], [ProgramResultsColumn]}],

                                  Results = mnesia:select(?USER_PROGRAMS_TABLE, ProgramMatcher),
                                  [mnesia:read(?USER_PROGRAMS_TABLE, ResultId) || ResultId <- Results]
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, { error, Reason }} ->
            {error, Reason };
        { atomic, Result } ->
            {ok, Result};
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

-spec store_new_program_content(string(), string(), #stored_program_content{}) -> { 'ok', string() } | { 'error', any() }.
store_new_program_content(Username, ProgramName,
                          #stored_program_content{ orig=ProgramOrig
                                                 , parsed=ProgramParsed
                                                 , type=ProgramType
                                                 })->
        Transaction = fun() ->
                          %% Find userid with that name
                          UserMatchHead = #registered_user_entry{ id='$1'
                                                                , username='$2'
                                                                , password='_'
                                                                , email='_'
                                                                },
                          UserGuard = {'==', '$2', Username},
                          UserResultColumn = '$1',
                          UserMatcher = [{UserMatchHead, [UserGuard], [UserResultColumn]}],

                          case mnesia:select(?REGISTERED_USERS_TABLE, UserMatcher) of
                              [] ->
                                  [];
                              [UserId] ->

                                  %% Find program with userId and name
                                  ProgramMatchHead = #user_program_entry{ id='$1'
                                                                        , user_id='$2'
                                                                        , program_name='$3'
                                                                        , program_type='_'
                                                                        , program_parsed='_'
                                                                        , program_orig='_'
                                                                        },
                                  ProgramGuard = {'andthen'
                                                 , {'==', '$2', UserId}
                                                 , {'==', '$3', ProgramName}},
                                  ProgramResultColumn = '$1',
                                  ProgramMatcher = [{ProgramMatchHead, [ProgramGuard], [ProgramResultColumn]}],

                                  case mnesia:select(?USER_PROGRAMS_TABLE, ProgramMatcher) of
                                      [] ->
                                          [];

                                      [ProgramId] ->
                                          ok = mnesia:write(?USER_PROGRAMS_TABLE,
                                                            #user_program_entry{ id=ProgramId
                                                                               , user_id=UserId
                                                                               , program_name=ProgramName
                                                                               , program_type=ProgramType
                                                                               , program_parsed=ProgramParsed
                                                                               , program_orig=ProgramOrig
                                                                               }, write),
                                          { ok, ProgramId }
                                  end
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, {ok, Result} } ->
            {ok, Result};
        { atomic, [] } ->
            {error, not_found};
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.


save_unique_user(UserData) ->
    #registered_user_entry{ id=UserId
                          , username=Username
                          , email=Email
                          } = UserData,

    MatchHead = #registered_user_entry{ id='$1'
                                      , username='$2'
                                      , password='_'
                                      , email='$3'
                                      },

    %% Check that neither the id, username or email matches another
    GuardId = {'==', '$1', UserId},
    GuardUsername = {'==', '$2', Username},
    GuardEmail = {'==', '$3', Email},
    Guard = {'orelse', GuardId, GuardUsername, GuardEmail},
    ResultColumn = '$1',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          case mnesia:select(?REGISTERED_USERS_TABLE, Matcher) of
                              [] ->
                                  mnesia:write(?REGISTERED_USERS_TABLE, UserData, write);
                              _ ->
                                  {error, colliding_element }
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

get_running_program_id(ProgramId) ->
    Transaction = fun() ->
                          mnesia:read(?RUNNING_PROGRAMS_TABLE, ProgramId)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

try_get_user_registered_service(Username, ServiceId) ->
    MatchHead = #registered_user_entry{ id='$1'
                                      , username='$2'
                                      , password='_'
                                      , email='_'
                                      },

    %% Check that neither the id, username or email matches another
    GuardUsername = {'==', '$2', Username},
    Guard = GuardUsername,
    ResultColumn = '$1',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          case mnesia:select(?REGISTERED_USERS_TABLE, Matcher) of
                              [UserId] ->
                                  ServiceMatchHead = #registered_service_entry{ registration_id='_'
                                                                              , service_id='$1'
                                                                              , user_id='$2'
                                                                              , enabled='$3'
                                                                              },

                                  %% Check that neither the id, username or email matches another
                                  GuardService = {'==', '$1', ServiceId},
                                  GuardUserId = {'==', '$2', UserId},
                                  ServiceGuard = {'andthen', GuardService, GuardUserId},
                                  ServiceResultColumn = '$3',
                                  ServiceMatcher = [{ServiceMatchHead, [ServiceGuard], [ServiceResultColumn]}],

                                  case mnesia:select(?REGISTERED_SERVICES_TABLE, ServiceMatcher) of
                                      [IsEnabled] ->
                                          { ok, IsEnabled };
                                      [] ->
                                          {ok, not_found}
                                  end;
                              [] ->
                                  {ok, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

-spec get_registration_token(binary(), binary()) -> {ok, binary()} | { error, not_found }.
get_registration_token(Username, ServiceId) ->
    MatchHead = #registered_user_entry{ id='$1'
                                      , username='$2'
                                      , password='_'
                                      , email='_'
                                      },

    %% Check that neither the id, username or email matches another
    GuardUsername = {'==', '$2', Username},
    Guard = GuardUsername,
    ResultColumn = '$1',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          case mnesia:select(?REGISTERED_USERS_TABLE, Matcher) of
                              [UserId] ->
                                  TokenMatchHead = #service_registration_token{ token='$1'
                                                                              , service_id='$2'
                                                                              , user_id='$3'
                                                                              },

                                  %% Check that neither the id, username or email matches another
                                  GuardService = {'==', '$2', ServiceId},
                                  GuardUserId = {'==', '$3', UserId},
                                  TokenGuard = {'andthen', GuardService, GuardUserId},
                                  TokenResultColumn = '$1',
                                  TokenMatcher = [{TokenMatchHead, [TokenGuard], [TokenResultColumn]}],

                                  case mnesia:select(?SERVICE_REGISTRATION_TOKEN_TABLE, TokenMatcher) of
                                      [Token] ->
                                          { ok, Token };
                                      [] ->
                                          {error, not_found}
                                  end;
                              [] ->
                                  {ok, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.


-spec gen_registration_token(binary(), binary()) -> {ok, binary()}.
gen_registration_token(Username, ServiceId) ->
    Token = generate_id(),
    MatchHead = #registered_user_entry{ id='$1'
                                      , username='$2'
                                      , password='_'
                                      , email='_'
                                      },

    %% Check that neither the id, username or email matches another
    GuardUsername = {'==', '$2', Username},
    Guard = GuardUsername,
    ResultColumn = '$1',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],
    Transaction = fun() ->
                          case mnesia:select(?REGISTERED_USERS_TABLE, Matcher) of
                              [UserId] ->
                                  TokenRegistration = #service_registration_token{ token=Token
                                                                                 , service_id=ServiceId
                                                                                 , user_id=UserId
                                                                                 },
                                  ok = mnesia:write(?SERVICE_REGISTRATION_TOKEN_TABLE, TokenRegistration, write),
                                  {ok, Token};
                              [] ->
                                  {ok, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

-spec finish_telegram_registration_store(binary(), binary()) -> {ok} | {error, not_found}.
finish_telegram_registration_store(RegistrationToken, TelegramUserId) ->
    Transaction = fun() ->
                          case mnesia:read(?SERVICE_REGISTRATION_TOKEN_TABLE, RegistrationToken) of
                              [#service_registration_token{ service_id=ServiceId
                                                          , user_id=UserId
                                                          }] ->
                                  case automate_bot_engine_telegram:get_platform_id() of
                                      ServiceId ->
                                          ok = mnesia:write(?TELEGRAM_SERVICE_REGISTRATION_TABLE,
                                                            #telegram_service_registration_entry{ telegram_user_id=TelegramUserId
                                                                                                , internal_user_id=UserId
                                                                                                }, write),

                                          ServiceMatchHead = #registered_service_entry{ registration_id='$1'
                                                                                      , service_id='$2'
                                                                                      , user_id='$3'
                                                                                      , enabled='_'
                                                                                      },

                                          %% Retrieve telegram service entry for user
                                          GuardService = {'==', '$2', ServiceId},
                                          GuardUserId = {'==', '$3', UserId},
                                          ServiceGuard = {'andthen', GuardService, GuardUserId},
                                          ServiceResultColumn = '$1',
                                          ServiceMatcher = [{ServiceMatchHead, [ServiceGuard], [ServiceResultColumn]}],

                                          case mnesia:select(?REGISTERED_SERVICES_TABLE, ServiceMatcher) of
                                              [RegisteredServiceId] ->
                                                  [Service] = mnesia:read(?REGISTERED_SERVICES_TABLE, RegisteredServiceId),
                                                  mnesia:write(?REGISTERED_SERVICES_TABLE,
                                                               Service#registered_service_entry{enabled=true},
                                                               write);
                                              [] ->
                                                  mnesia:write(?REGISTERED_SERVICES_TABLE,
                                                               #registered_service_entry{ registration_id=generate_id()
                                                                                        , service_id=ServiceId
                                                                                        , user_id=UserId
                                                                                        , enabled=true
                                                                                        },
                                                               write)
                                          end;
                                      _ ->
                                          %% TODO log appropiately (matched with token from another service)
                                          io:format("[Error] Matched token on another service~n"),
                                          mnesia:abort({error, not_found})
                                  end;
                              _ ->
                                  io:format("[Error] No token match~n"),
                                  mnesia:abort({error, not_found})
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, {error, Reason} } ->
            {error, Reason};
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

%%====================================================================
%% Startup functions
%%====================================================================
prepare_nodes(Nodes) ->
    %% Global structure
    case mnesia:create_schema(Nodes) of
        ok ->
            ok;
        {error, {_, {already_exists, _}}} ->
            ok
    end.

build_tables(Nodes) ->
    %% Registered users table
    ok = case mnesia:create_table(?REGISTERED_USERS_TABLE,
                                  [ {attributes, record_info(fields, registered_user_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, registered_user_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    %% User session table
    ok = case mnesia:create_table(?USER_SESSIONS_TABLE,
                                  [ {attributes, record_info(fields, user_session_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, user_session_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    %% User programs table
    ok = case mnesia:create_table(?USER_PROGRAMS_TABLE,
                                  [ {attributes, record_info(fields, user_program_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, user_program_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    %% Running programs table
    ok = case mnesia:create_table(?RUNNING_PROGRAMS_TABLE,
                                  [ {attributes, record_info(fields, running_program_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, running_program_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    %% Existing services table
    ok = case mnesia:create_table(?EXISTING_SERVICES_TABLE,
                                  [ {attributes, record_info(fields, existing_service_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, existing_service_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    %% Registered services table
    ok = case mnesia:create_table(?REGISTERED_SERVICES_TABLE,
                                  [ {attributes, record_info(fields, registered_service_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, registered_service_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    %% Service registration token table
    ok = case mnesia:create_table(?SERVICE_REGISTRATION_TOKEN_TABLE,
                                  [ {attributes, record_info(fields, service_registration_token)}
                                  , { disc_copies, Nodes }
                                  , { record_name, service_registration_token }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    %% TelegramId -> InternalId matches
    ok = case mnesia:create_table(?TELEGRAM_SERVICE_REGISTRATION_TABLE,
                                  [ { attributes, record_info(fields, telegram_service_registration_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, telegram_service_registration_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    ok.

generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).
