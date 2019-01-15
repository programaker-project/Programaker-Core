-module(automate_storage).

%% API exports
-export([ create_user/3
        , login_user/2
        , get_session_username/1
        , create_monitor/2
        , get_monitor_from_id/1
        , dirty_list_monitors/0
        , lists_monitors_from_username/1
        , get_userid_from_username/1

        , create_program/2
        , get_program/2
        , lists_programs_from_username/1
        , list_programs_from_userid/1
        , update_program/3
        , update_program_metadata/3
        , delete_program/2
        , delete_running_process/1

        , get_program_pid/1
        , register_program_runner/2
        , get_program_from_id/1
        , user_has_registered_service/2
        , dirty_list_running_programs/0

        , set_program_variable/3
        , get_program_variable/2
        ]).
-export([start_link/0]).

%% Structures
-define(REGISTERED_USERS_TABLE, automate_registered_users).
-define(USER_SESSIONS_TABLE, automate_user_sessions).
-define(USER_MONITORS_TABLE, automate_user_monitors).
-define(USER_PROGRAMS_TABLE, automate_user_programs).
-define(RUNNING_PROGRAMS_TABLE, automate_running_programs).
-define(REGISTERED_SERVICES_TABLE, automate_registered_services).
-define(PROGRAM_VARIABLE_TABLE, automate_program_variable_table).

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

-spec create_monitor(binary(), #monitor_entry{}) -> {ok, binary()} | {error, any()}.
create_monitor(Username, MonitorDescriptor=#monitor_entry{ id=none, user_id=none }) ->
    {ok, UserId} = get_userid_from_username(Username),
    MonitorId = generate_id(),
    Monitor = MonitorDescriptor#monitor_entry{ id=MonitorId, user_id=UserId },
    case store_new_monitor(Monitor) of
        ok ->
            { ok, MonitorId };
        {error, Reason} ->
            { error, Reason }
    end.

dirty_list_monitors() ->
    {ok, mnesia:dirty_all_keys(?USER_MONITORS_TABLE)}.


-spec get_monitor_from_id(binary()) -> #monitor_entry{} | {error, any()}.
get_monitor_from_id(MonitorId) ->
    Transaction = fun() ->
                          mnesia:read(?USER_MONITORS_TABLE, MonitorId)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, [Result] } ->
            Result;
        { aborted, Reason } ->
            io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec lists_monitors_from_username(binary()) -> {'ok', [ { binary(), binary() } ] }.
lists_monitors_from_username(Username) ->
    case retrieve_monitors_list_from_username(Username) of
        {ok, Monitors} ->
            { ok
            , [{Id, Name} || [#monitor_entry{id=Id, name=Name}] <- Monitors]};
        X ->
            X
    end.


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


-spec lists_programs_from_username(binary()) -> {'ok', [ { binary(), binary() } ] }.
lists_programs_from_username(Username) ->
    case retrieve_program_list_from_username(Username) of
        {ok, Programs} ->
            { ok
            , [{Id, Name} || [#user_program_entry{id=Id, program_name=Name}] <- Programs]};
        X ->
            X
    end.

list_programs_from_userid(Userid) ->
    case retrieve_program_list_from_userid(Userid) of
        {ok, Programs} ->
            { ok
            , [{Id, Name} || [#user_program_entry{id=Id, program_name=Name}] <- Programs]};
        X ->
            X
    end.

-spec update_program(binary(), binary(), #stored_program_content{}) -> { 'ok', binary() } | { 'error', any() }.
update_program(Username, ProgramName, Content)->
    store_new_program_content(Username, ProgramName, Content).

-spec update_program_metadata(binary(), binary(), #editable_user_program_metadata{}) -> { 'ok', binary() } | { 'error', any() }.
update_program_metadata(Username, ProgramName, #editable_user_program_metadata{program_name=NewProgramName})->
    case retrieve_program(Username, ProgramName) of
        {ok, ProgramEntry=#user_program_entry{id=ProgramId}} ->
            Transaction = fun() ->
                                  ok = mnesia:write(?USER_PROGRAMS_TABLE,
                                                    ProgramEntry#user_program_entry{program_name=NewProgramName}, write),
                                  {ok, ProgramId}
                          end,
            case mnesia:transaction(Transaction) of
                { atomic, Result } ->
                    io:format("Register result: ~p~n", [Result]),
                    Result;
                { aborted, Reason } ->
                    io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
                    {error, mnesia:error_description(Reason)}
            end;
        X ->
            X
    end.

-spec delete_program(binary(), binary()) -> { 'ok', binary() } | { 'error', any() }.
delete_program(Username, ProgramName)->
    case retrieve_program(Username, ProgramName) of
        {ok, ProgramEntry=#user_program_entry{id=ProgramId}} ->
            Transaction = fun() ->
                                  ok = mnesia:delete_object(?USER_PROGRAMS_TABLE,
                                                            ProgramEntry, write)
                          end,
            case mnesia:transaction(Transaction) of
                { atomic, ok } ->
                    {ok, ProgramId};
                { atomic, Result } ->
                    Result;
                { aborted, Reason } ->
                    io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
                    {error, mnesia:error_description(Reason)}
            end;
        X ->
            X
    end.

-spec delete_running_process(binary()) -> ok | {error, not_found}.
delete_running_process(ProcessId) ->
    Transaction = fun() ->
                         ok = mnesia:delete(?RUNNING_PROGRAMS_TABLE, ProcessId, write)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec get_program_pid(binary()) -> {'ok', pid()} | {error, not_running}.
get_program_pid(ProgramId) ->
    case get_running_program_id(ProgramId) of
        [#running_program_entry{runner_pid=PID}] ->
            {ok, PID};
        [] ->
            {error, not_running};
        {error, Reason} ->
            {error, Reason}
    end.

-spec register_program_runner(binary(), pid()) -> 'ok' | {error, not_running}.
register_program_runner(ProgramId, Pid) ->
    Transaction = fun() ->
                          case mnesia:read(?RUNNING_PROGRAMS_TABLE, ProgramId) of
                              [] ->
                                  ok = mnesia:write(?RUNNING_PROGRAMS_TABLE,
                                               #running_program_entry{ program_id=ProgramId
                                                                     , runner_pid=Pid
                                                                     , variables=#{}
                                                                     , stats=#{}
                                                                     }, write);
                              [Program] ->
                                  ok = mnesia:write(?RUNNING_PROGRAMS_TABLE,
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
                          case mnesia:read(?USER_PROGRAMS_TABLE, ProgramId) of
                              [] ->
                                  {error, not_found};
                              [Program] ->
                                  {ok, Program}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
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

-spec get_program_variable(binary(), atom()) -> {ok, any()} | {error, not_found}.
get_program_variable(ProgramId, Key) ->
    Transaction = fun() ->
                          mnesia:read(?PROGRAM_VARIABLE_TABLE, {ProgramId, Key})
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, [#program_variable_table_entry{value=Value}] } ->
            {ok, Value};
        { atomic, [] } ->
            {error, not_found};
        { aborted, Reason } ->
            io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec get_userid_from_username(binary()) -> {ok, binary()} | {error, no_user_found}.
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
    Password = Plaintext,
    Opslimit = libsodium_crypto_pwhash:opslimit_interactive(), % Minimal recommended
    Memlimit = libsodium_crypto_pwhash:memlimit_interactive(), % 64MiB
    HashedPassword = libsodium_crypto_pwhash:str(Password, Opslimit, Memlimit),
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

store_new_monitor(Monitor) ->
    Transaction = fun() ->
                          mnesia:write(?USER_MONITORS_TABLE
                                      , Monitor
                                      , write)
                  end,
    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

retrieve_monitors_list_from_username(Username) ->
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
                                  MonitorMatchHead = #monitor_entry{ id='$1'
                                                                   , user_id='$2'
                                                                   , name='_'
                                                                   , type='_'
                                                                   , value='_'
                                                                   },
                                  MonitorGuard = {'==', '$2', UserId},
                                  MonitorResultsColumn = '$1',
                                  MonitorMatcher = [{MonitorMatchHead, [MonitorGuard], [MonitorResultsColumn]}],

                                  Results = mnesia:select(?USER_MONITORS_TABLE, MonitorMatcher),
                                  [mnesia:read(?USER_MONITORS_TABLE, ResultId) || ResultId <- Results]
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
                                                                        , program_name='_'
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

retrieve_program_list_from_userid(UserId) ->
    Transaction = fun() ->
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
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, { error, Reason }} ->
            {error, Reason };
        { atomic, Result } ->
            {ok, Result};
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

-spec store_new_program_content(binary(), binary(), #stored_program_content{}) -> { 'ok', binary() } | { 'error', any() }.
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

-spec set_program_variable(binary(), atom(), any()) -> ok | {error, any()}.
set_program_variable(ProgramId, Key, Value) ->
    Transaction = fun() ->
                          mnesia:write(?PROGRAM_VARIABLE_TABLE, #program_variable_table_entry{ id={ProgramId, Key}
                                                                                             , value=Value
                                                                                             },
                                       write)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, ok } ->
            ok;
        { aborted, Reason } ->
            io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
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

    %% User monitors table
    ok = case mnesia:create_table(?USER_MONITORS_TABLE,
                                  [ {attributes, record_info(fields, monitor_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, monitor_entry }
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

    %% Program variable table
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

    %% Registered services table
    ok = case mnesia:create_table(?PROGRAM_VARIABLE_TABLE,
                                  [ {attributes, record_info(fields, program_variable_table_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, program_variable_table_entry }
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
