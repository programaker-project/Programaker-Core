-module(automate_storage).

%% API exports
-export([ create_user/3
        , login_user/2
        , get_session_username/1
        , get_session_userid/1
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
        , update_program_status/3

        , get_program_owner/1
        , get_program_pid/1
        , register_program_runner/2
        , get_program_from_id/1
        , register_program_tags/2
        , get_tags_program_from_id/1
        , dirty_list_running_programs/0

        , create_thread/2
        , dirty_list_running_threads/0
        , register_thread_runner/2
        , get_thread_from_id/1
        , delete_thread/1
        , update_thread/1
        , get_threads_from_program/1

        , set_program_variable/3
        , get_program_variable/2

        , create_custom_signal/2
        , list_custom_signals_from_user_id/1

        , add_mnesia_node/1
        , register_table/2
        ]).
-export([start_link/0]).
-define(SERVER, ?MODULE).

-include("./databases.hrl").
-include("./records.hrl").
-include("../automate_bot_engine/src/program_records.hrl").

-define(DEFAULT_PROGRAM_TYPE, scratch_program).
-define(WAIT_READY_LOOP_TIME, 1000).
%%====================================================================
%% API functions
%%====================================================================
create_user(Username, Password, Email) ->
    UserId = generate_id(),
    CipheredPassword = case Password of
                           undefined -> undefined;
                           _ -> cipher_password(Password)
                       end,
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
                    { ok, {SessionToken, UserId} };
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

get_session_userid(SessionId) when is_binary(SessionId) ->
    Transaction = fun() ->
                          case mnesia:read(?USER_SESSIONS_TABLE, SessionId) of
                              [] ->
                                  { error, session_not_found };
                              [#user_session_entry{ user_id=UserId } | _] ->
                                  {ok, UserId}
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
                                     , enabled=true
                                     },
    case store_new_program(UserProgram) of
        ok ->
            { ok, ProgramId };
        {error, Reason} ->
            { error, Reason }
    end.


get_program(Username, ProgramName) ->
    retrieve_program(Username, ProgramName).


-spec lists_programs_from_username(binary()) -> {'ok', [ { binary(), binary(), boolean() } ] }.
lists_programs_from_username(Username) ->
    case retrieve_program_list_from_username(Username) of
        {ok, Programs} ->
            { ok
            , [{Id, Name, Enable} || [#user_program_entry{id=Id, program_name=Name, enabled=Enable}] <- Programs]};
        X ->
            X
    end.

list_programs_from_userid(Userid) ->
    case retrieve_program_list_from_userid(Userid) of
        {ok, Programs} ->
            { ok
            , [{Id, Name, Enabled} || [#user_program_entry{id=Id, program_name=Name, enabled=Enabled}] <- Programs]};
        X ->
            X
    end.

-spec update_program_status(binary(), binary(), boolean()) -> 'ok' | { 'error', any() }.
update_program_status(Username, ProgramId, Status)->
    Transaction = fun() ->
                          case mnesia:read(?USER_PROGRAMS_TABLE, ProgramId) of
                              [Program] ->
                                  ok = mnesia:write(?USER_PROGRAMS_TABLE,
                                                    Program#user_program_entry{ enabled=Status
                                                                              }, write)
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            ok;
        { aborted, Reason } ->
            io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
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

-spec get_program_owner(binary()) -> {'ok', binary() | undefined} | {error, not_found}.
get_program_owner(ProgramId) ->
    case get_program_from_id(ProgramId) of
        {ok, #user_program_entry{user_id=UserId}} ->
            {ok, UserId};
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

-spec register_program_tags(binary(), [binary()]) -> 'ok' | {error, not_running}.
register_program_tags(ProgramId, Tags) ->
    Transaction = fun() ->
                          case mnesia:read(?PROGRAM_TAGS_TABLE, ProgramId) of
                              [] ->
                                  ok = mnesia:write(?PROGRAM_TAGS_TABLE,
                                                    #program_tags_entry{ program_id=ProgramId
                                                                       , tags=Tags
                                                                       }, write);
                              [Program] ->
                                  ok = mnesia:write(?PROGRAM_TAGS_TABLE,
                                                    Program#program_tags_entry{ program_id=ProgramId
                                                                              , tags=Tags
                                                                              }, write)
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

get_tags_program_from_id(ProgramId) ->
    Transaction = fun() ->
                          case mnesia:read(?PROGRAM_TAGS_TABLE, ProgramId) of
                              [] ->
                                  {ok, []};
                              [#program_tags_entry{tags=Tags}] ->
                                  {ok, Tags}
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

-spec create_thread(binary(), #program_thread{}) -> {ok, thread_id()}.
create_thread(ParentProgramId, #program_thread{ program=Instructions
                                              , global_memory=Memory
                                              , instruction_memory=InstructionMemory
                                              , position=Position
                                              }) ->
    ThreadId = generate_id(),
    UserThread = #running_program_thread_entry{ thread_id=ThreadId
                                              , runner_pid=undefined
                                              , parent_program_id=ParentProgramId
                                              , instructions=Instructions
                                              , memory=Memory
                                              , instruction_memory=InstructionMemory
                                              , position=Position
                                              , stats=#{}
                                              },

    case store_new_thread(UserThread) of
        ok ->
            { ok, ThreadId };
        {error, Reason} ->
            { error, Reason }
    end.


-spec delete_thread(binary()) -> ok | {error, not_found}.
delete_thread(ThreadId) ->
    Transaction = fun() ->
                          ok = mnesia:delete(?RUNNING_THREADS_TABLE, ThreadId, write)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            io:format("[Thread delete] Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec update_thread(#running_program_thread_entry{}) -> ok | {error, not_found}.
update_thread(Thread=#running_program_thread_entry{ thread_id=Id }) ->
    Transaction = fun() ->
                          case mnesia:read(?RUNNING_THREADS_TABLE, Id) of
                              [] ->
                                  {error, not_found};
                              [_] ->
                                  ok = mnesia:write(?RUNNING_THREADS_TABLE, Thread, write)
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            io:format("[Thread update] Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec get_threads_from_program(binary()) -> {ok, [thread_id()]} | {error, not_found}.
get_threads_from_program(ParentProgramId) ->
    MatchHead = #running_program_thread_entry{ thread_id = '$1'
                                             , runner_pid = '_'
                                             , parent_program_id = '$2'
                                             , instructions = '_'
                                             , memory = '_'
                                             , instruction_memory = '_'
                                             , position = '_'
                                             , stats = '_'
                                             },
    Guard = {'==', '$2', ParentProgramId},
    ResultColumn = '$1',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],
    Transaction = fun() ->
                          mnesia:select(?RUNNING_THREADS_TABLE, Matcher)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            {ok, Result};
        { aborted, Reason } ->
            {error, Reason}
    end.


dirty_list_running_threads() ->
    {ok, mnesia:dirty_all_keys(?RUNNING_THREADS_TABLE)}.


-spec register_thread_runner(binary(), pid()) -> {'ok', #running_program_thread_entry{}} | {error, not_running}.
register_thread_runner(ThreadId, Pid) ->
    Transaction = fun() ->
                          case mnesia:read(?RUNNING_THREADS_TABLE, ThreadId) of
                              [Thread] ->
                                  NewEntry = Thread#running_program_thread_entry{runner_pid=Pid},
                                  ok = mnesia:write(?RUNNING_THREADS_TABLE,
                                                    NewEntry,
                                                    write),
                                  {ok, NewEntry}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            io:fwrite("Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec get_thread_from_id(binary()) -> {ok, #running_program_thread_entry{}} | {error, binary()}.
get_thread_from_id(ThreadId) ->
    Transaction = fun() ->
                          case mnesia:read(?RUNNING_THREADS_TABLE, ThreadId) of
                              [] ->
                                  {error, not_found};
                              [Thread] ->
                                  {ok, Thread}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
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
get_userid_from_username(undefined) ->
    {ok, undefined};

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


%% Custom signals
-spec create_custom_signal(binary(), binary()) -> {ok, binary()}.
create_custom_signal(UserId, SignalName) ->
    {ok, Id} = automate_channel_engine:create_channel(),
    Entry = #custom_signal_entry{ id=Id
                                , name=SignalName
                                , owner=UserId
                                },

    Transaction = fun() ->
                          ok = mnesia:write(?CUSTOM_SIGNALS_TABLE, Entry, write),
                          {ok, Id}
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.


-spec list_custom_signals_from_user_id(binary()) -> {ok, [#custom_signal_entry{}]}.
list_custom_signals_from_user_id(UserId) ->
    Transaction = fun() ->
                          %% Find userid with that name
                          MatchHead = #custom_signal_entry{ id='_'
                                                          , name='_'
                                                          , owner='$1'
                                                          },
                          Guard = {'==', '$1', UserId},
                          ResultColumn = '$_',
                          Matcher = [{MatchHead, [Guard], [ResultColumn]}],

                          {ok, mnesia:select(?CUSTOM_SIGNALS_TABLE, Matcher)}
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

%% Exposed startup entrypoint
start_link() ->
    start_coordinator().

-spec add_mnesia_node(node()) -> ok.
add_mnesia_node(Node) ->
    ok = rpc:call(Node, mnesia, start, []),
    {ok, _} = mnesia:change_config(extra_db_nodes, [Node]),
    ok.

-spec register_table(term(), term()) -> ok.
register_table(_TableName, _RecordDef) ->
    erlang:error(not_implemented).

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

store_new_thread(UserThread) ->
    Transaction = fun() ->
                          ok = mnesia:write(?RUNNING_THREADS_TABLE
                                           , UserThread
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
                                                                        , enabled='_'
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
                                                                        , enabled='_'
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
                                                                , enabled='_'
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
                                                                        , enabled='_'
                                                                        },
                                  ProgramGuard = {'andthen'
                                                 , {'==', '$2', UserId}
                                                 , {'==', '$3', ProgramName}},
                                  ProgramResultColumn = '$_',
                                  ProgramMatcher = [{ProgramMatchHead, [ProgramGuard], [ProgramResultColumn]}],

                                  case mnesia:select(?USER_PROGRAMS_TABLE, ProgramMatcher) of
                                      [] ->
                                          [];

                                      [Program] ->
                                          ok = mnesia:write(?USER_PROGRAMS_TABLE,
                                                            Program#user_program_entry{ user_id=UserId
                                                                                      , program_name=ProgramName
                                                                                      , program_type=ProgramType
                                                                                      , program_parsed=ProgramParsed
                                                                                      , program_orig=ProgramOrig
                                                                                      }, write),
                                          { ok, Program#user_program_entry.id }
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
start_coordinator() ->
    Primary = automate_configuration:get_sync_primary(),
    IsPrimary = automate_configuration:is_node_primary(node()),

    Spawner = self(),
    Coordinator = spawn_link(fun() ->
                                     mnesia:stop(),

                                     register(?SERVER, self()),
                                     SyncPeers = automate_configuration:get_sync_peers(),
                                     NonPrimaries = sets:del_element(Primary, sets:from_list(SyncPeers)),
                                     io:fwrite("Primary: ~p, IP: ~p~n", [Primary, IsPrimary]),
                                     ok = wait_for_all_nodes_ready(IsPrimary, Primary, NonPrimaries),
                                     io:fwrite("[Automate storage] Successfully connected to nodes~n"),
                                     case IsPrimary of
                                         true ->
                                             ok = prepare_nodes(SyncPeers),
                                             ok = mnesia:start(),
                                             NonPrimaryList = sets:to_list(NonPrimaries),
                                             lists:foreach(fun (Node) ->
                                                                   ok = add_mnesia_node(Node)
                                                           end, NonPrimaryList),
                                             mnesia:info(),
                                             io:fwrite("SP: ~p~n", [SyncPeers]),
                                             ok = build_tables(SyncPeers),

                                             lists:foreach(fun (Node) ->
                                                                   {?SERVER, Node} ! {self(), storage_started},
                                                                   io:fwrite("~p ! ~p~n", [ {?SERVER, Node}
                                                                                          , { self(), storage_started}])
                                                           end, NonPrimaryList);
                                         _ ->
                                             ok
                                     end,

                                     Spawner ! {self(), ready},
                                     coordinate_loop(Primary)
                             end),
    receive
        {Coordinator, ready} ->
            io:fwrite("[Automate storage] Ready~n"),
            {ok, Coordinator}
    end.

%% Not a primary node
wait_for_all_nodes_ready(false, Primary, NonPrimaries) ->
    {?SERVER, Primary} ! { self(), {node_ready, node() }},
    io:fwrite("~p ! ~p~n", [{?SERVER, Primary}, { self(), {node_ready, node() }}]),
    receive
        { _From, storage_started } ->
            ok;
        X ->
            io:fwrite("[automate_storage coordinator | ~p | Prim: ~p] Unknown message: ~p~n",
                      [node(), Primary, X]),
            wait_for_all_nodes_ready(false, Primary, NonPrimaries)
    after ?WAIT_READY_LOOP_TIME ->
            wait_for_all_nodes_ready(false, Primary, NonPrimaries)
    end;

wait_for_all_nodes_ready(true, Primary, NonPrimariesToGo) ->
    io:fwrite("Primary waiting messages [To go: ~p]~n", [sets:to_list(NonPrimariesToGo)]),

    case sets:is_empty(NonPrimariesToGo) of
        true ->
            ok;
        false ->
            receive
                Msg = { _From, { node_ready, Node } } ->
                    io:fwrite("[automate_storage coordinator | Prim, ~p] NodeReady: ~p~n",
                              [node(), Msg]),

                    case sets:is_element(Node, NonPrimariesToGo) of
                        true ->
                            ToGo = sets:del_element(Node, NonPrimariesToGo),
                            wait_for_all_nodes_ready(true, Primary, ToGo);
                        _ -> %% Reminded that node is ready... nothing to do
                            wait_for_all_nodes_ready(true, Primary, NonPrimariesToGo)
                    end;
                X ->
                    io:fwrite("[automate_storage coordinator | Prim, ~p] Unknown message: ~p~n",
                              [node(), X]),
                    wait_for_all_nodes_ready(true, Primary, NonPrimariesToGo)
            end
    end.

coordinate_loop(Primary) ->
    receive
        %% To be defined
        X ->
            io:fwrite("[automate_storage coordinator | ~p | Prim: ~p] Unknown message: ~p~n",
                      [node(), Primary, X]),
            coordinate_loop(Primary)
    end.

prepare_nodes(Nodes) ->
    %% Global structure
    io:fwrite("Preparing nodes: ~p~n", [Nodes]),
    case mnesia:create_schema(Nodes) of
        ok ->
            ok;
        {error, {_, {already_exists, _}}} ->
            ok
    end.

build_tables(Nodes) ->
    %% Registered users table
    io:fwrite("Building tables: ~p~n", [Nodes]),
    ok = automate_storage_versioning:apply_versioning(automate_storage_configuration:get_versioning(Nodes),
                                                      Nodes, ?MODULE).

generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).
