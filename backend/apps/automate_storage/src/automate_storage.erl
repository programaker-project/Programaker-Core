-module(automate_storage).

%% API exports
-export([ create_user/3
        , login_user/2
        , get_session_username/1
        , create_program/2
        , get_program/2
        , lists_programs_from_username/1
        , update_program/3
        ]).
-export([start_link/0]).

%% Structures
-define(REGISTERED_USERS_TABLE, automate_registered_users).
-define(USER_SESSIONS_TABLE, automate_user_sessions).
-define(USER_PROGRAMS_TABLE, automate_user_programs).
-include("./records.hrl").

-define(DEFAULT_PROGRAM_TYPE, scratch_program).

%%====================================================================
%% API functions
%%====================================================================
create_user(Username, Password, Email) ->
    UserId = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
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
                    SessionToken = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
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
    ProgramId = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
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
                                          case mnesia:write(?USER_PROGRAMS_TABLE,
                                                            #user_program_entry{ id=ProgramId
                                                                               , user_id=UserId
                                                                               , program_name=ProgramName
                                                                               , program_type=ProgramType
                                                                               , program_parsed=ProgramParsed
                                                                               , program_orig=ProgramOrig
                                                                               }, write) of
                                              ok ->
                                                  { ok, ProgramId };
                                              X ->
                                                  X
                                          end
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
    ok.

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
