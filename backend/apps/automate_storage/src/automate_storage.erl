-module(automate_storage).

%% API exports
-export([ create_user/3
        , login_user/2
        , get_session/1
        ]).
-export([start_link/0]).

%% Structures
-define(REGISTERED_USERS_TABLE, automate_registered_users).
-define(USER_SESSIONS_TABLE, automate_user_sessions).

-record(registered_user_entry, { id
                               , username
                               , password
                               , email
                               }).

-record(user_session_entry, { session_id
                            , user_id
                            , session_start_time
                            }).

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

get_session(SessionId) when is_binary(SessionId) ->
    Transaction = fun() ->
                          mnesia:read(?USER_SESSIONS_TABLE
                                     , SessionId)
                  end,
    {atomic, Result} = mnesia:transaction(Transaction),
    case Result of
        [Session] ->
            { ok, Session };
        [] ->
            { error, session_not_found };
        _ ->
            { error, collision_on_session_id }
    end.

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

get_userid_and_password_from_username(Username) ->
    MatchHead = #registered_user_entry{ id='$1'
                                      , username='$2'
                                      , password='$3'
                                      , email='_'
                                      },
    %% Check that neither the id, username or email matches another
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
