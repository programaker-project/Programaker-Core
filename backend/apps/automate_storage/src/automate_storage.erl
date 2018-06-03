-module(automate_storage).

%% API exports
-export([create_user/3]).
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
                                      , email='$3'},

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
