-module(automate_storage).

%% API exports
-export([ create_user/4
        , login_user/2
        , get_user/1
        , generate_token_for_user/1
        , delete_user/1
        , get_session_username/2
        , get_session_userid/2
        , create_monitor/2
        , get_monitor_from_id/1
        , dirty_list_monitors/0
        , lists_monitors_from_username/1
        , list_monitors/1
        , get_userid_from_username/1
        , update_user_settings/3
        , promote_user_to_admin/1
        , admin_list_users/0
        , set_user_in_preview/2
        , search_users/1

        , create_mail_verification_entry/1
        , verify_registration_with_code/1
        , create_recovery_verification/1
        , get_user_from_mail_address/1
        , check_password_reset_verification_code/1
        , reset_password/2

        , create_program/2
        , create_program/3
        , get_program/2
        , lists_programs_from_username/1
        , list_programs_from_userid/1
        , list_programs/1
        , update_program/3
        , fix_program_channel/1

        , checkpoint_program/3
        , get_last_checkpoint_content/1

        , update_program_by_id/2
        , update_program_metadata/3
        , update_program_metadata/2
        , delete_program/2
        , delete_program/1
        , delete_running_process/1
        , update_program_status/2
        , is_user_allowed/3
        , get_program_pages/1
        , get_program_page/2
        , add_user_asset/3
        , get_user_asset_info/2

        , get_program_owner/1
        , get_program_pid/1
        , get_user_from_pid/1
        , register_program_runner/2
        , get_program_from_id/1
        , register_program_tags/2
        , get_tags_program_from_id/1
        , get_logs_from_program_id/1
        , dirty_list_running_programs/0
        , store_program_event/2
        , get_program_events/1

        , add_user_generated_log/1
        , get_user_generated_logs/1

        , create_thread/2
        , dirty_list_running_threads/0
        , register_thread_runner/2
        , get_thread_from_id/1
        , dirty_is_thread_alive/1
        , delete_thread/1
        , update_thread/1
        , get_threads_from_program/1

        , set_program_variable/3
        , get_program_variable/2
        , set_widget_value/3
        , get_widget_values_in_program/1

        , log_program_error/1
        , mark_successful_call_to_bridge/2
        , mark_failed_call_to_bridge/2

        , create_custom_signal/2
        , list_custom_signals/1

        , create_group/3
        , delete_group/1
        , update_group_metadata/2
        , get_user_groups/1
        , get_group_by_name/2
        , is_allowed_to_read_in_group/2
        , is_allowed_to_write_in_group/2
        , is_allowed_to_admin_in_group/2
        , can_user_admin_as/2
        , can_user_edit_as/2
        , can_user_view_as/2
        , list_collaborators/1
        , add_collaborators/2
        , update_collaborators/2

        , add_mnesia_node/1
        , register_table/2

          %% Utils
        , wrap_transaction/1
        ]).
-export([start_link/0]).
-define(SERVER, ?MODULE).

-include("./security_params.hrl").
-include("./databases.hrl").
-include("./records.hrl").
-include("../../automate_bot_engine/src/program_records.hrl").
-include_lib("./_build/default/lib/eargon2/include/eargon2.hrl").

-define(WAIT_READY_LOOP_TIME, 1000).
-define(DEFAULT_PROGRAM_TYPE, scratch_program).

%%====================================================================
%% API functions
%%====================================================================
create_user(Username, Password, Email, Status) ->
    UserId = generate_id(),
    CurrentTime = erlang:system_time(second),
    CipheredPassword = case Password of
                           undefined -> undefined;
                           _ -> cipher_password(Password)
                       end,

    CanonicalUsername = automate_storage_utils:canonicalize(Username),

    RegisteredUserData = #registered_user_entry{ id=UserId
                                               , username=Username
                                               , canonical_username=CanonicalUsername
                                               , password=CipheredPassword
                                               , email=Email
                                               , registration_time=CurrentTime
                                               , status=Status
                                               , is_admin=false
                                               , is_advanced=false
                                               , is_in_preview=false
                                               },
    case save_unique_user(RegisteredUserData) of
        ok ->
            { ok, UserId };
        {error, Reason} ->
            { error, Reason }
    end.

-spec delete_user(binary()) -> ok.
delete_user(UserId) ->
    Transaction = fun() ->
                          mnesia:delete({?REGISTERED_USERS_TABLE, UserId})
                  end,
    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

login_user(Username, Password) ->
    Result = case binary:match(Username, <<"@">>) of
                 nomatch ->
                     get_user_from_username(Username);
                 _ ->
                     get_user_from_email(Username)
             end,
    case Result of
        {ok, #registered_user_entry{ id=UserId
                                   , password=StoredPassword
                                   , status=Status
                                   }} ->
            case verify_passwd_hash(StoredPassword, Password) of
                ok ->
                    case Status of
                        ready ->
                            SessionToken = generate_id(),
                            ok = add_token_to_user(UserId, SessionToken),
                            { ok, {SessionToken, UserId} };
                        _ ->
                            {error, {user_not_ready, Status}}
                    end;
                {error, ?EARGON2_ERROR_CODE_VERIFY_MISMATCH} ->
                    {error, invalid_user_password};
                {error, ErrorCode} ->
                    automate_logging:log_platform(error, io_lib:format("Error verifying user password (id=~p), error code: ~p", [UserId, ErrorCode])),
                    {error, invalid_user_password}
            end;
        {error, no_user_found} ->
            {error, no_user_found};

        {error, Reason} ->
            { error, Reason }
    end.

get_user(UserId) ->
    Transaction = fun() ->
                          case mnesia:read(?REGISTERED_USERS_TABLE, UserId) of
                              [User] ->
                                  {ok, User};
                              [] ->
                                  {error, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

promote_user_to_admin(UserId) ->
    Transaction = fun() ->
                          case mnesia:read(?REGISTERED_USERS_TABLE, UserId) of
                              [User] ->
                                  ok = mnesia:write(?REGISTERED_USERS_TABLE
                                                   , User#registered_user_entry{ is_admin=true }
                                                   , write);
                              [] ->
                                  {error, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

set_user_in_preview(UserId, InPreview) when is_boolean(InPreview) ->
    Transaction = fun() ->
                          case mnesia:read(?REGISTERED_USERS_TABLE, UserId) of
                              [User] ->
                                  ok = mnesia:write(?REGISTERED_USERS_TABLE
                                                   , User#registered_user_entry{ is_in_preview=InPreview }
                                                   , write);
                              [] ->
                                  {error, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

search_users(Query) ->
    {ok, QueryRe} = re:compile(query_to_re(Query)),
    Transaction = fun() ->
                          {ok, search_users_iter(mnesia:first(?REGISTERED_USERS_TABLE), [], QueryRe)}
                  end,
    mnesia:activity(ets, Transaction).

admin_list_users() ->
    Transaction = fun() ->
                          Result = lists:map(fun(UserId) ->
                                                     [V] = mnesia:read(?REGISTERED_USERS_TABLE, UserId),
                                                     Sessions = get_userid_sessions(UserId),
                                                     Last = case Sessions of
                                                                [] -> undefined;
                                                                _ ->
                                                                    Times = lists:map(fun(#user_session_entry{
                                                                                             session_last_used_time=LastTime }) ->
                                                                                              LastTime
                                                                                      end, Sessions),
                                                                    lists:last(lists:sort(Times))
                                                            end,
                                                     {V, Last}
                                             end,
                                             mnesia:all_keys(?REGISTERED_USERS_TABLE)),
                          { ok, Result }
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

generate_token_for_user(UserId) ->
    case get_user(UserId) of
        {ok, #registered_user_entry{ status=ready }} ->
            SessionToken = generate_id(),
            ok = add_token_to_user(UserId, SessionToken),
            { ok, SessionToken };
        {ok, _} ->
            {error, user_not_ready};
        {error, Reason } ->
            {error, Reason}
    end.

get_session_username(SessionId, RefreshUsedTime) when is_binary(SessionId) ->
    Transaction = fun() ->
                          case mnesia:read(?USER_SESSIONS_TABLE, SessionId) of
                              [] ->
                                  { error, session_not_found };
                              [Session=#user_session_entry{ user_id=UserId } | _] ->
                                  case mnesia:read(?REGISTERED_USERS_TABLE, UserId) of
                                      [] ->
                                          %% TODO log event, this shouldn't happen
                                          { error, session_not_found };
                                      [#registered_user_entry{canonical_username=Username} | _] ->
                                          ok = case RefreshUsedTime of
                                                   true ->
                                                       mnesia:write(
                                                         ?USER_SESSIONS_TABLE
                                                        , Session#user_session_entry{session_last_used_time=erlang:system_time(second)}
                                                        , write);
                                                   false ->
                                                       ok
                                               end,
                                          {ok, Username}
                                  end
                          end
                  end,

    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

get_session_userid(SessionId, RefreshUsedTime) when is_binary(SessionId) ->
    Transaction = fun() ->
                          case mnesia:read(?USER_SESSIONS_TABLE, SessionId) of
                              [] ->
                                  { error, session_not_found };
                              [Session=#user_session_entry{ user_id=UserId } | _] ->
                                  ok = case RefreshUsedTime of
                                           true ->
                                               mnesia:write(
                                                 ?USER_SESSIONS_TABLE
                                                , Session#user_session_entry{session_last_used_time=erlang:system_time(second)}
                                                , write);
                                           false ->
                                               ok
                                       end,
                                  {ok, UserId}
                          end
                  end,

    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

-spec create_monitor(binary(), #monitor_entry{}) -> {ok, binary()} | {error, any()}.
create_monitor(Username, MonitorDescriptor=#monitor_entry{ id=none, owner=none }) ->
    io:fwrite("\033[7m[create_monitor(Username,...)] To be deprecated\033[0m~n"),

    {ok, Owner} = get_userid_from_username(Username),
    MonitorId = generate_id(),
    Monitor = MonitorDescriptor#monitor_entry{ id=MonitorId, owner=Owner },
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
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec lists_monitors_from_username(binary()) -> {'ok', [ { binary(), binary() } ] }.
lists_monitors_from_username(Username) ->
    io:fwrite("\033[7m[lists_monitors_from_username] To be deprecated\033[0m~n"),
    case retrieve_monitors_list_from_username(Username) of
        {ok, Monitors} ->
            { ok
            , [{Id, Name} || [#monitor_entry{id=Id, name=Name}] <- Monitors]};
        X ->
            X
    end.

-spec list_monitors(owner_id()) -> {'ok', [ #monitor_entry{} ] }.
list_monitors(Owner) ->
    Transaction = fun() ->
                          {ok, mnesia:index_read(?USER_MONITORS_TABLE, Owner, #monitor_entry.owner)}
                  end,
    wrap_transaction(mnesia:activity(ets, Transaction)).


-spec create_mail_verification_entry(binary()) -> {ok, binary()} | {error, _}.
create_mail_verification_entry(UserId) ->
    create_verification_entry(UserId, registration_mail_verification).

-spec verify_registration_with_code(binary()) -> {ok, binary()} | {error, _}.
verify_registration_with_code(RegistrationCode) ->
    Transaction = fun() ->
                          case mnesia:read(?USER_VERIFICATION_TABLE, RegistrationCode) of
                              [] ->
                                  {error, not_found};
                              [#user_verification_entry{ user_id=UserId
                                                       , verification_type=registration_mail_verification
                                                       }] ->
                                  case mnesia:read(?REGISTERED_USERS_TABLE, UserId) of
                                      [] ->
                                          {error, user_not_found};
                                      [User=#registered_user_entry{status=mail_not_verified}] ->
                                          ok = mnesia:write(?REGISTERED_USERS_TABLE, User#registered_user_entry{ status=ready }, write),
                                          ok = mnesia:delete({?USER_VERIFICATION_TABLE, RegistrationCode}),
                                          {ok, UserId};
                                      [#registered_user_entry{status=Status}] ->
                                          {error, {status_mismatch, Status}}
                                  end;
                              [#user_verification_entry{ verification_type=VerificationType }] ->
                                  {error, {invalid_verification_type, VerificationType}}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, {error, {invalid_verification_type, VerificationType}} } ->
            io:fwrite("[Storage] Expected type ~p on verification, found: ~p~n",
                      [registration_mail_verification, VerificationType]),
            {error, invalid_verification_type};
        { atomic, {error, {status_mismatch, StatusFound}} } ->
            io:fwrite("[Storage] Status mismatch. Expected ~p, found: ~p~n",
                      [mail_not_verified, StatusFound]),
            {error, status_mismatch};
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, Reason}
    end.

create_recovery_verification(UserId) ->
    create_verification_entry(UserId, password_reset_verification).

get_user_from_mail_address(Email) ->
    MatchHead = #registered_user_entry{ id='_'
                                      , username='_'
                                      , canonical_username='_'
                                      , password='_'
                                      , email='$1'
                                      , status='_'
                                      , registration_time='_'
                                      , is_admin='_'
                                      , is_advanced='_'
                                      , is_in_preview='_'
                                      },
    Guard = {'==', '$1', Email},
    ResultColumn = '$_',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          case mnesia:select(?REGISTERED_USERS_TABLE, Matcher) of
                              [User] ->
                                  {ok, User};
                              [] ->
                                  {error, no_user_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, Reason}
    end.

-spec reset_password(binary(), binary()) -> ok | {error, _}.
reset_password(VerificationCode, Password) ->
    HashedPassword = cipher_password(Password),
    Transaction = fun() ->
                          case mnesia:read(?USER_VERIFICATION_TABLE, VerificationCode) of
                              [] ->
                                  {error, not_found};
                              [#user_verification_entry{ user_id=UserId, verification_type=password_reset_verification }] ->
                                  case mnesia:read(?REGISTERED_USERS_TABLE, UserId) of
                                      [] ->
                                          {error, user_not_found};
                                      [User] ->
                                          ok = mnesia:write(?REGISTERED_USERS_TABLE,
                                                            User#registered_user_entry{ password=HashedPassword },
                                                            write),
                                          ok = mnesia:delete({?USER_VERIFICATION_TABLE, VerificationCode})
                                  end;
                              [#user_verification_entry{ verification_type=OtherVerificationType }] ->
                                  {error, {invalid_verification_type, OtherVerificationType}}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, {error, {invalid_verification_type, OtherVerificationType}} } ->
            io:fwrite("[Storage] Expected type ~p on verification, found: ~p~n",
                      [password_reset_verification, OtherVerificationType]),
            {error, invalid_verification_type};
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, Reason}
    end.

-spec check_password_reset_verification_code(binary()) -> ok | {error, _}.
check_password_reset_verification_code(VerificationCode) ->
    check_verification_code(VerificationCode, password_reset_verification).

create_program(User, ProgramName) ->
    create_program(User, ProgramName, ?DEFAULT_PROGRAM_TYPE).

create_program(Username, ProgramName, ProgramType) when is_binary(Username) ->
    io:fwrite("\033[7m[create_program(Username,...)] To be deprecated\033[0m~n"),
    {ok, Owner} = get_userid_from_username(Username),
    create_program(Owner, ProgramName, ProgramType);

create_program(Owner, ProgramName, ProgramType) ->
    ProgramId = generate_id(),
    {ok, ProgramChannel} = automate_channel_engine:create_channel(),
    CurrentTime = erlang:system_time(second),
    UserProgram = #user_program_entry{ id=ProgramId
                                     , owner=Owner
                                     , program_name=ProgramName
                                     , program_type=ProgramType
                                     , program_parsed=undefined
                                     , program_orig=undefined
                                     , enabled=true
                                     , program_channel=ProgramChannel
                                     , creation_time=CurrentTime
                                     , last_upload_time=0
                                     , last_successful_call_time=0
                                     , last_failed_call_time=0
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
    io:fwrite("\033[7m[lists_programs_from_username] To be deprecated\033[0m~n"),
    case retrieve_program_list_from_username(Username) of
        {ok, Programs} ->
            { ok
            , [{Id, Name, Enable, Type}
               || [#user_program_entry{ id=Id
                                      , program_name=Name
                                      , program_type=Type
                                      , enabled=Enable
                                      }] <- Programs]};
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

-spec list_programs(owner_id()) -> {ok, [#user_program_entry{}, ...]} | {error, any()}.
list_programs(Owner) ->
    Transaction = fun() ->
                          {ok, mnesia:index_read(?USER_PROGRAMS_TABLE, Owner, #user_program_entry.owner)}
                  end,
    wrap_transaction(mnesia:activity(ets, Transaction)).

-spec update_program_status(binary(), boolean()) -> 'ok' | { 'error', any() }.
update_program_status(ProgramId, Status)->
    Transaction = fun() ->
                          case mnesia:read(?USER_PROGRAMS_TABLE, ProgramId) of
                              [Program] ->
                                  ok = mnesia:write(?USER_PROGRAMS_TABLE,
                                                    Program#user_program_entry{ enabled=Status
                                                                              }, write)
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, ok } ->
            ok;
        { aborted, Reason } ->
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec is_user_allowed(owner_id(), binary(), read_program|edit_program|delete_program) -> {ok, boolean()} | {error, any()}.
is_user_allowed(Owner, ProgramId, Action) ->
    Check = case Action of
                read_program -> fun can_user_view_as/2;
                _ -> fun can_user_edit_as/2
            end,
    Transaction = fun() ->
                          case mnesia:read(?USER_PROGRAMS_TABLE, ProgramId) of
                              [#user_program_entry{owner=RealOwner}] ->
                                  {ok, Check(Owner, RealOwner)};
                              [] ->
                                  {error, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, Reason}
    end.

-spec get_program_pages(ProgramId :: binary()) -> {ok, [#program_pages_entry{}]} | {error, not_found}.
get_program_pages(ProgramId) ->
    T = fun() ->
                {ok, mnesia:index_read(?PROGRAM_PAGES_TABLE, ProgramId, #program_pages_entry.program_id)}
        end,
    wrap_transaction(mnesia:ets(T)).

-spec get_program_page(ProgramId :: binary(), Path :: binary()) -> {ok, #program_pages_entry{}} | {error, not_found}.
get_program_page(ProgramId, Path) ->
    T = fun() ->
                case mnesia:read(?PROGRAM_PAGES_TABLE, {ProgramId, Path}) of
                    [ Page ] -> {ok, Page};
                    [] -> {error, not_found}
                end
        end,
    wrap_transaction(mnesia:ets(T)).

-spec add_user_asset(OwnerId :: owner_id(), AssetId :: binary(), MimeType :: mime_type()) -> ok.
add_user_asset(OwnerId, AssetId, MimeType) ->
    T = fun() ->
                mnesia:write(?USER_ASSET_TABLE, #user_asset_entry{ asset_id={ OwnerId, AssetId }
                                                                 , owner_id=OwnerId
                                                                 , mime_type=MimeType
                                                                 }, write)
        end,
    wrap_transaction(mnesia:transaction(T)).

-spec get_user_asset_info(OwnerId :: owner_id(), AssetId :: binary()) -> { ok, #user_asset_entry{} } | {error, not_found}.
get_user_asset_info(OwnerId, AssetId) ->
    T = fun() ->
                case mnesia:read(?USER_ASSET_TABLE, { OwnerId, AssetId }) of
                    [] -> {error, not_found};
                    [Entry] -> {ok, Entry}
                end
        end,
    wrap_transaction(mnesia:ets(T)).


-spec update_program(binary(), binary(), #stored_program_content{}) -> { 'ok', binary() } | { 'error', any() }.
update_program(Username, ProgramName, Content)->
    store_new_program_content(Username, ProgramName, Content).

-spec fix_program_channel(binary()) -> ok | {error, nothing_to_fix | not_found}.
fix_program_channel(ProgramId) ->
    Transaction = fun() ->
                          case mnesia:read(?USER_PROGRAMS_TABLE, ProgramId) of
                              [] -> {error, not_found};
                              [Program=#user_program_entry{program_channel=ChannelId}] ->
                                  case automate_channel_engine_mnesia_backend:exists_channel(ChannelId) of
                                      true ->
                                          {error, nothing_to_fix};
                                      false ->
                                          {ok, NewChannel} = automate_channel_engine:create_channel(),
                                          ok = mnesia:write(?USER_PROGRAMS_TABLE, Program#user_program_entry{program_channel=NewChannel}, write)
                                  end
                          end
                  end,
    wrap_transaction(mnesia:transaction(Transaction)).

-spec update_program_by_id(binary(), #stored_program_content{}) -> { 'ok', binary() } | { 'error', any() }.
update_program_by_id(ProgramId, Content)->
    store_new_program_content(ProgramId, Content).

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
                    io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
                    {error, mnesia:error_description(Reason)}
            end;
        X ->
            X
    end.

-spec update_program_metadata(binary(), #editable_user_program_metadata{}) -> { 'ok', binary() } | { 'error', any() }.
update_program_metadata(ProgramId, #editable_user_program_metadata{program_name=NewProgramName})->
    Transaction = fun() ->
                          case get_program_from_id(ProgramId) of
                              {ok, ProgramEntry=#user_program_entry{id=ProgramId}} ->
                                  ok = mnesia:write(?USER_PROGRAMS_TABLE,
                                                    ProgramEntry#user_program_entry{program_name=NewProgramName}, write),
                                  {ok, ProgramId};
                              X ->
                                  X
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            io:format("Register result: ~p~n", [Result]),
            Result;
        { aborted, Reason } ->
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec checkpoint_program(binary(), binary(), any()) -> 'ok' | { 'error', any() }.
checkpoint_program(UserId, ProgramId, Content)->
    CurrentTime = erlang:system_time(millisecond),
    Transaction = fun() ->
                          ok = mnesia:write(?USER_PROGRAM_CHECKPOINTS_TABLE,
                                            #user_program_checkpoint{ program_id=ProgramId
                                                                    , user_id=UserId
                                                                    , event_time=CurrentTime
                                                                    , content=Content
                                                                    }, write),
                          ok = mnesia:delete(?USER_PROGRAM_EVENTS_TABLE, ProgramId, write)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),

            {error, mnesia:error_description(Reason)}
    end.

-spec get_last_checkpoint_content(binary()) -> {ok, #user_program_checkpoint{}} | {error, any()}.
get_last_checkpoint_content(ProgramId) ->
    Checkpoints = mnesia:dirty_read(?USER_PROGRAM_CHECKPOINTS_TABLE, ProgramId),
    Sorted = lists:sort(fun( #user_program_checkpoint{event_time=EventTime1}
                           , #user_program_checkpoint{event_time=EventTime2}
                           ) ->
                                EventTime1 > EventTime2
                        end,
                        Checkpoints),
    case Sorted of
        [Checkpoint | _] ->
            {ok, Checkpoint};
        [] ->
            {error, not_found}
    end.

-spec delete_program(binary(), binary()) -> { 'ok', binary() } | { 'error', any() }.
delete_program(Username, ProgramName)->
    case retrieve_program(Username, ProgramName) of
        {ok, ProgramEntry=#user_program_entry{ id=ProgramId
                                             , program_channel=Channel
                                             }} ->
            ok = automate_channel_engine:delete_channel(Channel),
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
                    io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
                    {error, mnesia:error_description(Reason)}
            end;
        X ->
            X
    end.

-spec delete_program(binary()) -> ok | { 'error', any() }.
delete_program(ProgramId)->
    Transaction = fun() ->
                          case get_program_from_id(ProgramId) of
                              {ok, ProgramEntry=#user_program_entry{ id=ProgramId
                                                                   , program_channel=Channel
                                                                   }} ->
                                  ok = automate_channel_engine:delete_channel(Channel),
                                  ok = mnesia:delete_object(?USER_PROGRAMS_TABLE,
                                                            ProgramEntry, write);

                              X ->
                                  X
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
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
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
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


-spec get_user_from_pid(pid()) -> { ok, owner_id() } | {error, not_found}.
get_user_from_pid(Pid) ->
    %% Look for it as a program (not running thread)
    ProgMatchHead = #running_program_entry{ program_id = '$1'
                                          , runner_pid = '$2'
                                          , variables = '_'
                                          , stats = '_'
                                          },
    ProgGuard = {'==', '$2', Pid},
    ProgResultColumn = '$1',
    ProgMatcher = [{ProgMatchHead, [ProgGuard], [ProgResultColumn]}],

    %% Then look for it as a thread
    ThreadMatchHead = #running_program_thread_entry{ thread_id = '_'
                                                   , runner_pid = '$2'
                                                   , parent_program_id = '$1'
                                                   , instructions = '_'
                                                   , memory = '_'
                                                   , instruction_memory = '_'
                                                   , position = '_'
                                                   , stats = '_'
                                                   },
    ThreadGuard = {'==', '$2', Pid},
    ThreadResultColumn = '$1',
    ThreadMatcher = [{ThreadMatchHead, [ThreadGuard], [ThreadResultColumn]}],
    Transaction = fun() ->
                          case mnesia:select(?RUNNING_PROGRAMS_TABLE, ProgMatcher) of
                              [ProgramId] ->
                                  [#user_program_entry{ owner=Owner }] = mnesia:read(?USER_PROGRAMS_TABLE, ProgramId),
                                  { ok, Owner };
                              [] ->
                                  case mnesia:select(?RUNNING_THREADS_TABLE, ThreadMatcher) of
                                      [ ParentProgramId ] ->
                                          [#user_program_entry{ owner=Owner }] = mnesia:read(?USER_PROGRAMS_TABLE, ParentProgramId),
                                          { ok, Owner };
                                      [] ->
                                          {error, not_found}
                                  end
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, Reason}
    end.

-spec get_program_owner(binary()) -> {'ok', owner_id() | undefined} | {error, not_found}.
get_program_owner(ProgramId) ->
    case get_program_from_id(ProgramId) of
        {ok, #user_program_entry{owner=Owner}} ->
            {ok, Owner};
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
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec get_program_from_id(binary()) -> {ok, #user_program_entry{}} | {error, not_found}.
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
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
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
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
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
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec get_logs_from_program_id(binary()) -> {ok, [#user_program_log_entry{}]} | {error, atom()}.
get_logs_from_program_id(ProgramId) ->
    Transaction = fun() ->
                          {ok, mnesia:read(?USER_PROGRAM_LOGS_TABLE, ProgramId)}
                  end,
    wrap_transaction(mnesia:activity(ets, Transaction)).

dirty_list_running_programs() ->
    {ok, mnesia:dirty_all_keys(?RUNNING_PROGRAMS_TABLE)}.


-spec store_program_event(binary(), any()) -> ok | {error, any()}.
store_program_event(ProgramId, Event) ->
    Time = erlang:monotonic_time(),
    UMI = erlang:unique_integer([monotonic]),
    EventTag = {Time, UMI},

    T = fun() ->
                mnesia:write(?USER_PROGRAM_EVENTS_TABLE, #user_program_editor_event{ program_id=ProgramId, event=Event, event_tag=EventTag }, write)
        end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec get_program_events(binary()) -> {ok, [#user_program_editor_event{}]} | {error, any()}.
get_program_events(ProgramId) ->
    T = fun() ->
                mnesia:read(?USER_PROGRAM_EVENTS_TABLE, ProgramId)
        end,
    case mnesia:transaction(T) of
        {atomic, Results} ->
            {ok, Results};
        {aborted, Reason} ->
            {error, Reason}
    end.

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
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec dirty_is_thread_alive(binary()) -> {ok, boolean()}.
dirty_is_thread_alive(ThreadId) ->
    case mnesia:dirty_read(?RUNNING_THREADS_TABLE, ThreadId) of
        [] ->
            {ok, false};
        [_Thread] ->
            {ok, true}
    end.

-spec get_program_variable(binary(), binary()) -> {ok, any()} | {error, not_found}.
get_program_variable(ProgramId, Key) ->
    Transaction = fun() ->
                          mnesia:read(?PROGRAM_VARIABLE_TABLE, {ProgramId, Key})
                  end,
    case mnesia:async_dirty(Transaction) of
        [#program_variable_table_entry{value=Value}] ->
            {ok, Value};
        [] ->
            {error, not_found}
    end.

-spec log_program_error(#user_program_log_entry{}) -> ok | {error, atom()}.
log_program_error(LogEntry=#user_program_log_entry{ program_id=ProgramId }) ->
    {LowWatermark, HighWatermark} = automate_configuration:get_program_logs_watermarks(),
    Transaction = fun() ->
                          ok = mnesia:write(?USER_PROGRAM_LOGS_TABLE, LogEntry, write),

                          ProgramEntries = mnesia:read(?USER_PROGRAM_LOGS_TABLE, ProgramId),
                          case length(ProgramEntries) > HighWatermark of
                              false -> ok;
                              true ->
                                  %% Start prunning logs
                                  Sorted = lists:sort(fun( #user_program_log_entry{ event_time=Time1 }
                                                         , #user_program_log_entry{ event_time=Time2 }
                                                         ) ->
                                                              Time1 >= Time2
                                                      end, ProgramEntries),
                                  {Kept, _} = lists:split(LowWatermark, Sorted),

                                  %% Delete old values
                                  ok = mnesia:delete(?USER_PROGRAM_LOGS_TABLE, ProgramId, write),

                                  %% Write new values
                                  lists:foreach(fun(Element) ->
                                                        ok = mnesia:write(?USER_PROGRAM_LOGS_TABLE, Element, write)
                                                end, Kept)
                              end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, Reason}
    end.


-spec add_user_generated_log(#user_generated_log_entry{}) -> ok | {error, atom()}.
add_user_generated_log(LogEntry=#user_generated_log_entry{program_id=ProgramId}) ->
    {LowWatermark, HighWatermark} = automate_configuration:get_program_logs_watermarks(),
    Transaction = fun() ->
                          ok = mnesia:write(?USER_GENERATED_LOGS_TABLE, LogEntry, write),

                          ProgramEntries = mnesia:read(?USER_GENERATED_LOGS_TABLE, ProgramId),
                          case length(ProgramEntries) > HighWatermark of
                              false -> ok;
                              true ->
                                  %% Start prunning logs
                                  Sorted = lists:sort(fun( #user_generated_log_entry{ event_time=Time1 }
                                                         , #user_generated_log_entry{ event_time=Time2 }
                                                         ) ->
                                                              Time1 >= Time2
                                                      end, ProgramEntries),
                                  {Kept, _} = lists:split(LowWatermark, Sorted),

                                  %% Delete old values
                                  ok = mnesia:delete(?USER_GENERATED_LOGS_TABLE, ProgramId, write),

                                  %% Write new values
                                  lists:foreach(fun(Element) ->
                                                        ok = mnesia:write(?USER_GENERATED_LOGS_TABLE, Element, write)
                                                end, Kept)
                          end

                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, Reason}
    end.

-spec get_user_generated_logs(binary()) -> {ok, [#user_generated_log_entry{}]}.
get_user_generated_logs(ProgramId) ->
    Transaction = fun() ->
                          {ok, mnesia:read(?USER_GENERATED_LOGS_TABLE, ProgramId)}
                  end,
    wrap_transaction(mnesia:activity(ets, Transaction)).


-spec mark_successful_call_to_bridge(binary(), binary()) -> ok.
mark_successful_call_to_bridge(ProgramId, _BridgeId) ->
    CurrentTime = erlang:system_time(second),
    Transaction = fun() ->
                          case mnesia:read(?USER_PROGRAMS_TABLE, ProgramId) of
                              [Program=#user_program_entry{}] ->
                                  ok = mnesia:write( ?USER_PROGRAMS_TABLE
                                                   , Program#user_program_entry{ last_successful_call_time=CurrentTime }
                                                   , write
                                                   );
                              [] ->
                                  {error, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

-spec mark_failed_call_to_bridge(binary(), binary()) -> ok.
mark_failed_call_to_bridge(ProgramId, _BridgeId) ->
    CurrentTime = erlang:system_time(second),
    Transaction = fun() ->
                          case mnesia:read(?USER_PROGRAMS_TABLE, ProgramId) of
                              [Program=#user_program_entry{}] ->
                                  ok = mnesia:write( ?USER_PROGRAMS_TABLE
                                                   , Program#user_program_entry{ last_failed_call_time=CurrentTime }
                                                   , write
                                                   );
                              [] ->
                                  {error, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.


-spec get_userid_from_username(binary()) -> {ok, owner_id()} | {error, no_user_found}.
get_userid_from_username(undefined) ->
    {ok, undefined};

get_userid_from_username(Username) ->
    MatchHead = #registered_user_entry{ id='$1'
                                      , username='_'
                                      , canonical_username='$2'
                                      , password='_'
                                      , email='_'
                                      , status='_'
                                      , registration_time='_'
                                      , is_admin='_'
                                      , is_advanced='_'
                                      , is_in_preview='_'
                                      },
    %% Check that neither the id, username or email matches another
    Guard = {'==', '$2', automate_storage_utils:canonicalize(Username)},
    ResultColumn = '$1',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          mnesia:select(?REGISTERED_USERS_TABLE, Matcher)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, [Result] } ->
            {ok, {user, Result}};
        { atomic, [] } ->
            {error, no_user_found};
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

-spec update_user_settings(binary(), map(), [atom()]) -> ok | {error, _}.
update_user_settings(UserId, Settings, Permissions) ->
    Transaction = fun() ->
                          case mnesia:read(?REGISTERED_USERS_TABLE, UserId) of
                              [User] ->
                                  case apply_user_settings(User, Settings, Permissions) of
                                      {ok, NewUser} ->
                                          ok = mnesia:write(?REGISTERED_USERS_TABLE, NewUser, write);
                                      {error, Reason} ->
                                          {error, Reason}
                                  end;
                              [] ->
                                  {error, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, Reason}
    end.


%% Custom signals
-spec create_custom_signal(owner_id(), binary()) -> {ok, binary()}.
create_custom_signal(Owner, SignalName) ->
    {ok, Id} = automate_channel_engine:create_channel(),
    Entry = #custom_signal_entry{ id=Id
                                , name=SignalName
                                , owner=Owner
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


-spec list_custom_signals(owner_id()) -> {ok, [#custom_signal_entry{}]}.
list_custom_signals({OwnerType, OwnerId}) ->
    Transaction = fun() ->
                          %% Find userid with that name
                          MatchHead = #custom_signal_entry{ id='_'
                                                          , name='_'
                                                          , owner={'$1', '$2'}
                                                          },
                          Guards = [ {'==', '$1', OwnerType}
                                   , {'==', '$2', OwnerId}
                                   ],
                          ResultColumn = '$_',
                          Matcher = [{MatchHead, Guards, [ResultColumn]}],

                          {ok, mnesia:select(?CUSTOM_SIGNALS_TABLE, Matcher)}
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

%% Group management
-spec create_group(binary(), binary(), boolean()) -> {ok, #user_group_entry{}} | {error, any()}.
create_group(Name, AdminUserId, Public) ->
    Canonicalized = automate_storage_utils:canonicalize(Name),
    Id = generate_id(),
    CurrentTime = erlang:system_time(second),
    Transaction = fun() ->
                          case mnesia:index_read(?USER_GROUPS_TABLE, Name, #user_group_entry.canonical_name) of
                              [] ->
                                  Entry = #user_group_entry{ id=Id
                                                           , name=Name
                                                           , canonical_name=Canonicalized
                                                           , public=Public
                                                           , creation_time=CurrentTime
                                                           },
                                  ok = mnesia:write(?USER_GROUPS_TABLE, Entry, write),
                                  ok = mnesia:write(?USER_GROUP_PERMISSIONS_TABLE, #user_group_permissions_entry{ group_id=Id
                                                                                                                , user_id={user, AdminUserId}
                                                                                                                , role=admin
                                                                                                                }, write),
                                  {ok, Entry};
                              _ ->
                                  {error, already_exists}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.


-spec delete_group(binary()) -> ok | {error, any()}.
delete_group(GroupId) ->
    T = fun() ->
                ok = mnesia:delete(?USER_GROUPS_TABLE, GroupId, write),
                ok = mnesia:delete(?USER_GROUP_PERMISSIONS_TABLE, GroupId, write)
        end,
    wrap_transaction(mnesia:transaction(T)).

-spec update_group_metadata(binary(), group_metadata_edition()) -> ok | {error, any()}.
update_group_metadata(GroupId, MetadataChanges) ->
    T = fun() ->
                [Group] = mnesia:read(?USER_GROUPS_TABLE, GroupId),
                NewGroup = apply_group_metadata_changes(Group, MetadataChanges),
                mnesia:write(?USER_GROUPS_TABLE, NewGroup, write)
        end,
    wrap_transaction(mnesia:transaction(T)).

-spec get_user_groups(owner_id()) -> {ok, [{#user_group_entry{}, user_in_group_role()}, ...]} | {error, any()}.
get_user_groups(UserId) ->
    Transaction = fun() ->
                          Permissions = mnesia:index_read(?USER_GROUP_PERMISSIONS_TABLE, UserId, #user_group_permissions_entry.user_id),
                          Groups = lists:map(fun(#user_group_permissions_entry{ group_id=GroupId, role=Role }) ->
                                                     [Group] = mnesia:read(?USER_GROUPS_TABLE, GroupId),
                                                     {Group, Role}
                                             end, Permissions),
                          {ok, Groups}
                  end,
    wrap_transaction(mnesia:activity(ets, Transaction)).

-spec get_group_by_name(binary(), owner_id()) -> {ok, #user_group_entry{}} | {error, any()}.
get_group_by_name(GroupName, AccessorId) ->
    CanonicalizedName = automate_storage_utils:canonicalize(GroupName),
    Transaction = fun() ->
                          [Group=#user_group_entry{id=GroupId}] = mnesia:index_read(?USER_GROUPS_TABLE
                                                                                   , CanonicalizedName
                                                                                   , #user_group_entry.canonical_name),

                          case is_allowed_to_read_in_group(AccessorId, GroupId) of
                              true ->
                                  {ok, Group};
                              false ->
                                  {error, not_authorized}
                          end
                  end,
    wrap_transaction(mnesia:activity(ets, Transaction)).

-spec is_allowed_to_read_in_group(owner_id(), binary()) -> true | false.
is_allowed_to_read_in_group({group, GroupId}, GroupId) ->
    true;
is_allowed_to_read_in_group(AccessorId, GroupId) ->
    Transaction = fun() ->
                          lists:any(fun(#user_group_permissions_entry{user_id=UserId}) ->
                                            UserId =:= AccessorId
                                    end, mnesia:read(?USER_GROUP_PERMISSIONS_TABLE, GroupId))
                  end,
    wrap_transaction(mnesia:activity(ets, Transaction)).

-spec is_allowed_to_write_in_group(owner_id(), binary()) -> true | false.
is_allowed_to_write_in_group({group, GroupId}, GroupId) ->
    true;
is_allowed_to_write_in_group(AccessorId, GroupId) ->
    Transaction = fun() ->
                          lists:any(fun(#user_group_permissions_entry{user_id=UserId, role=Role}) ->
                                            (UserId == AccessorId)
                                                and
                                                  ( (Role == admin) or (Role == editor) )
                                    end, mnesia:read(?USER_GROUP_PERMISSIONS_TABLE, GroupId))
                  end,
    wrap_transaction(mnesia:activity(ets, Transaction)).

-spec is_allowed_to_admin_in_group(owner_id(), binary()) -> true | false.
is_allowed_to_admin_in_group({group, GroupId}, GroupId) ->
    true;
is_allowed_to_admin_in_group(AccessorId, GroupId) ->
    Transaction = fun() ->
                          lists:any(fun(#user_group_permissions_entry{user_id=UserId, role=Role}) ->
                                            (UserId == AccessorId)
                                                and
                                                  ( Role == admin )
                                    end, mnesia:read(?USER_GROUP_PERMISSIONS_TABLE, GroupId))
                  end,
    wrap_transaction(mnesia:activity(ets, Transaction)).

-spec can_user_admin_as(owner_id(), owner_id()) -> true | false.
can_user_admin_as(AccessorId, {group, GroupId}) ->
    is_allowed_to_admin_in_group(AccessorId, GroupId);
can_user_admin_as({user, UserId}, {user, UserId}) ->
    true;
can_user_admin_as({user, _UserId}, {user, _AnotherUser}) ->
    false.

-spec can_user_edit_as(owner_id(), owner_id()) -> true | false.
can_user_edit_as(AccessorId, {group, GroupId}) ->
    is_allowed_to_write_in_group(AccessorId, GroupId);
can_user_edit_as({user, UserId}, {user, UserId}) ->
    true;
can_user_edit_as({user, _UserId}, {user, _AnotherUser}) ->
    false.

-spec can_user_view_as(owner_id(), owner_id()) -> true | false.
can_user_view_as(AccessorId, {group, GroupId}) ->
    is_allowed_to_read_in_group(AccessorId, GroupId);
can_user_view_as({user, UserId}, {user, UserId}) ->
    true;
can_user_view_as({user, _UserId}, {user, _AnotherUser}) ->
    false.

-spec list_collaborators({group, binary()}) -> {ok, [{#user_program_entry{}, user_in_group_role()}, ...]} | {error, any()}.
list_collaborators({group, GroupId}) ->
    Transaction = fun() ->
                          Results = lists:map(fun(#user_group_permissions_entry{user_id={user, UserId}, role=Role}) ->
                                                      [User] = mnesia:read(?REGISTERED_USERS_TABLE, UserId),
                                                      {User, Role}
                                              end, mnesia:read(?USER_GROUP_PERMISSIONS_TABLE, GroupId)),
                          {ok, Results}
                  end,
    wrap_transaction(mnesia:activity(ets, Transaction)).

-spec add_collaborators({group, binary()}, [{ Id :: binary(), Role :: user_in_group_role() }]) -> ok | {error, any()}.
add_collaborators({group, GroupId}, Collaborators) ->
    Transaction = fun() ->
                          ok = lists:foreach(fun({ CollaboratorId, CollaboratorRole }) ->
                                                     ok = mnesia:write(?USER_GROUP_PERMISSIONS_TABLE
                                                                      , #user_group_permissions_entry{ group_id=GroupId
                                                                                                     , user_id={user, CollaboratorId}
                                                                                                     , role=CollaboratorRole
                                                                                                     }
                                                                      , write)
                                             end, Collaborators)
                  end,
    wrap_transaction(mnesia:transaction(Transaction)).

-spec update_collaborators({group, binary()}, [{ Id :: binary(), Role :: user_in_group_role() }]) -> ok | {error, any()}.
update_collaborators({group, GroupId}, Collaborators) ->
    Transaction = fun() ->
                          %% Delete all collaborators
                          ok = mnesia:delete(?USER_GROUP_PERMISSIONS_TABLE, GroupId, write),
                          %% And add new ones
                          add_collaborators({group, GroupId}, Collaborators)
                  end,
    wrap_transaction(mnesia:transaction(Transaction)).

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
wrap_transaction(TransactionResult) ->
    case TransactionResult of
        {aborted, Reason} ->
            {error, Reason};
        {atomic, Result} ->
            Result;
        Result ->
            Result
    end.

gen_salt() ->
    gen_salt(?PASSWORD_HASHING_SALTLEN).
gen_salt(SaltLen) ->
    crypto:strong_rand_bytes(SaltLen).

-spec cipher_password(binary()) -> binary() | string().
cipher_password(Plaintext) ->
    Password = Plaintext,
    Salt = gen_salt(),

    {ok, HashResult} = eargon2:hash(?PASSWORD_HASHING_OPS_LIMIT, ?PASSWORD_HASHING_MEM_LIMIT, ?PASSWORD_HASHING_PARALLELISM,
                                    Password, Salt,
                                    ?PASSWORD_HASHING_HASHLEN,
                                    ?EARGON2_RESULT_TYPE_ENCODED, ?EARGON2_HASH_TYPE_ARGON2_I, ?EARGON2_VERSION_NUMBER),

    HashResult.

-spec verify_passwd_hash(binary() | string(), binary()) -> ok | {error, number()}.
verify_passwd_hash(Hash, Password) when is_binary(Hash) ->
    %% Fix mismatch between libsodium and eargon2
    verify_passwd_hash(binary:bin_to_list(Hash), Password);

verify_passwd_hash(Hash=("$argon2i$" ++ _), Password) ->
    %% Handle Argon2 - I
    eargon2:verify_2i(Hash, Password);

verify_passwd_hash(Hash=("$argon2d$" ++ _), Password) ->
    %% Handle Argon2 - D
    eargon2:verify_2d(Hash, Password);

verify_passwd_hash(Hash=("$argon2id$" ++ _), Password) ->
    %% Handle Argon2 - ID
    eargon2:verify_2id(Hash, Password).


add_token_to_user(UserId, SessionToken) ->
    StartTime = erlang:system_time(second),
    Transaction = fun() ->
                          mnesia:write(?USER_SESSIONS_TABLE
                                      , #user_session_entry{ session_id=SessionToken
                                                           , user_id=UserId
                                                           , session_start_time=StartTime
                                                           , session_last_used_time=0
                                                           }
                                      , write)
                  end,
    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

get_userid_sessions(UserId) ->
    %% User session queries
    SessionMatchHead = #user_session_entry{ session_id='_'
                                          , user_id='$1'
                                          , session_start_time='_'
                                          , session_last_used_time='_'
                                          },
    SessionResultColumn = '$_',
    SessionMatcher = [{ SessionMatchHead
                      , [{ '==', '$1', UserId }]
                      , [SessionResultColumn]
                      }],
    mnesia:select(?USER_SESSIONS_TABLE, SessionMatcher).

search_users_iter('$end_of_table', Acc, _QueryRe) ->
    Acc;
search_users_iter(Key, Acc, QueryRe) ->
    [Element] = mnesia:read(?REGISTERED_USERS_TABLE, Key),
    Accumulated = case user_match_query(Element, QueryRe) of
                      true ->
                          [Element | Acc];
                      false ->
                          Acc
                  end,
    search_users_iter(mnesia:next(?REGISTERED_USERS_TABLE, Key), Accumulated, QueryRe).


user_match_query(#registered_user_entry{status=mail_not_verified}, _QueryRe) ->
    false;
user_match_query(#registered_user_entry{username=Username, email=Email}, QueryRe) ->
    case re:run(Username, QueryRe) of
        {match, _} ->
            true;
        nomatch ->
            case re:run(Email, QueryRe) of
                {match, _} ->
                    true;
                nomatch ->
                    false
            end
    end.


query_to_re(Query) ->
    Parts = binary:split(escape_re(Query), [<<" ">>, <<"*">>], [global, trim_all]),
    join_with([<<"">> | Parts], <<".*">>).

escape_re(Expression) ->
    %% Note that Whitespaces and Askterisks (*) are NOT escaped
    re:replace(Expression, "[-[\\]{}()+?.,\\^$|#]", "\\\\&", [{return, binary}, global]).

-spec join_with([binary(), ...], binary()) -> [binary()].
join_with(Parts, Joiner) when is_list(Parts) and is_binary(Joiner) ->
    interleave(Parts, Joiner, []).

interleave([], _Joiner, Acc) ->
    Acc;
interleave([H|T], Joiner, Acc) ->
    interleave(T, Joiner, [H | [ Joiner | Acc]]).

get_user_from_username(Username) ->
    MatchHead = #registered_user_entry{ id='$1'
                                      , username='_'
                                      , canonical_username='$2'
                                      , password='$3'
                                      , email='_'
                                      , status='_'
                                      , registration_time='_'
                                      , is_admin='_'
                                      , is_advanced='_'
                                      , is_in_preview='_'
                                      },
    Guard = {'==', '$2', automate_storage_utils:canonicalize(Username)},
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

get_user_from_email(Email) ->
    MatchHead = #registered_user_entry{ id='$1'
                                      , username='_'
                                      , canonical_username='_'
                                      , password='$3'
                                      , email='$2'
                                      , status='_'
                                      , registration_time='_'
                                      , is_admin='_'
                                      , is_advanced='_'
                                      , is_in_preview='_'
                                      },
    Guard = {'==', '$2', Email},
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

apply_user_settings(User, Settings, Permissions) ->
    apply_user_settings(User, Settings, Permissions, []).

apply_user_settings(User, _Settings, [], []) ->
    %% No more permissions to apply, no errors
    {ok, User};
apply_user_settings(_User, _Settings, [], ErrorAcc) ->
    %% No more permissions to apply, errors found
    {error, {data_error, ErrorAcc}};
apply_user_settings(User, Settings, [ user_permissions | T ], ErrorAcc) ->
    {NewUser, NewErrors} = apply_user_permissions(User, Settings),
    apply_user_settings(NewUser, Settings, T, NewErrors ++ ErrorAcc).

apply_user_permissions(User, Settings) ->
    Errors = [],
    {User1, Errors1} = case Settings of
                           #{ <<"is_advanced">> := IsAdvanced } when is_boolean(IsAdvanced) ->
                               { User#registered_user_entry{ is_advanced=IsAdvanced}, Errors };
                           #{ <<"is_advanced">> := _IsAdvanced } ->
                               %% Is advanced found, but it's not boolean
                               { User, [ { bad_type, is_advanced } | Errors ] };
                           #{} ->
                               { User, Errors }
                       end,
    {User1, Errors1}.

-spec create_verification_entry(binary(), verification_type()) -> {ok, binary()} | {error, _}.
create_verification_entry(UserId, VerificationType) ->
    VerificationId = generate_id(),
    Transaction = fun() ->
                          ok = mnesia:write(?USER_VERIFICATION_TABLE,
                                            #user_verification_entry{ verification_id=VerificationId
                                                                    , user_id=UserId
                                                                    , verification_type=VerificationType
                                                                    }, write)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, ok } ->
            {ok, VerificationId};
        { aborted, Reason } ->
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, Reason}
    end.

check_verification_code(VerificationCode, VerificationType) ->
    Transaction = fun() ->
                          case mnesia:read(?USER_VERIFICATION_TABLE, VerificationCode) of
                              [] ->
                                  {error, not_found};
                              [#user_verification_entry{ verification_type=VerificationType }] ->
                                  ok;
                              [#user_verification_entry{ verification_type=OtherVerificationType }] ->
                                  {error, {invalid_verification_type, OtherVerificationType}}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, {error, {invalid_verification_type, OtherVerificationType}} } ->
            io:fwrite("[Storage] Expected type ~p on verification, found: ~p~n",
                      [VerificationType, OtherVerificationType]),
            {error, invalid_verification_type};
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, Reason}
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
    io:fwrite("\033[7m[retrieve_monitors_list_from_username] To be deprecated\033[0m~n"),
    Transaction = fun() ->
                          %% Find userid with that name
                          UserMatchHead = #registered_user_entry{ id='$1'
                                                                , username='_'
                                                                , canonical_username='$2'
                                                                , password='_'
                                                                , email='_'
                                                                , status='_'
                                                                , registration_time='_'
                                                                , is_admin='_'
                                                                , is_advanced='_'
                                                                , is_in_preview='_'
                                                                },
                          UserGuard = {'==', '$2', automate_storage_utils:canonicalize(Username)},
                          UserResultColumn = '$1',
                          UserMatcher = [{UserMatchHead, [UserGuard], [UserResultColumn]}],

                          case mnesia:select(?REGISTERED_USERS_TABLE, UserMatcher) of
                              [] ->
                                  {error, user_not_found};
                              [UserId] ->

                                  %% Find program with userId and name
                                  MonitorMatchHead = #monitor_entry{ id='$1'
                                                                   , owner={user, '$2'}
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
    io:fwrite("\033[7m[retrieve_program(Username, ProgramName)] To be deprecated\033[0m~n"),
    Transaction = fun() ->
                          %% Find userid with that name
                          UserMatchHead = #registered_user_entry{ id='$1'
                                                                , username='_'
                                                                , canonical_username='$2'
                                                                , password='_'
                                                                , email='_'
                                                                , status='_'
                                                                , registration_time='_'
                                                                , is_admin='_'
                                                                , is_advanced='_'
                                                                , is_in_preview='_'
                                                                },
                          UserGuard = {'==', '$2', automate_storage_utils:canonicalize(Username)},
                          UserResultColumn = '$1',
                          UserMatcher = [{UserMatchHead, [UserGuard], [UserResultColumn]}],

                          case mnesia:select(?REGISTERED_USERS_TABLE, UserMatcher) of
                              [] ->
                                  [];
                              [UserId] ->

                                  %% Find program with userId and name
                                  ProgramMatchHead = #user_program_entry{ id='$1'
                                                                        , owner={user, '$2'}
                                                                        , program_name='$3'
                                                                        , program_type='_'
                                                                        , program_parsed='_'
                                                                        , program_orig='_'
                                                                        , enabled='_'
                                                                        , program_channel='_'
                                                                        , creation_time='_'
                                                                        , last_upload_time='_'
                                                                        , last_successful_call_time='_'
                                                                        , last_failed_call_time='_'
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
    io:fwrite("\033[7m[retrieve_program_list_from_username] To be deprecated\033[0m~n"),
    Transaction = fun() ->
                          %% Find userid with that name
                          UserMatchHead = #registered_user_entry{ id='$1'
                                                                , username='_'
                                                                , canonical_username='$2'
                                                                , password='_'
                                                                , email='_'
                                                                , status='_'
                                                                , registration_time='_'
                                                                , is_admin='_'
                                                                , is_advanced='_'
                                                                , is_in_preview='_'
                                                                },
                          UserGuard = {'==', '$2', automate_storage_utils:canonicalize(Username)},
                          UserResultColumn = '$1',
                          UserMatcher = [{UserMatchHead, [UserGuard], [UserResultColumn]}],

                          case mnesia:select(?REGISTERED_USERS_TABLE, UserMatcher) of
                              [] ->
                                  {error, user_not_found};
                              [UserId] ->

                                  %% Find program with userId and name
                                  ProgramMatchHead = #user_program_entry{ id='$1'
                                                                        , owner={user, '$2'}
                                                                        , program_name='_'
                                                                        , program_type='_'
                                                                        , program_parsed='_'
                                                                        , program_orig='_'
                                                                        , enabled='_'
                                                                        , program_channel='_'
                                                                        , creation_time='_'
                                                                        , last_upload_time='_'
                                                                        , last_successful_call_time='_'
                                                                        , last_failed_call_time='_'
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
    io:fwrite("\033[7m[retrieve_program_list_from_userid] To be deprecated\033[0m~n"),
    Transaction = fun() ->
                          %% Find program with userId and name
                          ProgramMatchHead = #user_program_entry{ id='$1'
                                                                , owner={user, '$2'}
                                                                , program_name='$3'
                                                                , program_type='_'
                                                                , program_parsed='_'
                                                                , program_orig='_'
                                                                , enabled='_'
                                                                , program_channel='_'
                                                                , creation_time='_'
                                                                , last_upload_time='_'
                                                                , last_successful_call_time='_'
                                                                , last_failed_call_time='_'
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
                                                 , pages=Pages
                                                 })->
    io:fwrite("\033[7m[store_new_program_content(Username, ProgramName,...)] To be deprecated\033[0m~n"),

    CurrentTime = erlang:system_time(second),
    Transaction = fun() ->
                          %% Find userid with that name
                          UserMatchHead = #registered_user_entry{ id='$1'
                                                                , username='_'
                                                                , canonical_username='$2'
                                                                , password='_'
                                                                , email='_'
                                                                , status='_'
                                                                , registration_time='_'
                                                                , is_admin='_'
                                                                , is_advanced='_'
                                                                , is_in_preview='_'
                                                                },
                          UserGuard = {'==', '$2', automate_storage_utils:canonicalize(Username)},
                          UserResultColumn = '$1',
                          UserMatcher = [{UserMatchHead, [UserGuard], [UserResultColumn]}],

                          case mnesia:select(?REGISTERED_USERS_TABLE, UserMatcher) of
                              [] ->
                                  [];
                              [UserId] ->

                                  %% Find program with userId and name
                                  ProgramMatchHead = #user_program_entry{ id='$1'
                                                                        , owner={user, '$2'}
                                                                        , program_name='$3'
                                                                        , program_type='_'
                                                                        , program_parsed='_'
                                                                        , program_orig='_'
                                                                        , enabled='_'
                                                                        , program_channel='_'
                                                                        , creation_time='_'
                                                                        , last_upload_time='_'
                                                                        , last_successful_call_time='_'
                                                                        , last_failed_call_time='_'
                                                                        },
                                  ProgramGuard = {'andthen'
                                                 , {'==', '$2', UserId}
                                                 , {'==', '$3', ProgramName}},
                                  ProgramResultColumn = '$_',
                                  ProgramMatcher = [{ProgramMatchHead, [ProgramGuard], [ProgramResultColumn]}],

                                  case mnesia:select(?USER_PROGRAMS_TABLE, ProgramMatcher) of
                                      [] ->
                                          [];

                                      [Program=#user_program_entry{id=ProgramId}] ->
                                          ok = mnesia:write(?USER_PROGRAMS_TABLE,
                                                            Program#user_program_entry{ owner={user, UserId}
                                                                                      , program_name=ProgramName
                                                                                      , program_type=ProgramType
                                                                                      , program_parsed=ProgramParsed
                                                                                      , program_orig=ProgramOrig
                                                                                      , last_upload_time=CurrentTime
                                                                                      }, write),

                                          ok = mnesia:delete(?USER_PROGRAM_EVENTS_TABLE, ProgramId, write),

                                          %% Refresh pages
                                          %% Remove old pages
                                          PagesInDb = mnesia:index_read(?PROGRAM_PAGES_TABLE, ProgramId, program_id),
                                          ok = lists:foreach(fun (PageInDb) ->
                                                                     ok = mnesia:delete_object(?PROGRAM_PAGES_TABLE, PageInDb, write)
                                                             end,
                                                             PagesInDb),

                                          %% Add new pages
                                          ok = lists:foreach(fun({Path, Page}) ->
                                                                        ok = mnesia:write(?PROGRAM_PAGES_TABLE
                                                                                         , #program_pages_entry{ page_id={ ProgramId, Path }
                                                                                                               , program_id=ProgramId
                                                                                                               , contents=Page
                                                                                                               }
                                                                                         , write)
                                                                end, maps:to_list(Pages)),

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


-spec store_new_program_content(binary(), #stored_program_content{}) -> { 'ok', binary() } | { 'error', any() }.
store_new_program_content(ProgramId,
                          #stored_program_content{ orig=ProgramOrig
                                                 , parsed=ProgramParsed
                                                 , type=ProgramType
                                                 , pages=Pages
                                                 })->
    CurrentTime = erlang:system_time(second),
    Transaction = fun() ->
                          case mnesia:read(?USER_PROGRAMS_TABLE, ProgramId) of
                              [] ->
                                  [];

                              [Program=#user_program_entry{id=ProgramId}] ->
                                  ok = mnesia:write(?USER_PROGRAMS_TABLE,
                                                    Program#user_program_entry{ program_type=ProgramType
                                                                              , program_parsed=ProgramParsed
                                                                              , program_orig=ProgramOrig
                                                                              , last_upload_time=CurrentTime
                                                                              }, write),
                                  ok = mnesia:delete(?USER_PROGRAM_EVENTS_TABLE, ProgramId, write),

                                  %% Refresh pages
                                  %% Remove old pages
                                  PagesInDb = mnesia:index_read(?PROGRAM_PAGES_TABLE, ProgramId, program_id),
                                  ok = lists:foreach(fun (PageInDb) ->
                                                             ok = mnesia:delete_object(?PROGRAM_PAGES_TABLE, PageInDb, write)
                                                    end,
                                               PagesInDb),

                                  %% Add new pages
                                  ok = lists:foreach(fun({Path, Contents}) ->
                                                             ok = mnesia:write(?PROGRAM_PAGES_TABLE
                                                                              , #program_pages_entry{ page_id={ ProgramId, Path }
                                                                                                    , program_id=ProgramId
                                                                                                    , contents= Contents
                                                                                                    }
                                                                              , write)
                                                     end, maps:to_list(Pages)),

                                  { ok, ProgramId }
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
                          , canonical_username=CanonicalUsername
                          , email=Email
                          } = UserData,

    MatchHead = #registered_user_entry{ id='$1'
                                      , username='_'
                                      , canonical_username='$2'
                                      , password='_'
                                      , email='$3'
                                      , status='_'
                                      , registration_time='_'
                                      , is_admin='_'
                                      , is_advanced='_'
                                      , is_in_preview='_'
                                      },

    %% Check that neither the id, username or email matches another
    GuardId = {'==', '$1', UserId},
    GuardUsername = {'==', '$2', CanonicalUsername},
    GuardEmail = {'==', '$3', Email},
    Guard = {'orelse', GuardId, GuardUsername, GuardEmail},
    ResultColumn = '$2',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          case mnesia:select(?REGISTERED_USERS_TABLE, Matcher) of
                              [] ->
                                  mnesia:write(?REGISTERED_USERS_TABLE, UserData, write);
                              [CanonicalUsername | _] ->
                                  {error, {colliding_element, username} };
                              _ ->
                                  {error, {colliding_element, email}}
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

-spec set_program_variable(binary(), binary(), any()) -> ok | {error, any()}.
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
            io:format("[~p:~p] Error: ~p~n", [?MODULE, ?LINE, mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.

-spec set_widget_value(ProgramId :: binary(), WidgetId :: binary(), Value :: any()) -> ok.
set_widget_value(ProgramId, WidgetId, Value) ->
    Transaction = fun() ->
                          mnesia:write(?PROGRAM_WIDGET_VALUE_TABLE, #program_widget_value_entry{ widget_id={ProgramId, WidgetId}
                                                                                               , program_id=ProgramId
                                                                                               , value=Value
                                                                                               },
                                       write)
                  end,
    wrap_transaction(mnesia:transaction(Transaction)).

-spec get_widget_values_in_program(ProgramId :: binary()) -> {ok, #{ binary() => any() }}.
get_widget_values_in_program(ProgramId) ->
    T = fun() ->
                mnesia:index_read(?PROGRAM_WIDGET_VALUE_TABLE, ProgramId, program_id)
        end,
    case wrap_transaction(mnesia:ets(T)) of
        {error, Reason}  ->
            {error, Reason};
        Values ->
            MappedValues = maps:from_list(lists:map(fun(#program_widget_value_entry{ widget_id={ _, WidgetId }, value=Value }) ->
                                                            { WidgetId, Value }
                                                    end, Values)),
            {ok, MappedValues}
    end.



-spec apply_group_metadata_changes(#user_group_entry{}, group_metadata_edition()) -> #user_group_entry{}.
apply_group_metadata_changes(Group, MetadataChanges) ->
    apply_group_metadata_public_changes(Group, MetadataChanges).

apply_group_metadata_public_changes(Group=#user_group_entry{}, #{ public := IsPublic }) ->
    Group#user_group_entry{ public=IsPublic };
apply_group_metadata_public_changes(Group, _) ->
    Group.


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

                                             %% This might fail when some nodes are blocked.
                                             %% It is handled as the information itself is not needed.
                                             try mnesia:info() of _ -> ok
                                             catch _:_:_ -> io:fwrite("Error getting mnesia info~n")
                                             end,
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
