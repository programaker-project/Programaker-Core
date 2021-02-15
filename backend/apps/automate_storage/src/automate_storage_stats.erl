%% Automate storage extension for stats

-module(automate_storage_stats).

-export([ get_user_metrics/0
        , get_group_metrics/0
        , get_program_metrics/0
        ]).

-define(SECONDS_IN_HOUR, (60 * 60)).
-define(SECONDS_IN_DAY, (?SECONDS_IN_HOUR * 24)).
-define(SECONDS_IN_7DAY_WEEK, (?SECONDS_IN_DAY * 7)).
-define(SECONDS_IN_28DAY_MONTH, (?SECONDS_IN_DAY * 28)).

-include("./databases.hrl").
-include("./records.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec get_user_metrics() -> { ok
                            , pos_integer(), pos_integer(), pos_integer(), pos_integer()
                            , pos_integer(), pos_integer(), pos_integer(), pos_integer()
                            }.
get_user_metrics() ->
    %% This is done in a dirty way for the sake of performance.
    %% It's no supposed to have great consistency, but good speed.
    CurrentTime = erlang:system_time(second),

    %% User registration queries
    UserMatchHead = #registered_user_entry{ id='$1'
                                          , username='_'
                                          , canonical_username='_'
                                          , password='_'
                                          , email='_'
                                          , status='_'
                                          , registration_time='$2'
                                          , is_admin='_'
                                          , is_advanced='_'
                                          , is_in_preview='_'
                                          },
    UserResultColumn = '$1',

    RegisteredUsersLastDayMatcher = [{ UserMatchHead
                                   , [{ '>', '$2', CurrentTime - ?SECONDS_IN_DAY }]
                                   , [UserResultColumn]}],
    RegisteredUsersLastWeekMatcher = [{ UserMatchHead
                                    , [{ '>', '$2', CurrentTime - ?SECONDS_IN_7DAY_WEEK }]
                                    , [UserResultColumn]}],
    RegisteredUsersLastMonthMatcher = [{ UserMatchHead
                                     , [{ '>', '$2', CurrentTime - ?SECONDS_IN_28DAY_MONTH }]
                                     , [UserResultColumn]}],

    %% User session queries
    SessionMatchHead = #user_session_entry{ session_id='_'
                                          , user_id='$1'
                                          , session_start_time='_'
                                          , session_last_used_time='$2'
                                          },
    SessionResultColumn = '$1',
    HourlyActiveSessionMatcher = [{ SessionMatchHead
                                  , [{ '>', '$2', CurrentTime - ?SECONDS_IN_HOUR }]
                                  , [SessionResultColumn]
                                  }],
    DailyActiveSessionMatcher = [{ SessionMatchHead
                                 , [{ '>', '$2', CurrentTime - ?SECONDS_IN_DAY }]
                                 , [SessionResultColumn]
                                 }],
    WeeklyActiveSessionMatcher = [{ SessionMatchHead
                                  , [{ '>', '$2', CurrentTime - ?SECONDS_IN_7DAY_WEEK }]
                                  , [SessionResultColumn]
                                  }],
    MonthlyActiveSessionMatcher = [{ SessionMatchHead
                                   , [{ '>', '$2', CurrentTime - ?SECONDS_IN_28DAY_MONTH }]
                                   , [SessionResultColumn]
                                   }],

    Transaction = fun () ->
                          UserCount = mnesia:table_info(?REGISTERED_USERS_TABLE, size),
                          RegisteredUsersLastDay = select_length(?REGISTERED_USERS_TABLE, RegisteredUsersLastDayMatcher),
                          RegisteredUsersLastWeek = select_length(?REGISTERED_USERS_TABLE, RegisteredUsersLastWeekMatcher),
                          RegisteredUsersLastMonth = select_length(?REGISTERED_USERS_TABLE, RegisteredUsersLastMonthMatcher),

                          LoggedUsersLastHour = select_unique_length(?USER_SESSIONS_TABLE, HourlyActiveSessionMatcher),
                          LoggedUsersLastDay = select_unique_length(?USER_SESSIONS_TABLE, DailyActiveSessionMatcher),
                          LoggedUsersLastWeek = select_unique_length(?USER_SESSIONS_TABLE, WeeklyActiveSessionMatcher),
                          LoggedUsersLastMonth = select_unique_length(?USER_SESSIONS_TABLE, MonthlyActiveSessionMatcher),

                          { ok
                          , UserCount, RegisteredUsersLastDay, RegisteredUsersLastWeek, RegisteredUsersLastMonth
                          , LoggedUsersLastHour, LoggedUsersLastDay, LoggedUsersLastWeek, LoggedUsersLastMonth
                          }
                  end,
    mnesia:async_dirty(Transaction).

get_group_metrics() ->
    %% This is done in a dirty way for the sake of performance.
    %% It's no supposed to have great consistency, but good speed.
    CurrentTime = erlang:system_time(second),

    %% Group queries
    GroupMatchHead = #user_group_entry{ id='$1'
                                      , name='_'
                                      , canonical_name='_'
                                      , public='_'
                                      , creation_time='$2'
                                      , min_level_for_private_bridge_usage='_'
                                      },
    GroupResultColumn = '$1',

    CreatedGroupsLastDayMatcher = [{ GroupMatchHead
                                   , [{ '>', '$2', CurrentTime - ?SECONDS_IN_DAY }]
                                   , [GroupResultColumn]}],
    CreatedGroupsLastWeekMatcher = [{ GroupMatchHead
                                    , [{ '>', '$2', CurrentTime - ?SECONDS_IN_7DAY_WEEK }]
                                    , [GroupResultColumn]}],
    CreatedGroupsLastMonthMatcher = [{ GroupMatchHead
                                     , [{ '>', '$2', CurrentTime - ?SECONDS_IN_28DAY_MONTH }]
                                     , [GroupResultColumn]}],

    Transaction = fun () ->
                          GroupCount = mnesia:table_info(?USER_GROUPS_TABLE, size),
                          CreatedGroupsLastDay = select_length(?USER_GROUPS_TABLE, CreatedGroupsLastDayMatcher),
                          CreatedGroupsLastWeek = select_length(?USER_GROUPS_TABLE, CreatedGroupsLastWeekMatcher),
                          CreatedGroupsLastMonth = select_length(?USER_GROUPS_TABLE, CreatedGroupsLastMonthMatcher),

                          { ok
                          , GroupCount, CreatedGroupsLastDay, CreatedGroupsLastWeek, CreatedGroupsLastMonth
                          }
                  end,
    mnesia:async_dirty(Transaction).

-spec get_program_metrics() -> {ok, #{ program_id() => #{log_severity() => non_neg_integer() }}}.
get_program_metrics() ->
    %% Get programs
    Transaction = fun () ->
                          { ok
                          , cross_db_from_id_to_map(
                              ?USER_PROGRAMS_TABLE, ?USER_PROGRAM_LOGS_TABLE,
                              fun (_ProgId, Logs) ->
                                      map_count_group2_by(fun(#user_program_log_entry{ severity=Severity, event_data=Data }) ->
                                                                  Type = case Data of
                                                                             %% Bridge errors
                                                                             { badmatch, {error, no_connection} } ->
                                                                                 no_connection;

                                                                             { program_error, {disconnected_bridge, _, _}, _ } ->
                                                                                 no_connection;
                                                                             { program_error, {bridge_call_connection_not_found, _, _}, _ } ->
                                                                                 bridge_call_connection_not_found;

                                                                             { program_error, {bridge_call_timeout, _, _}, _ } ->
                                                                                 bridge_call_timeout;
                                                                             { program_error, {bridge_call_failed, _, _, _}, _ } ->
                                                                                 bridge_call_failed;
                                                                             { program_error, {bridge_call_error_getting_resource, _, _}, _ } ->
                                                                                 bridge_call_error_getting_resource;

                                                                             %% Program errors
                                                                             { program_error, {variable_not_set, _}, _ } ->
                                                                                 variable_not_set;
                                                                             { program_error, {list_not_set, _}, _ } ->
                                                                                 list_not_set;
                                                                             { program_error, {index_not_in_list, _, _, _}, _ } ->
                                                                                 index_not_in_list;
                                                                             { program_error, {memory_not_set, _}, _ } ->
                                                                                 memory_not_set;

                                                                             %% Version errors
                                                                             bad_operation ->
                                                                                 bad_operation;

                                                                             %% Platform errors
                                                                             {badmatch, {error, _}} ->
                                                                                 platform_error;
                                                                             function_clause ->
                                                                                 platform_error;
                                                                             {program_error, {unknown_operation}, _} ->
                                                                                 platform_error;

                                                                             %% Unknown errorrs
                                                                             undef ->
                                                                                 undefined;
                                                                             {EventType, _} ->
                                                                                 binary:list_to_bin(
                                                                                   lists:flatten(io_lib:fwrite("unknown_~p",
                                                                                                               [EventType])));
                                                                             _ ->
                                                                                 binary:list_to_bin(
                                                                                   lists:flatten(io_lib:fwrite("unknown_~p",
                                                                                                               [Data])))
                                                                         end,
                                                                  {Severity, Type}
                                                          end, Logs)
                                                    end)
                          }
                  end,
    mnesia:async_dirty(Transaction).

%%====================================================================
%% Internal functions
%%====================================================================
select_length(Tab, Matcher) ->
    case mnesia:select(Tab, Matcher) of
        Records ->
            length(Records)
    end.

select_unique_length(Tab, Matcher) ->
    case mnesia:select(Tab, Matcher) of
        Records ->
            Unique = sets:from_list(Records),
            sets:size(Unique)
    end.

cross_db_from_id_to_map(Left, Right, FCross) ->
    cross_db_from_id_to_map_iter(Left, Right, FCross, mnesia:first(Left), #{}).

cross_db_from_id_to_map_iter(_Left, _Right, _FCross, '$end_of_table', Acc) ->
    Acc;
cross_db_from_id_to_map_iter(Left, Right, FCross, Key, Acc) ->
    Elements = mnesia:read(Right, Key),
    NewElements = FCross(Key, Elements),
    cross_db_from_id_to_map_iter(Left, Right, FCross, mnesia:next(Left, Key), Acc#{ Key => NewElements }).

map_count_group2_by(FSelect, List) ->
    map_count_group2_by_iter(FSelect, List, #{}).

map_count_group2_by_iter(_FSelect, [], Acc) ->
    Acc;
map_count_group2_by_iter(FSelect, [H | T], Acc) ->
    {Key, SubKey} = FSelect(H),
    Values = case Acc of
                 #{ Key := Prev=#{ SubKey := Value } } ->
                     Prev#{ SubKey => Value + 1 };
                 #{ Key := Prev } ->
                     Prev#{ SubKey => 1 };
                 _ ->
                     #{ SubKey => 1 }
             end,
    map_count_group2_by_iter(FSelect, T, Acc#{ Key => Values }).
