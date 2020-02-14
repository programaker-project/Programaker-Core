%% Automate storage extension for stats

-module(automate_storage_stats).

-export([ get_user_metrics/0
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
                                          , password='_'
                                          , email='_'
                                          , status='_'
                                          , registration_time='$2'
                                          },
    UserResultColumn = '$1',

    DailyRegisteredUsersMatcher = [{ UserMatchHead
                                   , [{ '>', '$2', CurrentTime - ?SECONDS_IN_DAY }]
                                   , [UserResultColumn]}],
    WeeklyRegisteredUsersMatcher = [{ UserMatchHead
                                    , [{ '>', '$2', CurrentTime - ?SECONDS_IN_7DAY_WEEK }]
                                    , [UserResultColumn]}],
    MonthlyRegisteredUsersMatcher = [{ UserMatchHead
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
                          DailyRegisteredUsers = select_length(?REGISTERED_USERS_TABLE, DailyRegisteredUsersMatcher),
                          WeeklyRegisteredUsers = select_length(?REGISTERED_USERS_TABLE, WeeklyRegisteredUsersMatcher),
                          MonthlyRegisteredUsers = select_length(?REGISTERED_USERS_TABLE, MonthlyRegisteredUsersMatcher),

                          LoggedUsersLastHour = select_unique_length(?USER_SESSIONS_TABLE, HourlyActiveSessionMatcher),
                          LoggedUsersLastDay = select_unique_length(?USER_SESSIONS_TABLE, DailyActiveSessionMatcher),
                          LoggedUsersLastWeek = select_unique_length(?USER_SESSIONS_TABLE, WeeklyActiveSessionMatcher),
                          LoggedUsersLastMonth = select_unique_length(?USER_SESSIONS_TABLE, MonthlyActiveSessionMatcher),

                          { ok
                          , UserCount, DailyRegisteredUsers, WeeklyRegisteredUsers, MonthlyRegisteredUsers
                          , LoggedUsersLastHour, LoggedUsersLastDay, LoggedUsersLastWeek, LoggedUsersLastMonth
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
