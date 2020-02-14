%% Automate storage extension for stats

-module(automate_storage_stats).

-export([ get_user_metrics/0
        ]).

-define(SECONDS_IN_DAY, (60 * 60 * 24)).
-define(SECONDS_IN_28DAY_MONTH, (?SECONDS_IN_DAY * 28)).

-include("./databases.hrl").
-include("./records.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec get_user_metrics() -> {ok, pos_integer(), pos_integer(), pos_integer()}.
get_user_metrics() ->
    %% This is done in a dirty way for the sake of performance.
    %% It's no supposed to have great consistency, but good speed.
    CurrentTime = erlang:system_time(second),

    MatchHead = #registered_user_entry{ id='$1'
                                      , username='_'
                                      , password='_'
                                      , email='_'
                                      , status='_'
                                      , registration_time='$2'
                                      },
    ResultColumn = '$1',

    DailyRegisteredUsersMatcher = [{MatchHead, [{ '>', '$2', CurrentTime - ?SECONDS_IN_DAY }], [ResultColumn]}],
    MonthlyRegisteredUsersMatcher = [{MatchHead, [{ '>', '$2', CurrentTime - ?SECONDS_IN_28DAY_MONTH }], [ResultColumn]}],

    Transaction = fun () ->
                          UserCount = mnesia:table_info(?REGISTERED_USERS_TABLE, size),
                          DailyRegisteredUsers = select_length(?REGISTERED_USERS_TABLE, DailyRegisteredUsersMatcher),
                          MonthlyRegisteredUsers = select_length(?REGISTERED_USERS_TABLE, MonthlyRegisteredUsersMatcher),
                          {ok, UserCount, DailyRegisteredUsers, MonthlyRegisteredUsers}
                  end,
    mnesia:async_dirty(Transaction).

%%====================================================================
%% Internal functions
%%====================================================================
select_length(Tab, Matcher) ->
    case mnesia:select(Tab, Matcher) of
        {aborted, Reason} ->
            {aborted, Reason};
        Records ->
            length(Records)
    end.
