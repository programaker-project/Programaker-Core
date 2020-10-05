%%% @doc
%%% REST endpoint to retrieve platform stats.
%%% @end

-module(automate_rest_api_admin_stats_root).
-export([init/2]).
-export([ allowed_methods/2
        , is_authorized/2
        , content_types_provided/2
        , options/2
        ]).

-export([ to_json/2
        ]).
-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_stats/src/records.hrl").
-include("../../automate_storage/src/records.hrl").

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

-spec is_authorized(cowboy_req:req(),_) -> {'true' | {'false', binary()}, cowboy_req:req(),_}.
is_authorized(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };
        _ ->
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    { {false, <<"Authorization header not found">>} , Req1, State };
                X ->
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            case automate_storage:get_user(UserId) of
                                {ok, #registered_user_entry{ is_admin=true }} ->
                                    { true, Req1, State };
                                {ok, _} ->
                                    { { false, <<"User not authorized (not admin)">>}, Req1, State };
                                {error, Reason} ->
                                    automage_logging:log_api(error, ?MODULE, Reason)
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

%%%% GET
-spec to_json(cowboy_req:req(),#rest_session{}) -> {binary(),cowboy_req:req(),_}.
to_json(Req, Session) ->
    {ok, Metrics, Errors} = automate_stats:get_internal_metrics(),
    Output = jiffy:encode(#{ stats => serialize_stats(Metrics)
                           , errors => serialize_errors(Errors)
                           }),
    Res = ?UTILS:send_json_format(Req),
    { Output, Res, Session }.


%% Serialization
serialize_stats(#internal_metrics{ services_active=ServiceCounts
                                 , bot_count=BotCount
                                 , thread_count=ThreadCount
                                 , monitor_count=MonitorCount
                                 , service_count=ServiceCount
                                 , user_stats=#user_stat_metrics{ count=UserCount
                                                                , registered_last_day=RegisteredUsersLastDay
                                                                , registered_last_week=RegisteredUsersLastWeek
                                                                , registered_last_month=RegisteredUsersLastMonth
                                                                , logged_last_hour=LoggedUsersLastHour
                                                                , logged_last_day=LoggedUsersLastDay
                                                                , logged_last_week=LoggedUsersLastWeek
                                                                , logged_last_month=LoggedUsersLastMonth
                                                                }
                                 , group_stats=#group_stat_metrics{ count=GroupCount
                                                                  , created_last_day=CreatedGroupsLastDay
                                                                  , created_last_week=CreatedGroupsLastWeek
                                                                  , created_last_month=CreatedGroupsLastMonth
                                                                  }
                                 , bridge_stats=#bridge_stat_metrics{ public_count=NumBridgesPublic
                                                                    , private_count=NumBridgesPrivate
                                                                    , connections=NumConnections
                                                                    , unique_connections=NumUniqueConnections
                                                                    , messages_on_flight=NumMessagesOnFlight
                                                                    }
                                 }) ->
    #{ active_services => map_with_null_values(ServiceCounts)
     , bot_count => map_with_null_values(BotCount)
     , thread_count => map_with_null_values(ThreadCount)
     , monitor_count => map_with_null_values(MonitorCount)
     , service_count => map_with_null_values(ServiceCount)
     , users => #{ count => UserCount
                 , registered_last_day => RegisteredUsersLastDay
                 , registered_last_week => RegisteredUsersLastWeek
                 , registered_last_month => RegisteredUsersLastMonth
                 , logged_last_hour => LoggedUsersLastHour
                 , logged_last_day => LoggedUsersLastDay
                 , logged_last_week => LoggedUsersLastWeek
                 , logged_last_month => LoggedUsersLastMonth
                 }
     , groups => #{ count => GroupCount
                  , created_last_day => CreatedGroupsLastDay
                  , created_last_week => CreatedGroupsLastWeek
                  , created_last_month => CreatedGroupsLastMonth
                  }
     , bridges => #{ public_count => NumBridgesPublic
                   , private_count => NumBridgesPrivate
                   , connections => NumConnections
                   , unique_connections => NumUniqueConnections
                   , messages_on_flight => NumMessagesOnFlight
                   }
     }.

serialize_errors(Errors) ->
    lists:map(fun(Err) -> serialize_error(Err) end, Errors).

serialize_error({Module, { ErrorNS, Error, _StackTrace }}) ->
    binary:list_to_bin(lists:flatten(io_lib:format("Failed to get value from ~p (~p:~p)", [Module, ErrorNS, Error])));
serialize_error({Module, Reason}) ->
    binary:list_to_bin(lists:flatten(io_lib:format("Failed to get value from ~p: ~s", [Module, Reason]))).

map_with_null_values(Orig) ->
    maps:map(fun(_, V) ->
                     case V of
                         undefined -> null;
                         _ -> V
                     end
             end, Orig).
