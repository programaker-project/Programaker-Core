%%%-------------------------------------------------------------------
%% @doc automate_logging public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_logging).

%% Application callbacks
-export([ log_event/2
        , log_signal_to_bridge_and_owner/3
        , get_signal_by_bridge_and_owner_history/2
        , log_program_call_by_user/2
        , log_call_to_bridge/5
        , log_program_error/1
        , add_user_generated_program_log/1
        , log_platform/4
        , log_platform/2
        , log_bridge/2
        , log_api/3
        ]).

-define(DEFAULT_LOG_HISTORY_RETRIEVE, 1000).
-include("../../automate_storage/src/records.hrl").
-include("../../automate_bot_engine/src/program_records.hrl").
-include("./records.hrl").

%%====================================================================
%% Logging API
%%====================================================================
-spec log_event(binary(), any()) -> ok.
log_event(Channel, Message) ->
    case automate_service_port_engine:get_channel_origin_bridge(Channel) of
        {ok, BridgeId} ->
            Info = #{ <<"channel">> => Channel
                    , <<"message">> => Message
                    , <<"bridge">> => BridgeId
                    , <<"@timestamp">> => get_timestamp()
                    },
            Method = post,
            Config = get_config(),
            case Config of
                #{ "type" := elasticsearch
                 , "url" := BaseURL
                 , "index_prefix" := Index
                 , "exclude_bridges" := Excluded
                 , "user" := User
                 , "password" := Password
                 } ->
                    case lists:member(BridgeId, Excluded) of
                        false ->
                            Token  = base64:encode_to_string(User ++ ":" ++ Password),
                            Header = [{"Authorization", "Basic " ++ Token }],
                            URL = BaseURL ++ Index ++ "_event/_doc",
                            Type = "application/json",
                            Body = jiffy:encode(Info),
                            HTTPOptions = [],
                            Options = [],
                            {ok, _} = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
                            ok;
                        true ->
                            ok
                    end;
                none -> ok
            end;
        {error, not_found} ->
            %% io:fwrite("[DEBUG][logging] No bridge found for ~p~n", [Channel]),
            ok
    end.

-spec log_signal_to_bridge_and_owner(Signal :: any(), BridgeId :: binary(), Owner :: owner_id()) -> ok.
log_signal_to_bridge_and_owner(Signal, BridgeId, {OwnerType, OwnerId}) ->
    Config = get_signal_storage_config(),
    case Config of
        #{ type := raw
         , url := BaseURL
         } ->
            Url = lists:flatten(io_lib:format("~s/~s_~p_~s", [BaseURL, BridgeId, OwnerType, OwnerId])),
            Type = "application/json",
            Body = list_to_binary([jiffy:encode(Signal)]),
            Headers = [],
            HTTPOptions = [],
            Options = [],
            case httpc:request(post, {Url, Headers, Type, Body}, HTTPOptions, Options) of
                {ok, _} -> ok;
                {error, Reason} ->
                    log_platform(error, list_to_binary(io_lib:format("Error logging signal: ~p", [Reason])))
            end;
        undefined ->
            io:fwrite("[Error] Signal logging configuration not set")
    end.

-spec get_signal_by_bridge_and_owner_history(BridgeId :: binary(), Owner :: owner_id()) -> {ok, iolist()} | {error, _}.
get_signal_by_bridge_and_owner_history(BridgeId, {OwnerType, OwnerId}) ->
    Config = get_signal_storage_config(),
    case Config of
        #{ type := raw
         , url := BaseURL
         } ->
            Url = lists:flatten(io_lib:format("~s/~s_~p_~s?q=latest&n=~p", [BaseURL, BridgeId, OwnerType, OwnerId, ?DEFAULT_LOG_HISTORY_RETRIEVE])),
            Headers = [],
            HTTPOptions = [],
            Options = [{body_format, binary}],
            {ok, { {_, StatusCode, _StatusPhrase}, _Headers, Body }
            } = httpc:request(get, {Url, Headers}, HTTPOptions, Options),
            2 = StatusCode div 100, %% Expect a 2XX status code.
            { ok
            , [<<"[">>, binary:replace(Body, <<"\0">>, <<",">>, [global]), <<"]">>]
            };
        undefined ->
            {error, no_signal_logging};
        none ->
            {error, no_signal_logging}
    end.

-spec log_program_call_by_user(CallData :: #call_data{}, Owner :: owner_id() | 'none' | 'undefined') -> ok.
log_program_call_by_user(CallData, undefined) ->
    io:fwrite("[WARN] Cannot log call which is done by no user~n"),
    ok;
log_program_call_by_user(CallData, none) ->
    io:fwrite("[WARN] Cannot log call which is done by no user~n"),
    ok;
log_program_call_by_user(CallData, {OwnerType, OwnerId}) ->
    ProgramConfig = get_program_call_log_storage_config(),
    case ProgramConfig of
        #{ type := raw
         , url := BaseURL
         } ->
            Url = lists:flatten(io_lib:format("~s/~p_~s", [BaseURL, OwnerType, OwnerId])),
            Type = "application/json",
            try
                list_to_binary([jiffy:encode(to_map(CallData))])
            of
                Body ->
                    Headers = [],
                    HTTPOptions = [],
                    Options = [],
                    case httpc:request(post, {Url, Headers, Type, Body}, HTTPOptions, Options) of
                        {ok, _} -> ok;
                        {error, Reason} ->
                            log_platform(error, list_to_binary(io_lib:format("Error logging signal: ~p", [Reason])))
                    end
            catch
                ErrType:ErrReason:ErrStack ->
                    io:fwrite("[Error] Preparing data to log signal: ~p~n", [ErrType, ErrReason])
                end;
        undefined ->
            io:fwrite("[Error] Signal logging configuration not set~n");
        none ->
            io:fwrite("[WARN] Signal logging configuration not set~n")
    end.

-spec log_call_to_bridge(binary(), binary(), binary(), binary(), map()) -> ok.
log_call_to_bridge(BridgeId, FunctionName, Arguments, UserId, ExtraData) ->
    Info = #{ <<"bridge_id">> => BridgeId
            , <<"function_name">> => FunctionName
            , <<"arguments">> => Arguments
            , <<"user_id">> => UserId
            , <<"extra_data">> => ExtraData
            , <<"@timestamp">> => get_timestamp()
            },
    Method = post,
    Config = get_config(),
    case Config of
        #{ "type" := elasticsearch
         , "url" := BaseURL
         , "index_prefix" := Index
         , "user" := User
         , "password" := Password
         } ->
            Token  = base64:encode_to_string(User ++ ":" ++ Password),
            Header = [{"Authorization", "Basic " ++ Token }],
            URL = BaseURL ++ Index ++ "_call_to_bridge/_doc",
            Type = "application/json",
            Body = jiffy:encode(Info),
            HTTPOptions = [],
            Options = [],
            {ok, R} = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
            io:fwrite("Logging response: ~p~n",[R]),
            ok;
        none -> ok
    end.

-spec log_program_error(#user_program_log_entry{}) -> ok | {error, atom()}.
log_program_error(LogEntry=#user_program_log_entry{ severity=Severity, program_id=ProgramId }) ->
    case automate_storage:get_program_from_id(ProgramId) of
        {ok, #user_program_entry{ program_channel=Channel }} ->
            automate_channel_engine:send_to_channel(Channel, LogEntry);
        {error, not_found} ->
            log_platform(Severity, io_lib:format(
                                     "Cannot log error on program '~p', channel not found",
                                     [ProgramId]))
    end,

    automate_storage:log_program_error(LogEntry).

-spec add_user_generated_program_log(#user_generated_log_entry{}) -> ok | {error, atom()}.
add_user_generated_program_log(LogEntry=#user_generated_log_entry{ program_id=ProgramId, severity=Severity }) ->
    case automate_storage:get_program_from_id(ProgramId) of
        {ok, #user_program_entry{ program_channel=Channel }} ->
            automate_channel_engine:send_to_channel(Channel, LogEntry);
        {error, not_found} ->
            log_platform(Severity, io_lib:format(
                                     "Cannot log error on program '~p', channel not found",
                                     [ProgramId]))
    end,

    automate_storage:add_user_generated_log(LogEntry).


-spec log_platform(log_severity(), _, _, _) -> ok.
log_platform(warning, ErrorNS, Error, _StackTrace) ->
    io:fwrite("~s [~p] ~p:~p~n", [get_time_string(), warning, ErrorNS, Error]);
log_platform(debug, _ErrorNS, _Error, _StackTrace) ->
    ok; %% Ignored for now

log_platform(Severity, ErrorNS, Error, StackTrace) ->
    io:fwrite("~s [~p] ~p:~p || ~p~n", [get_time_string(), Severity, ErrorNS, Error, StackTrace]).

-spec log_platform(atom(), _) -> ok.
log_platform(Severity, Msg) when is_list(Msg) ->
    io:fwrite("~s [~p] ~s~n", [get_time_string(), Severity, binary:list_to_bin(lists:flatten(Msg))]);
log_platform(Severity, Msg) ->
    io:fwrite("~s [~p] ~p~n", [get_time_string(), Severity, Msg]).

-spec log_bridge(log_severity(), iolist()) -> ok.
log_bridge(Severity, Msg) ->
    io:fwrite("~s [~p] ~s~n", [get_time_string(), Severity, binary:list_to_bin([Msg])]).

-spec log_api(log_severity(), _, _) -> ok.
log_api(debug, _, _) ->
    ok; %% Ignored for now
log_api(Severity, Endpoint, Error) when is_binary(Error) ->
    io:fwrite("~s [~p@~p] ~s~n", [get_time_string(), Severity, Endpoint, Error]);
log_api(Severity, Endpoint, Error) ->
    io:fwrite("~s [~p@~p] ~p~n", [get_time_string(), Severity, Endpoint, Error]).


%%====================================================================
%% Internal functions
%%====================================================================
get_config() ->
    case application:get_env(automate_logging, endpoint) of
        {ok, [Config]} ->
            Config;
        undefined ->
            none
    end.

get_signal_storage_config() ->
    case application:get_env(automate_logging, signal_storage_endpoint) of
        {ok, Config} ->
            Config;
        undefined ->
            none
    end.

get_program_call_log_storage_config() ->
    case application:get_env(automate_logging, program_call_log_storage_endpoint) of
        {ok, Config} ->
            Config;
        undefined ->
            none
    end.

get_timestamp() ->
    erlang:system_time(millisecond).

get_time_string() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
    io_lib:format("~4..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Min, Sec]).

-spec to_map(#call_data{}) -> map().
to_map(#call_data{ call_start_time=CallStartTime
                 , call_end_time=CallEndTime
                 , program_id=ProgramId
                 , operation=Operation
                 , arguments=Arguments
                 , result=Result
    }) ->
    #{ call_start_time => CallStartTime
     , call_end_time =>   CallEndTime
     , program_id => ProgramId
     , operation =>  Operation
     , arguments =>  case Arguments of
                        Tup when is_tuple(Tup) ->
                            tuple_to_list(Tup);
                        _ -> Arguments
                    end
     , result => case Result of
                    #program_error{} ->
                        %% HACK: It's not ideal to require something at the
                        %%   "bottom" of the module dependency graph from the top.
                        <<"error">>;
                    _ -> Result
                end

    }.
