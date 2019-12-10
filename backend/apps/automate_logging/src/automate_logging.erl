%%%-------------------------------------------------------------------
%% @doc automate_logging public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_logging).

%% Application callbacks
-export([log_event/2, log_call_to_bridge/5]).


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

get_timestamp() ->
    erlang:system_time(millisecond).
