%%%-------------------------------------------------------------------
%% @doc automate_channel_engine public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_channel_engine).

%% Public API
-export([ create_channel/0
        , delete_channel/1
        , listen_channel/1
        , listen_channel/2
        , send_to_channel/2
        , monitor_listeners/3
        , get_listeners_on_channel/1
        ]).
-include("records.hrl").
-define(LOGGING, automate_logging).

%%====================================================================
%% API
%%====================================================================
-spec create_channel() -> {ok, binary()} | {error, term(), string()}.
create_channel() ->
    ChannelId = generate_id(),
    case automate_channel_engine_mnesia_backend:register_channel(ChannelId) of
        ok ->
            {ok, ChannelId};
        X ->
            X
    end.

-spec delete_channel(binary()) -> ok | {error, atom()}.
delete_channel(ChannelId) ->
    automate_channel_engine_mnesia_backend:delete_channel(ChannelId).

-spec listen_channel(binary()) -> ok | {error, channel_not_found}.
listen_channel(ChannelId) ->
    automate_channel_engine_mnesia_backend:add_listener_to_channel(ChannelId, self(), node(), undefined, undefined).

-spec listen_channel(binary(), {binary()} | {binary(), binary()}) -> ok | {error, channel_not_found}.
listen_channel(ChannelId, {Key, SubKey}) ->
    automate_channel_engine_mnesia_backend:add_listener_to_channel(ChannelId, self(), node(), Key, SubKey);

listen_channel(ChannelId, {Key}) ->
    automate_channel_engine_mnesia_backend:add_listener_to_channel(ChannelId, self(), node(), Key, undefined).

-spec send_to_channel(binary(), any()) -> ok | {error, channel_not_found }.
send_to_channel(ChannelId, Message) ->
    %% TODO: Use the key/subkey information to better route calls
    spawn(fun () ->
                  automate_stats:log_observation(counter,
                                                 automate_channel_engine_messages_in,
                                                 [ChannelId]),
                  ?LOGGING:log_event(ChannelId, Message)
    end),

    case get_unique_listener_pids_on_channel(ChannelId) of
        {ok, UniquePids} ->
            %% io:format("Forwarding ~p to ~p~n", [Message, Listeners]),
            lists:foreach(fun(Pid) ->
                                  Pid ! {channel_engine, ChannelId, Message},
                                  spawn(fun () ->
                                                automate_stats:log_observation(counter,
                                                                               automate_channel_engine_messages_out,
                                                                               [ChannelId])
                                        end)
                          end, UniquePids),
            ok;
        X ->
            X
    end.

-spec monitor_listeners(binary(), pid(), node()) -> ok | {error, channel_not_found}.
monitor_listeners(ChannelId, Pid, Node) ->
    automate_channel_engine_mnesia_backend:add_listener_monitor(ChannelId, Pid, Node).

-spec get_listeners_on_channel(binary()) -> {ok, [{ pid(), binary() | undefined, binary() | undefined}]} | {error, channel_not_found}.
get_listeners_on_channel(ChannelId) ->
    case automate_channel_engine_mnesia_backend:get_listeners_on_channel(ChannelId) of
        { ok, Listeners } ->
            {ok, [ { Pid, Key, SubKey } || #listeners_table_entry{pid=Pid, key=Key, subkey=SubKey} <- Listeners ]};
        Error ->
            Error
    end.


%%====================================================================
%% Internal functions
%%====================================================================
generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).

get_unique_listener_pids_on_channel(ChannelId) ->
    case automate_channel_engine_mnesia_backend:get_listeners_on_channel(ChannelId) of
        {error, Reason } ->
            {error, Reason};
        {ok, Listeners} ->
            Uniques = sets:from_list(lists:map(fun(#listeners_table_entry{pid=Pid}) -> Pid end,
                                               Listeners)),
            {ok, sets:to_list(Uniques)}
    end.
