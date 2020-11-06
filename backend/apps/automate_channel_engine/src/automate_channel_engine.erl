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
-define(MONITOR, automate_channel_engine_listener_monitor).

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
    listen_channel(ChannelId, {undefined, undefined}).

-spec listen_channel(binary(), {binary()} | {binary() | undefined, binary() | undefined}) -> ok | {error, channel_not_found}.
listen_channel(ChannelId, {Key}) ->
    listen_channel(ChannelId, {Key, undefined});

listen_channel(ChannelId, {Key, SubKey}) ->
    Pid = self(),
    Node = node(),
    case automate_channel_engine_mnesia_backend:add_listener_to_channel(ChannelId, Pid, Node,
                                                                        automate_channel_engine_utils:canonicalize_selector(Key),
                                                                        automate_channel_engine_utils:canonicalize_selector(SubKey)) of
        ok ->
            ?MONITOR:monitor_listener(Pid);
        Error ->
            Error
    end.

-spec send_to_channel(binary(), any()) -> ok | {error, channel_not_found }.
send_to_channel(ChannelId, Message) ->
    spawn(fun () ->
                  automate_stats:log_observation(counter,
                                                 automate_channel_engine_messages_in,
                                                 [ChannelId]),
                  ?LOGGING:log_event(ChannelId, Message)
    end),

    case get_appropriate_listeners(ChannelId, Message) of
        {ok, []} ->
            ok;

        {ok, UniquePids} ->
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

get_appropriate_listeners(ChannelId, #{ <<"key">> := Key, <<"subkey">> := SubKey }) ->
    get_appropriate_listeners_key_subkey(ChannelId, {Key, SubKey});
get_appropriate_listeners(ChannelId, #{ <<"key">> := Key }) ->
    get_appropriate_listeners_key_subkey(ChannelId, {Key, null});
get_appropriate_listeners(ChannelId, _Message) ->
    get_appropriate_listeners_key_subkey(ChannelId, {null, null}).

get_appropriate_listeners_key_subkey(ChannelId, {Key, SubKey}) ->
    case automate_channel_engine_mnesia_backend:get_listeners_on_channel(ChannelId) of
        {error, Reason } ->
            {error, Reason};
        {ok, Listeners} ->
            CanonicalKey = automate_channel_engine_utils:canonicalize_selector(Key),
            CanonicalSubKey = automate_channel_engine_utils:canonicalize_selector(SubKey),
            Uniques = sets:from_list(lists:filtermap(fun(#listeners_table_entry{pid=Pid, key=ListenerKey, subkey=ListenerSubKey}) ->
                                                             AcceptedKey = case ListenerKey of
                                                                               CanonicalKey -> true;
                                                                               undefined -> true;
                                                                               _ -> false
                                                                           end,
                                                             AcceptedSubKey = case ListenerSubKey of
                                                                                  CanonicalSubKey -> true;
                                                                                  undefined -> true;
                                                                                  _ -> false
                                                                              end,
                                                             case AcceptedKey and AcceptedSubKey of
                                                                 true ->
                                                                     {true, Pid};
                                                                 false -> false
                                                             end
                                                     end,
                                                     Listeners)),
            {ok, sets:to_list(Uniques)}
    end.
