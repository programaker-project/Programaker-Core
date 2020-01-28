%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_channel_engine_mnesia_backend).

-export([ start_link/0
        , register_channel/1
        , add_listener_to_channel/5
        , get_listeners_on_channel/1
        , add_listener_monitor/3
        ]).

-include("records.hrl").
-include("databases.hrl").

%%====================================================================
%% API
%%====================================================================
start_link() ->
    Nodes = automate_configuration:get_sync_peers(),

    ok = automate_storage_versioning:apply_versioning(
           automate_channel_engine_configuration:get_versioning(Nodes),
           Nodes, ?MODULE),

    %% These are run on RAM, so they are created manually
    ok = case mnesia:create_table(?LISTENERS_TABLE,
                                  [ { attributes, record_info(fields, listeners_table_entry)}
                                  , { ram_copies, Nodes }
                                  , { record_name, listeners_table_entry }
                                  , { type, bag }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    ok = case mnesia:create_table(?MONITORS_TABLE,
                                  [ { attributes, record_info(fields, monitors_table_entry)}
                                  , { ram_copies, Nodes }
                                  , { record_name, monitors_table_entry }
                                  , { type, bag }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    ok = mnesia:wait_for_tables([ ?LIVE_CHANNELS_TABLE
                                , ?LISTENERS_TABLE
                                ], automate_configuration:get_table_wait_time()),
    ignore.

-spec register_channel(binary()) -> ok | {error, term(), string()}.
register_channel(ChannelId) ->
    Entry = #live_channels_table_entry{live_channel_id=ChannelId, stats=[]},
    Transaction = fun() ->
                          ok = mnesia:write(?LIVE_CHANNELS_TABLE, Entry, write)
                  end,

    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec add_listener_to_channel(binary(), pid(), node(), binary() | undefined, binary() | undefined) -> ok | {error, channel_not_found}.
add_listener_to_channel(ChannelId, Pid, Node, Key, SubKey) ->
    Transaction = fun() ->
                          case mnesia:read(?LIVE_CHANNELS_TABLE, ChannelId) of
                              [] -> {error, channel_not_found};
                              _ -> mnesia:write(?LISTENERS_TABLE
                                               , #listeners_table_entry
                                                { live_channel_id=ChannelId
                                                , pid=Pid
                                                , node=Node
                                                , key=Key
                                                , subkey=SubKey
                                                }
                                               , write)
                          end
                  end,

    {atomic, Result} = mnesia:transaction(Transaction),
    case Result of
        ok ->
            case get_monitors_on_channel(ChannelId) of
                {ok, Monitors} ->
                    [ MonPid ! { automate_channel_engine, add_listener, { Pid, Key, SubKey } }
                      || #monitors_table_entry{pid=MonPid} <- Monitors ],
                    ok
            end;
        _ ->
            Result
    end.

-spec get_listeners_on_channel(binary()) -> {ok, [#listeners_table_entry{}]}
                                                | {error, channel_not_found}.
get_listeners_on_channel(ChannelId) ->
    MatchHead = #listeners_table_entry{ live_channel_id='$1'
                                      , pid='_'
                                      , node='_'
                                      , key='_'
                                      , subkey='_'
                                      },
    %% Check that neither the id, username or email matches another
    Guard = {'==', '$1', ChannelId},
    ResultColumn = '$_',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          case mnesia:read(?LIVE_CHANNELS_TABLE, ChannelId) of
                              [] -> {error, channel_not_found};
                              _ -> {ok, mnesia:select(?LISTENERS_TABLE, Matcher)}
                          end
                  end,

    {atomic, Result} = mnesia:transaction(Transaction),

    case Result of
        {error, Error} -> {error, Error};
        {ok, Listeners} ->
            {AliveListeners, DeadListeners} = check_alive_listeners(Listeners),
            lists:foreach(fun unlink_listener/1, DeadListeners),
            {ok, AliveListeners}
    end.

-spec add_listener_monitor(binary(), pid(), node()) -> ok | {error, channel_not_found}.
add_listener_monitor(ChannelId, Pid, Node) ->
    Transaction = fun() ->
                          case mnesia:read(?LIVE_CHANNELS_TABLE, ChannelId) of
                              [] -> {error, channel_not_found};
                              _ -> mnesia:write(?MONITORS_TABLE
                                               , #monitors_table_entry
                                                { live_channel_id=ChannelId
                                                , pid=Pid
                                                , node=Node
                                                }
                                               , write)
                          end
                  end,

    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

%%====================================================================
%% Internal functions
%%====================================================================
-spec get_monitors_on_channel(binary()) -> {ok, [#monitors_table_entry{}]}
                                                | {error, channel_not_found}.
get_monitors_on_channel(ChannelId) ->
    MatchHead = #monitors_table_entry{ live_channel_id='$1'
                                      , pid='$2'
                                      , node='$3'
                                      },
    %% Check that neither the id, username or email matches another
    Guard = {'==', '$1', ChannelId},
    ResultColumn = '$_',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          case mnesia:read(?LIVE_CHANNELS_TABLE, ChannelId) of
                              [] -> {error, channel_not_found};
                              _ -> {ok, mnesia:select(?MONITORS_TABLE, Matcher)}
                          end
                  end,

    {atomic, Result} = mnesia:transaction(Transaction),

    case Result of
        {error, Error} -> {error, Error};
        {ok, Monitors} ->
            {AliveMonitors, DeadMonitors} = check_alive_monitors(Monitors),
            lists:foreach(fun unlink_monitor/1, DeadMonitors),
            {ok, AliveMonitors}
    end.


-spec unlink_listener(#listeners_table_entry{}) -> ok.
unlink_listener(Entry) ->
    mnesia:dirty_delete(?LISTENERS_TABLE, Entry).

-spec unlink_monitor(#monitors_table_entry{}) -> ok.
unlink_monitor(Entry) ->
    mnesia:dirty_delete(?MONITORS_TABLE, Entry).

-spec check_alive_listeners([#listeners_table_entry{}]) -> { [#listeners_table_entry{}]
                                                           , [#listeners_table_entry{}]
                                                           }.
check_alive_listeners(Connections) ->
    lists:partition(fun(#listeners_table_entry{pid=Pid, node=Node }) ->
                            automate_coordination_utils:is_process_alive(Pid, Node)
                    end,
                    Connections).

-spec check_alive_monitors([#monitors_table_entry{}]) -> { [#monitors_table_entry{}]
                                                           , [#monitors_table_entry{}]
                                                           }.
check_alive_monitors(Connections) ->
    lists:partition(fun(#monitors_table_entry{pid=Pid, node=Node }) ->
                            automate_coordination_utils:is_process_alive(Pid, Node)
                    end,
                    Connections).
