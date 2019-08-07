%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_channel_engine_mnesia_backend).

-export([ start_link/0
        , register_channel/1
        , add_listener_to_channel/3
        , get_listeners_on_channel/1
        ]).

-define(LIVE_CHANNELS_TABLE, automate_channel_engine_live_channels_table).
-define(LISTENERS_TABLE, automate_channel_engine_listeners_table).
-include("records.hrl").

%%====================================================================
%% API
%%====================================================================
start_link() ->
    Nodes = automate_configuration:get_sync_peers(),
    %% Live channels table
    ok = case mnesia:create_table(?LIVE_CHANNELS_TABLE,
                                  [ { attributes, record_info(fields, live_channels_table_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, live_channels_table_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
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

-spec add_listener_to_channel(binary(), pid(), node()) -> ok | {error, channel_not_found}.
add_listener_to_channel(ChannelId, Pid, Node) ->
    Transaction = fun() ->
                          case mnesia:read(?LIVE_CHANNELS_TABLE, ChannelId) of
                              [] -> {error, channel_not_found};
                              _ -> mnesia:write(?LISTENERS_TABLE
                                               , #listeners_table_entry
                                                { live_channel_id=ChannelId
                                                , pid=Pid
                                                , node=Node
                                                }
                                               , write)
                          end
                  end,

    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

-spec get_listeners_on_channel(binary()) -> {ok, [#listeners_table_entry{}]}
                                                | {error, channel_not_found}.
get_listeners_on_channel(ChannelId) ->
    MatchHead = #listeners_table_entry{ live_channel_id='$1'
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

%%====================================================================
%% Internal functions
%%====================================================================
-spec unlink_listener(#listeners_table_entry{}) -> ok.
unlink_listener(Entry) ->
    mnesia:dirty_delete(?LISTENERS_TABLE, Entry).

-spec check_alive_listeners([#listeners_table_entry{}]) -> { [#listeners_table_entry{}]
                                                           , [#listeners_table_entry{}]
                                                           }.
check_alive_listeners(Connections) ->
    lists:partition(fun(#listeners_table_entry{pid=Pid, node=Node }) ->
                            automate_coordination_utils:is_process_alive(Pid, Node)
                    end,
                    Connections).
