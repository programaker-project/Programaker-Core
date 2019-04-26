%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_channel_engine_mnesia_backend).

-export([ start_link/0
        , register_channel/1
        , add_listener_to_channel/2
        , get_listeners_on_channel/1
        , unlink_listener/1
        ]).

-define(LIVE_CHANNELS_TABLE, automate_channel_engine_live_channels_table).
-define(LISTENERS_TABLE, automate_channel_engine_listeners_table).

-record(live_channels_table_entry, { live_channel_id :: binary()
                                   , stats :: [_]
                                   }).

-record(listeners_table_entry, { live_channel_id :: binary() | '$1' | '$2'
                               , pid :: pid() | '$1' | '$2'
                               }).


%%====================================================================
%% API
%%====================================================================
start_link() ->
    Nodes = [node()],
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

-spec add_listener_to_channel(binary(), pid()) -> ok | {error, channel_not_found}.
add_listener_to_channel(ChannelId, Pid) ->
    Transaction = fun() ->
                          case mnesia:read(?LIVE_CHANNELS_TABLE, ChannelId) of
                              [] -> {error, channel_not_found};
                              _ -> mnesia:write(?LISTENERS_TABLE
                                               , #listeners_table_entry
                                                { live_channel_id=ChannelId
                                                , pid=Pid
                                                }
                                               , write)
                          end
                  end,

    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

-spec get_listeners_on_channel(binary()) -> {ok, [pid()]} | {error, channel_not_found}.
get_listeners_on_channel(ChannelId) ->
    MatchHead = #listeners_table_entry{ live_channel_id='$1'
                                      , pid='$2'
                                      },
    %% Check that neither the id, username or email matches another
    Guard = {'==', '$1', ChannelId},
    ResultColumn = '$2',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          case mnesia:read(?LIVE_CHANNELS_TABLE, ChannelId) of
                              [] -> {error, channel_not_found};
                              _ -> {ok, mnesia:select(?LISTENERS_TABLE, Matcher)}
                          end
                  end,

    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

-spec unlink_listener(pid()) -> ok.
unlink_listener(Pid) ->
    %% @TODO Get a reverse lookup table for this purposes?
    MatchHead = #listeners_table_entry{ live_channel_id='$1'
                                      , pid='$2'
                                      },
    %% Check that neither the id, username or email matches another
    Guard = {'==', '$2', Pid},
    ResultColumn = '$1',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          on_all_select(?LISTENERS_TABLE, Matcher,
                                        fun(Record) ->
                                                mnesia:delete(?LISTENERS_TABLE, Record, write)
                                        end)
                  end,

    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

%%====================================================================
%% Internal functions
%%====================================================================

on_all_select(Tab, Matcher, Callback) ->
    Objects = mnesia:select(Tab, Matcher),
    lists:foreach(Callback, Objects).
