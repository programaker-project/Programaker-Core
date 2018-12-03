%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_channel_engine_mnesia_backend).

-export([ start_link/0
        , register_channel/1
        , add_listener_to_channel/2
        , get_listeners_on_channel/1
        ]).

-define(LIVE_CHANNELS_TABLE, automate_channel_engine_live_channels_table).
-define(LISTENERS_TABLE, automate_channel_engine_listeners_table).

-type callback() :: fun((_) -> ok).

-record(live_channels_table_entry, { live_channel_id :: binary()
                                   , stats :: [_]
                                   }).

-record(listeners_table_entry, { live_channel_id :: binary()
                               , callback :: callback()
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
    ignore.

-spec register_channel(binary()) -> ok | {error, term(), string()}.
register_channel(ChannelId) ->
    Entry = #live_channels_table_entry{live_channel_id=ChannelId, stats=7},
    io:fwrite("%%%%% ~p~n", [record_info(fields, live_channels_table_entry)]),
    Transaction = fun() ->
                          ok = mnesia:write(?LIVE_CHANNELS_TABLE, Entry, write)
                  end,

    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec add_listener_to_channel(binary(), callback()) -> ok | {error, channel_not_found}.
add_listener_to_channel(ChannelId, Callback) ->
    Transaction = fun() ->
                          case mnesia:read(?LIVE_CHANNELS_TABLE, ChannelId) of
                              [] -> {error, channel_not_found};
                              _ -> mnesia:write(?LISTENERS_TABLE
                                               , #listeners_table_entry
                                                { live_channel_id=ChannelId
                                                , callback=Callback
                                                }
                                               , write)
                          end
                  end,

    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

-spec get_listeners_on_channel(binary()) -> {ok, [callback()]} | {error, channel_not_found}.
get_listeners_on_channel(ChannelId) ->
    MatchHead = #listeners_table_entry{ live_channel_id='$1'
                                      , callback='$2'
                                      },
    %% Check that neither the id, username or email matches another
    Guard = {'==', '$1', ChannelId},
    ResultColumn = '$2',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          case mnesia:read(?LIVE_CHANNELS_TABLE, ChannelId) of
                              [] -> {error, channel_not_found};
                              _ -> mnesia:select(?LISTENERS_TABLE, Matcher)
                          end
                  end,

    {atomic, Result} = mnesia:transaction(Transaction),
    Result.


%%====================================================================
%% Internal functions
%%====================================================================
