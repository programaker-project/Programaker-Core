%%%-------------------------------------------------------------------
%% @doc automate_configuration module
%% @end
%%%-------------------------------------------------------------------

-module(automate_configuration).

-export([ get_table_wait_time/0
        , get_program_logs_watermarks/0
        , get_frontend_root_url/0
        , get_sync_peers/0
        , get_sync_primary/0
        , is_node_primary/1
        , asset_directory/1
        ]).

-define(APPLICATION, automate).
-define(DEFAULT_WAIT_TIME, 10000).
-define(DEFAULT_USER_PROGRAM_LOGS_LOW_WATERMARK, 1000).
-define(DEFAULT_USER_PROGRAM_LOGS_HIGH_WATERMARK, 2000).

-define(SYNC_PRIMARY_ENV_VARIABLE, "AUTOMATE_SYNC_PRIMARY").
-define(SYNC_PEERS_ENV_VARIABLE, "AUTOMATE_SYNC_PEERS").
-define(SYNC_PEERS_SPLIT_TOKEN, ",").

%%====================================================================
%% Utils functions
%%====================================================================
-spec get_table_wait_time() -> non_neg_integer().
get_table_wait_time() ->
    application:get_env(?APPLICATION, table_wait_time, ?DEFAULT_WAIT_TIME).

-spec get_program_logs_watermarks() -> {non_neg_integer(), non_neg_integer()}.
get_program_logs_watermarks() ->
    LowWatermark = application:get_env(?APPLICATION, user_program_logs_count_low_watermark, ?DEFAULT_USER_PROGRAM_LOGS_LOW_WATERMARK),
    HighWatermark = application:get_env(?APPLICATION, user_program_logs_count_high_watermark, ?DEFAULT_USER_PROGRAM_LOGS_HIGH_WATERMARK),
    case LowWatermark =< HighWatermark of
        true -> {LowWatermark, HighWatermark};
        false ->
            erlang:halt("'user_program_logs_count_low_watermark' must be =< 'user_program_logs_count_high_watermark'"
                       , [{flush, false}])
    end.

-spec get_frontend_root_url() -> binary().
get_frontend_root_url() ->
    application:get_env(?APPLICATION, frontend_root_url, <<"/">>).

-spec get_sync_peers() -> [node()].
get_sync_peers() ->
    case os:getenv(?SYNC_PEERS_ENV_VARIABLE) of
        false ->
            [node()];
        Value -> %% peer1@node1,peer2@node2
            lists:filtermap(fun (Candidate) ->
                                    case string:trim(Candidate) of
                                        "" -> false;
                                        Node -> {true, list_to_atom(Node)}
                                    end
                            end,
                            string:split(Value, ?SYNC_PEERS_SPLIT_TOKEN, all))
    end.

-spec get_sync_primary() -> node() | none.
get_sync_primary() ->
    case os:getenv(?SYNC_PRIMARY_ENV_VARIABLE) of
        false ->
            node();
        Value ->
            list_to_atom(Value)
    end.

-spec is_node_primary(node()) -> boolean().
is_node_primary(Node) ->
    case {node(), automate_configuration:get_sync_primary()} of
        {Node, Node} -> true; %% Is primary
        {_, none} -> true;    %% Primary not specified
        _ -> false            %% Another is specified as primary
    end.


-spec asset_directory(string() | binary()) -> binary().
asset_directory(SubDir) ->
    BaseDirectory = case application:get_env(?APPLICATION, asset_directory) of
                         undefined ->
                             code:lib_dir(automate_configuration, priv) ++ "/assets";
                         {ok, Value} ->
                             Value
                     end,
    binary:list_to_bin(
      lists:flatten(io_lib:format("~s/~s", [BaseDirectory, SubDir]))).
