-module(automate_bot_engine_utils).

-export([ get_block_id/1
        ]).

-include("instructions.hrl").

get_block_id(#{ ?BLOCK_ID := BlockId }) ->
    BlockId;
get_block_id(_) ->
    none.
