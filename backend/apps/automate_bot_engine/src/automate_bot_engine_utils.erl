-module(automate_bot_engine_utils).

-export([ get_block_id/1
        , get_block_key_subkey/1
        , get_subkey_value/1
        ]).

-include("instructions.hrl").

get_block_id(#{ ?BLOCK_ID := BlockId }) ->
    BlockId;
get_block_id(_) ->
    none.

get_block_key_subkey(#{ <<"key">> := Key
                      , <<"subkey">> := #{ <<"type">> := <<"constant">>
                                         , <<"value">> := SubKey
                                         }
                      }) ->
    { key_and_subkey, Key, SubKey };
get_block_key_subkey(#{ <<"key">> := Key }) ->
    {key, Key};
get_block_key_subkey(_) ->
    { not_found }.

get_subkey_value(#{ <<"subkey">> := SubKey }) when is_binary(SubKey) ->
    {ok, SubKey};
get_subkey_value(_) ->
    {error, not_found}.
