%%%-------------------------------------------------------------------
%% @doc automate_channel_engine public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_channel_engine).

-export([create_channel/0, listen_channel/2, send_to_channel/2]).
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

-spec listen_channel(binary(), fun((_) -> ok)) -> ok | {error, channel_not_found}.
listen_channel(ChannelId, Callback) ->
    automate_channel_engine_mnesia_backend:add_listener_to_channel(ChannelId, Callback).

-spec send_to_channel(binary(), any()) -> ok.
send_to_channel(ChannelId, Message) ->
    Listeners = automate_channel_engine_mnesia_backend:get_listeners_on_channel(ChannelId),
    [Callback(Message) || Callback <- Listeners].

%%====================================================================
%% Internal functions
%%====================================================================

generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).
