%%%-------------------------------------------------------------------
%% @doc automate_channel_engine public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_channel_engine).

%% Public API
-export([create_channel/0, listen_channel/1, send_to_channel/2]).
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

-spec listen_channel(binary()) -> ok | {error, channel_not_found}.
listen_channel(ChannelId) ->
    automate_channel_engine_mnesia_backend:add_listener_to_channel(ChannelId, self(), node()).

-spec send_to_channel(binary(), any()) -> ok.
send_to_channel(ChannelId, Message) ->
    automate_stats:log_observation(counter,
                                   automate_channel_engine_messages_in,
                                   [ChannelId]),
    ?LOGGING:log_event(ChannelId, Message),

    case automate_channel_engine_mnesia_backend:get_listeners_on_channel(ChannelId) of
        {ok, Listeners} ->
            %% io:format("Forwarding ~p to ~p~n", [Message, Listeners]),
            lists:foreach(fun(#listeners_table_entry{pid=Pid}) ->
                                  Pid ! {channel_engine, ChannelId, Message},
                                  automate_stats:log_observation(counter,
                                                                 automate_channel_engine_messages_out,
                                                                 [ChannelId])
                          end, Listeners),
            ok;
        X ->
            X
    end.

%%====================================================================
%% Internal functions
%%====================================================================
generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).
