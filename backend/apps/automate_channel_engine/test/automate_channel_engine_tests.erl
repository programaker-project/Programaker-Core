%%% @doc
%%% Automate channel engine tests.
%%% @end

-module(automate_channel_engine_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").

-define(TEST_NODES, [node()]).
-define(RECEIVE_TIMEOUT, 100).

%%====================================================================
%% Test API
%%====================================================================

session_manager_test_() ->
    {setup
    , fun setup/0
    , fun stop/1
    , fun tests/1
    }.

%% @doc App infrastructure setup.
%% @end
setup() ->
    NodeName = node(),

    %% Use a custom node name to avoid overwriting the actual databases
    net_kernel:start([?MODULE, shortnames]),

    {ok, Pid} = application:ensure_all_started(automate_channel_engine),

    {NodeName, Pid}.

%% @doc App infrastructure teardown.
%% @end
stop({NodeName, _Pid}) ->
    application:stop(automate_channel_engine),
    mnesia:stop(),
    ok = mnesia:delete_schema(?TEST_NODES),

    %% %% Restore the original node name
    net_kernel:start([NodeName, shortnames]),
    ok.

tests(_SetupResult) ->
    [ {"[Channel creation] Create two channels, IDs are different", fun channel_creation_different_names/0}
    , {"[Message sending] Register on a channel, message it", fun simple_listen_send/0}
    , {"[Message sending] Register twice on a channel, message it", fun simple_double_listen_send/0}
    ].


%%%% Channel creation
%% Initialization
channel_creation_different_names() ->
    {ok, ChannelId1} = automate_channel_engine:create_channel(),
    {ok, ChannelId2} = automate_channel_engine:create_channel(),
    true = (ChannelId1 =/= ChannelId2).

simple_listen_send() ->
    {ok, ChannelId} = automate_channel_engine:create_channel(),
    Message = simple_message,

    Pid = self(),
    automate_channel_engine:listen_channel(ChannelId,
                                           fun (Msg) ->
                                                   Pid ! { channel_msg, Msg }
                                           end),
    spawn(fun () ->
                  automate_channel_engine:send_to_channel(ChannelId, Message)
          end),
    receive {channel_msg, ReceivedMessage} ->
            Message = ReceivedMessage
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end.

simple_double_listen_send() ->
    {ok, ChannelId} = automate_channel_engine:create_channel(),
    Message = simple_message,

    Pid = self(),
    automate_channel_engine:listen_channel(ChannelId,
                                           fun (Msg) ->
                                                   Pid ! { channel_msg, Msg }
                                           end),

    automate_channel_engine:listen_channel(ChannelId,
                                           fun (Msg) ->
                                                   Pid ! { channel_msg2, Msg }
                                           end),
    spawn(fun () ->
                  automate_channel_engine:send_to_channel(ChannelId, Message)
          end),
    receive {channel_msg, ReceivedMessage} ->
            Message = ReceivedMessage
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end,
    receive {channel_msg2, ReceivedMessage2} ->
            Message = ReceivedMessage2
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end.
