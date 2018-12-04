%%% @doc
%%% Automate channel engine tests.
%%% @end

-module(automate_channel_engine_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").

-define(TEST_NODES, [node()]).
-define(RECEIVE_TIMEOUT, 100).
-define(FAST_RECEIVE_TIMEOUT, 10).

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
    , {"[Housekeeping] Register on on a channel, then close", fun register_and_close/0}
    ].


%%%% Channel creation
%% Initialization
channel_creation_different_names() ->
    {ok, ChannelId1} = automate_channel_engine:create_channel(),
    {ok, ChannelId2} = automate_channel_engine:create_channel(),
    true = (ChannelId1 =/= ChannelId2).

%%%% Message sending
simple_listen_send() ->
    {ok, ChannelId} = automate_channel_engine:create_channel(),
    Message = simple_message,
    process_flag(trap_exit, true),

    ok = automate_channel_engine:listen_channel(ChannelId, self()),
    Pid = spawn_link(fun () ->
                             automate_channel_engine:send_to_channel(ChannelId, Message)
                     end),
    receive
        {channel_engine, ChannelId, ReceivedMessage} ->
            Message = ReceivedMessage;
        {'EXIT', _, Reason} ->
            ct:fail(wat)
    after 1000 ->
            ct:fail(timeout2)
    end.

simple_double_listen_send() ->
    {ok, ChannelId} = automate_channel_engine:create_channel(),
    Message = simple_message,

    ok = automate_channel_engine:listen_channel(ChannelId, self()),
    ok = automate_channel_engine:listen_channel(ChannelId, self()),
    spawn(fun () ->
                  automate_channel_engine:send_to_channel(ChannelId, Message)
          end),
    receive {channel_engine, ChannelId, ReceivedMessage} ->
            Message = ReceivedMessage
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end,

    %% Even if we register twice, we should only receive it once?

    receive {channel_engine, ChannelId, _} ->
            ct:fail(timeout)
    after ?FAST_RECEIVE_TIMEOUT ->
            ok
    end.


%%%% Housekeeping
register_and_close() ->
    {ok, ChannelId} = automate_channel_engine:create_channel(),
    process_flag(trap_exit, true),

    Pid = spawn_link(fun() ->
                             ok = automate_channel_engine:listen_channel(ChannelId, self())
               end),

    receive {'EXIT', Pid, Reason} ->
            ok
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end,

    %% We use a ping to check that the channel_engine also processed the
    %% spawned process 'EXIT'
    automate_channel_engine:ping(),

    Result = automate_channel_engine_mnesia_backend:get_listeners_on_channel(ChannelId),
    {ok, []} = Result.
