%%% Automate bot engine getters tests.
%%% @end

-module(automate_bot_engine_signal_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").
-include("../src/instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").

%% Test data
-include("single_line_program.hrl").

-define(APPLICATION, automate_bot_engine).
-define(WAIT_PER_INSTRUCTION, 100).  %% Milliseconds
%% Note, if waiting per instruction takes too much time consider adding a method
%% which checks periodically.
-define(UTILS, automate_bot_engine_test_utils).
-define(BRIDGE_UTILS, automate_service_port_engine_test_utils).

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
    net_kernel:start([testing, shortnames]),

    {ok, _} = application:ensure_all_started(?APPLICATION),
    {ok, _} = application:ensure_all_started(automate_service_port_engine),
    {ok, _} = application:ensure_all_started(automate_services_time),

    {NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({_NodeName}) ->
    ok = application:stop(automate_services_time),
    ok = application:stop(automate_service_port_engine),
    ok = application:stop(?APPLICATION),

    ok.


tests(_SetupResult) ->
    %% Operations
    %% Lists
    [ {"[Bot engine][Signal management] Wait for signal operation", fun simple_wait_for_signal/0}
    , {"[Bot engine][Signal management] Wait for signal, check key", fun wait_for_signal_check_key/0}
    , {"[Bot engine][Signal management] Wait for signal, check subkey", fun wait_for_signal_check_subkey/0}
    , {"[Bot engine][Signal management] Wait for variable operation", fun simple_wait_for_variable/0}
    , {"[Bot engine][Signal management] Wait for monitor signal", fun wait_for_monitor_signal/0}
    , {"[Bot engine][Signal management] Wait for monitor signal, check key", fun wait_for_monitor_signal_check_key/0}
    , {"[Bot engine][Signal management] Wait for time signal", fun wait_for_time_signal/0}
    ].

%%%% Operations
simple_wait_for_signal() ->
    Prefix = erlang:atom_to_list(?MODULE),
    OwnerUserId = {user, iolist_to_binary([Prefix,"-test-1-owner"])},
    ServicePortName = iolist_to_binary([Prefix, "-test-1-service-port"]),

    {ok, ServicePortId} = automate_service_port_engine:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },
    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                                      , <<"value">> => Configuration
                                                                      })),
    {ok, ConnectionId} = ?BRIDGE_UTILS:establish_connection(ServicePortId, OwnerUserId),

    {ok, ProgramId} = ?UTILS:create_user_program(OwnerUserId),
    Thread = #program_thread{ position = [1]
                            , program=?UTILS:build_ast([ { ?COMMAND_LOG_VALUE, [constant_val(<<"before">>)] }
                                                       , { ?COMMAND_WAIT_FOR_NEXT_VALUE,
                                                           [ ?UTILS:block_val({ iolist_to_binary([ "services."
                                                                                                 , ServicePortId
                                                                                                 , ".on_new_message"
                                                                                                 ])
                                                                              })
                                                           ]
                                                         }
                                                       , { ?COMMAND_LOG_VALUE, [constant_val(<<"after">>)] }
                                                       ])
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    %% Launch
    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),

    %% Check logs before sending signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsBefore} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsBefore = [ M || #user_generated_log_entry{event_message=M} <- LogsBefore ],

    io:fwrite("Logs before signal: ~p~n", [MsgsBefore]),
    ?assertMatch([ <<"before">> ], MsgsBefore),

    %% Send signal
    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        jiffy:encode(#{ <<"type">> => <<"NOTIFICATION">>
                                                                      , <<"key">> => <<"on_new_message">>
                                                                      , <<"to_user">> => null
                                                                      , <<"value">> => <<"sample value">>
                                                                      , <<"content">> => <<"sample content">>
                                                                      })),

    %% Check logs after sending signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsAfter} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsAfter = [ M || #user_generated_log_entry{event_message=M} <- LogsAfter ],

    io:fwrite("Logs after signal: ~p~n", [MsgsAfter]),
    ?assertMatch([ <<"before">>, <<"after">>], MsgsAfter),
    ok.

wait_for_signal_check_key() ->
    Prefix = erlang:atom_to_list(?MODULE),
    OwnerUserId = {user, iolist_to_binary([Prefix,"-test-1-owner"])},
    ServicePortName = iolist_to_binary([Prefix, "-test-1-service-port"]),

    {ok, ServicePortId} = automate_service_port_engine:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },
    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                                      , <<"value">> => Configuration
                                                                      })),
    {ok, ConnectionId} = ?BRIDGE_UTILS:establish_connection(ServicePortId, OwnerUserId),

    {ok, ProgramId} = ?UTILS:create_user_program(OwnerUserId),
    Thread = #program_thread{ position = [1]
                            , program=?UTILS:build_ast([ { ?COMMAND_LOG_VALUE, [constant_val(<<"before">>)] }
                                                       , { ?COMMAND_WAIT_FOR_NEXT_VALUE,
                                                           [ ?UTILS:block_val({ iolist_to_binary([ "services."
                                                                                                 , ServicePortId
                                                                                                 , ".on_new_message"
                                                                                                 ])
                                                                              })
                                                           ]
                                                         }
                                                       , { ?COMMAND_LOG_VALUE, [constant_val(<<"after">>)] }
                                                       ])
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    %% Launch
    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),

    %% Check logs before sending signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsBefore} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsBefore = [ M || #user_generated_log_entry{event_message=M} <- LogsBefore ],

    io:fwrite("Logs before signal: ~p~n", [MsgsBefore]),
    ?assertMatch([ <<"before">> ], MsgsBefore),

    %% Send different signal
    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        jiffy:encode(#{ <<"type">> => <<"NOTIFICATION">>
                                                                      , <<"key">> => <<"another key">>
                                                                      , <<"to_user">> => null
                                                                      , <<"value">> => <<"sample value">>
                                                                      , <<"content">> => <<"sample content">>
                                                                      })),

    %% Check logs after different signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsNonWaited} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsNonWaited = [ M || #user_generated_log_entry{event_message=M} <- LogsNonWaited ],

    io:fwrite("Logs after non-waited signal: ~p~n", [MsgsNonWaited]),
    ?assertMatch([ <<"before">> ], MsgsNonWaited),

    %% Send correct signal
    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        jiffy:encode(#{ <<"type">> => <<"NOTIFICATION">>
                                                                      , <<"key">> => <<"on_new_message">>
                                                                      , <<"to_user">> => null
                                                                      , <<"value">> => <<"sample value">>
                                                                      , <<"content">> => <<"sample content">>
                                                                      })),

    %% Check logs after sending signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsAfter} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsAfter = [ M || #user_generated_log_entry{event_message=M} <- LogsAfter ],

    io:fwrite("Logs after signal: ~p~n", [MsgsAfter]),
    ?assertMatch([ <<"before">>, <<"after">>], MsgsAfter),
    ok.

wait_for_signal_check_subkey() ->
    Prefix = erlang:atom_to_list(?MODULE),
    OwnerUserId = {user, iolist_to_binary([Prefix,"-test-1-owner"])},
    ServicePortName = iolist_to_binary([Prefix, "-test-1-service-port"]),

    {ok, ServicePortId} = automate_service_port_engine:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },
    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                                      , <<"value">> => Configuration
                                                                      })),
    {ok, ConnectionId} = ?BRIDGE_UTILS:establish_connection(ServicePortId, OwnerUserId),

    {ok, ProgramId} = ?UTILS:create_user_program(OwnerUserId),
    Thread = #program_thread{ position = [1]
                            , program=?UTILS:build_ast([ { ?COMMAND_LOG_VALUE, [constant_val(<<"before">>)] }
                                                       , { ?COMMAND_WAIT_FOR_NEXT_VALUE,
                                                           [ ?UTILS:block_val({ iolist_to_binary([ "services."
                                                                                                 , ServicePortId
                                                                                                 , ".on_new_message"
                                                                                                 ])
                                                                              , #{ <<"key">> => <<"on_new_message">>
                                                                                 , <<"subkey">> => #{ ?TYPE => ?VARIABLE_CONSTANT
                                                                                                    , ?VALUE => <<"correct">>
                                                                                                    }
                                                                                 }
                                                                              })
                                                           ]
                                                         }
                                                       , { ?COMMAND_LOG_VALUE, [constant_val(<<"after">>)] }
                                                       ])
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    %% Launch
    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),

    %% Check logs before sending signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsBefore} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsBefore = [ M || #user_generated_log_entry{event_message=M} <- LogsBefore ],

    io:fwrite("Logs before signal: ~p~n", [MsgsBefore]),
    ?assertMatch([ <<"before">> ], MsgsBefore),

    %% Send different signal
    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        jiffy:encode(#{ <<"type">> => <<"NOTIFICATION">>
                                                                      , <<"key">> => <<"on_new_message">>
                                                                      , <<"subkey">> => <<"different">>
                                                                      , <<"to_user">> => null
                                                                      , <<"value">> => <<"sample value">>
                                                                      , <<"content">> => <<"sample content">>
                                                                      })),

    %% Check logs after different signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsNonWaited} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsNonWaited = [ M || #user_generated_log_entry{event_message=M} <- LogsNonWaited ],

    io:fwrite("Logs after non-waited signal: ~p~n", [MsgsNonWaited]),
    ?assertMatch([ <<"before">> ], MsgsNonWaited),

    %% Send correct signal
    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        jiffy:encode(#{ <<"type">> => <<"NOTIFICATION">>
                                                                      , <<"key">> => <<"on_new_message">>
                                                                      , <<"subkey">> => <<"correct">>
                                                                      , <<"to_user">> => null
                                                                      , <<"value">> => <<"sample value">>
                                                                      , <<"content">> => <<"sample content">>
                                                                      })),

    %% Check logs after sending signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsAfter} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsAfter = [ M || #user_generated_log_entry{event_message=M} <- LogsAfter ],

    io:fwrite("Logs after signal: ~p~n", [MsgsAfter]),
    ?assertMatch([ <<"before">>, <<"after">>], MsgsAfter),
    ok.

simple_wait_for_variable() ->
    {_Username, _ProgramName, ProgramId} = ?UTILS:create_anonymous_program(),
    Thread = #program_thread{ position = [1]
                            , program=?UTILS:build_ast([ { ?COMMAND_LOG_VALUE, [constant_val(<<"before">>)] }
                                                       , { ?COMMAND_WAIT_FOR_NEXT_VALUE,
                                                           [ #{ ?TYPE => ?VARIABLE_VARIABLE
                                                              , ?VALUE => <<"checkpoint">>
                                                              }
                                                           ]
                                                         }
                                                       , { ?COMMAND_LOG_VALUE, [constant_val(<<"after">>)] }
                                                       ])
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },
    {ok, Thread2} = automate_bot_engine_variables:set_program_variable(Thread,  <<"checkpoint">>, false),

    %% Launch
    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread2),

    %% Check logs before sending signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsBefore} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsBefore = [ M || #user_generated_log_entry{event_message=M} <- LogsBefore ],

    io:fwrite("Logs before signal: ~p~n", [MsgsBefore]),
    ?assertMatch([ <<"before">> ], MsgsBefore),

    %% Update variable
    {ok, _Thread3} = automate_bot_engine_variables:set_program_variable(Thread2,  <<"checkpoint">>, true),

    %% Check logs after sending signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsAfter} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsAfter = [ M || #user_generated_log_entry{event_message=M} <- LogsAfter ],

    io:fwrite("Logs after signal: ~p~n", [MsgsAfter]),
    ?assertMatch([ <<"before">>, <<"after">>], MsgsAfter),
    ok.

wait_for_monitor_signal() ->
    Prefix = erlang:atom_to_list(?MODULE),
    OwnerUserId = {user, iolist_to_binary([Prefix,"-test-1-owner"])},
    ServicePortName = iolist_to_binary([Prefix, "-test-1-service-port"]),

    {ok, ServicePortId} = automate_service_port_engine:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },

    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                                      , <<"value">> => Configuration
                                                                      })),
    {ok, ConnectionId} = ?BRIDGE_UTILS:establish_connection(ServicePortId, OwnerUserId),

    {ok, ProgramId} = ?UTILS:create_user_program(OwnerUserId),
    Thread = #program_thread{ position = [1]
                            , program=?UTILS:build_ast([ { ?COMMAND_LOG_VALUE, [constant_val(<<"before">>)] }
                                                       , { ?COMMAND_WAIT_FOR_NEXT_VALUE,
                                                           [ ?UTILS:block_val({ ?WAIT_FOR_MONITOR
                                                                              , #{ ?FROM_SERVICE => ServicePortId }
                                                                              })
                                                           ]
                                                         }
                                                       , { ?COMMAND_LOG_VALUE, [constant_val(<<"after">>)] }
                                                       ])
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    %% Launch
    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),

    %% Check logs before sending signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsBefore} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsBefore = [ M || #user_generated_log_entry{event_message=M} <- LogsBefore ],

    io:fwrite("Logs before signal: ~p~n", [MsgsBefore]),
    ?assertMatch([ <<"before">> ], MsgsBefore),

    %% Send signal
    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        jiffy:encode(#{ <<"type">> => <<"NOTIFICATION">>
                                                                      , <<"key">> => <<"on_new_message">>
                                                                      , <<"to_user">> => null
                                                                      , <<"value">> => <<"sample value">>
                                                                      , <<"content">> => <<"sample content">>
                                                                      })),

    %% Check logs after sending signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsAfter} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsAfter = [ M || #user_generated_log_entry{event_message=M} <- LogsAfter ],

    io:fwrite("Logs after signal: ~p~n", [MsgsAfter]),
    ?assertMatch([ <<"before">>, <<"after">>], MsgsAfter),
    ok.

wait_for_monitor_signal_check_key() ->
    Prefix = erlang:atom_to_list(?MODULE),
    OwnerUserId = {user, iolist_to_binary([Prefix,"-test-1-owner"])},
    ServicePortName = iolist_to_binary([Prefix, "-test-1-service-port"]),

    {ok, ServicePortId} = automate_service_port_engine:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },

    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                                      , <<"value">> => Configuration
                                                                      })),
    {ok, ConnectionId} = ?BRIDGE_UTILS:establish_connection(ServicePortId, OwnerUserId),

    {ok, ProgramId} = ?UTILS:create_user_program(OwnerUserId),
    Thread = #program_thread{ position = [1]
                            , program=?UTILS:build_ast([ { ?COMMAND_LOG_VALUE, [constant_val(<<"before">>)] }
                                                       , { ?COMMAND_WAIT_FOR_NEXT_VALUE,
                                                           [ ?UTILS:block_val({ ?WAIT_FOR_MONITOR
                                                                              , #{ ?FROM_SERVICE => ServicePortId
                                                                                 , <<"key">> => <<"on_new_message">>
                                                                                 }
                                                                              })
                                                           ]
                                                         }
                                                       , { ?COMMAND_LOG_VALUE, [constant_val(<<"after">>)] }
                                                       ])
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    %% Launch
    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),

    %% Check logs before sending signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsBefore} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsBefore = [ M || #user_generated_log_entry{event_message=M} <- LogsBefore ],

    io:fwrite("Logs before signal: ~p~n", [MsgsBefore]),
    ?assertMatch([ <<"before">> ], MsgsBefore),

    %% Send different signal
    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        jiffy:encode(#{ <<"type">> => <<"NOTIFICATION">>
                                                                      , <<"key">> => <<"another key">>
                                                                      , <<"to_user">> => null
                                                                      , <<"value">> => <<"sample value">>
                                                                      , <<"content">> => <<"sample content">>
                                                                      })),

    %% Check logs after different signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsNonWaited} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsNonWaited = [ M || #user_generated_log_entry{event_message=M} <- LogsNonWaited ],

    io:fwrite("Logs after non-waited signal: ~p~n", [MsgsNonWaited]),
    ?assertMatch([ <<"before">> ], MsgsNonWaited),

    %% Send correct signal
    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        jiffy:encode(#{ <<"type">> => <<"NOTIFICATION">>
                                                                      , <<"key">> => <<"on_new_message">>
                                                                      , <<"to_user">> => null
                                                                      , <<"value">> => <<"sample value">>
                                                                      , <<"content">> => <<"sample content">>
                                                                      })),

    %% Check logs after sending signal
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, LogsAfter} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsAfter = [ M || #user_generated_log_entry{event_message=M} <- LogsAfter ],

    io:fwrite("Logs after signal: ~p~n", [MsgsAfter]),
    ?assertMatch([ <<"before">>, <<"after">>], MsgsAfter),
    ok.

wait_for_time_signal() ->
    {_Username, _ProgramName, ProgramId} = ?UTILS:create_anonymous_program(),
    Thread = #program_thread{ position = [1]
                            , program=?UTILS:build_ast([ { ?COMMAND_LOG_VALUE, [constant_val(<<"before">>)] }
                                                       , { ?COMMAND_WAIT_FOR_NEXT_VALUE,
                                                           [ ?UTILS:block_val({ ?WAIT_FOR_MONITOR
                                                                              , #{ ?FROM_SERVICE => automate_services_time:get_uuid()
                                                                                 , <<"key">> => <<"utc_time">>
                                                                                 }
                                                                              })
                                                           ]
                                                         }
                                                       , { ?COMMAND_LOG_VALUE, [constant_val(<<"after">>)] }
                                                       ])
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    %% Launch
    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),

    %% Wait ~2 seconds, should be enough for the time signal to arrive
    timer:sleep(3000),
    {ok, LogsAfter} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsAfter = [ M || #user_generated_log_entry{event_message=M} <- LogsAfter ],

    io:fwrite("Logs after signal: ~p~n", [MsgsAfter]),
    ?assertMatch([ <<"before">>, <<"after">>], MsgsAfter),
    ok.

%%====================================================================
%% Util functions
%%====================================================================
constant_val(Val) ->
    #{ ?TYPE => ?VARIABLE_CONSTANT
     , ?VALUE => Val
     }.
