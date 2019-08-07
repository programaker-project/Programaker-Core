-include("../../automate_common_types/src/types.hrl").

-record(service_port_entry, { id    :: binary() | ?MNESIA_SELECTOR
                            , name  :: binary() | ?MNESIA_SELECTOR
                            , owner :: binary() | ?MNESIA_SELECTOR %% User id
                            , service_id :: binary() | 'undefined' | ?MNESIA_SELECTOR
                            }).

-record(service_port_entry_extra, { id    :: binary()
                                  , name  :: binary()
                                  , owner :: binary() %% User id
                                  , service_id :: binary() | 'undefined'
                                                % ↓ Extra data
                                  , is_connected :: boolean()
                                  }).

-type service_port_block_argument_type() :: binary(). %% <<"string">>
                                          %% | <<"integer">>
                                          %% | <<"float">>
                                          %% | <<"boolean">>
                                          %%   .

-record(service_port_block_static_argument, { type :: service_port_block_argument_type()
                                            , default :: binary() | 'undefined'
                                            , class :: binary() | 'undefined'
                                            }).

-record(service_port_block_dynamic_argument, { type :: service_port_block_argument_type()
                                             , callback :: binary()
                                             }).

-type service_port_block_argument() :: #service_port_block_static_argument{}
                                     | #service_port_block_dynamic_argument{}.

-type block_save_to() :: null | #{ binary() => any()}.

-record(service_port_block, { block_id :: binary()
                            , function_name :: binary()
                            , message :: binary()
                            , arguments :: [service_port_block_argument()]
                            , block_type :: binary()
                            , block_result_type :: binary()
                            , save_to :: block_save_to()
                            }).

-type service_port_trigger_expected_value() :: null | #{ binary() => any()}.

-record(service_port_trigger_block, { block_id :: binary()
                                    , function_name :: binary()
                                    , message :: binary()
                                    , arguments :: [service_port_block_argument()]
                                    , block_type :: binary()
                                    , save_to :: block_save_to()
                                    , expected_value :: service_port_trigger_expected_value()
                                    , key :: binary()
                                    }).

-record(service_port_configuration, { id :: binary() | ?MNESIA_SELECTOR %% Service port Id
                                    , service_name :: binary() | ?MNESIA_SELECTOR
                                    , service_id :: binary() | 'undefined' | ?MNESIA_SELECTOR
                                    , is_public :: boolean() | ?MNESIA_SELECTOR
                                    , blocks :: [#service_port_block{}] | ?MNESIA_SELECTOR
                                    }).


-record(service_port_user_obfuscation_entry, { id :: { binary() | ?MNESIA_SELECTOR  %% internal id
                                                     , binary() | ?MNESIA_SELECTOR  %% bridge id
                                                     }
                                             , obfuscated_id :: binary() | ?MNESIA_SELECTOR
                                             }).


-record(service_port_monitor_channel_entry, { id :: { binary() | ?MNESIA_SELECTOR  %% user id
                                                    , binary() | ?MNESIA_SELECTOR  %% bridge id
                                                    } | ?MNESIA_SELECTOR
                                            , channel_id :: binary() | ?MNESIA_SELECTOR
                                            }).

-record(bridge_connection_entry, { id :: binary() %% Bridge id
                                 , pid :: pid() %% Connection pid
                                 , node :: atom() %% node() %% Node where the connection pid lives
                                 }).

-record(on_flight_message_entry, { message_id :: binary()
                                 , pid :: pid() %% Asker pid. Process that asked the bridge.
                                 , node :: atom() %% node() %% Node where the "asker" pid lives
                                 }).
