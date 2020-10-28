-include("../../automate_common_types/src/types.hrl").

-record(service_port_entry, { id    :: binary() | ?MNESIA_SELECTOR
                            , name  :: binary() | ?MNESIA_SELECTOR
                            , owner :: owner_id() | ?OWNER_ID_MNESIA_SELECTOR
                            , old_skip_authentication :: boolean() | ?MNESIA_SELECTOR
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

-record(service_port_block_collection_argument, { name :: binary()
                                                }).

-type service_port_block_argument() :: #service_port_block_static_argument{}
                                     | #service_port_block_dynamic_argument{}
                                     | #service_port_block_collection_argument{} .

-type block_save_to() :: null | #{ binary() => any()}.
-type block_subkey() :: null | #{ binary() => any()}.

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
                                    , subkey :: block_subkey()
                                    }).

-type supported_icon_hashes() :: 'sha256'.

-type supported_icon_type() :: {url, binary()}
                             | {hash, supported_icon_hashes(), binary() }.

-record(service_port_configuration, { id :: binary() | ?MNESIA_SELECTOR %% Service port Id
                                    , service_name :: binary() | ?MNESIA_SELECTOR
                                    , service_id :: binary() | 'undefined' | ?MNESIA_SELECTOR
                                    , is_public :: boolean() | ?MNESIA_SELECTOR
                                    , blocks :: [#service_port_block{}] | ?MNESIA_SELECTOR
                                    , icon :: undefined | supported_icon_type() | ?MNESIA_SELECTOR
                                    , allow_multiple_connections :: boolean() | ?MNESIA_SELECTOR
                                    , resources :: [binary()] | ?MNESIA_SELECTOR
                                    }).

-record(service_port_entry_extra, { id    :: binary()
                                  , name  :: binary()
                                  , owner :: owner_id()
                                                  % ↓ Extra data
                                  , is_connected :: boolean()
                                  , icon :: supported_icon_type()
                                  }).



-record(service_port_metadata, { id    :: binary()   | ?MNESIA_SELECTOR
                               , name  :: binary()   | ?MNESIA_SELECTOR
                               , owner :: owner_id() | ?OWNER_ID_MNESIA_SELECTOR
                               , icon :: undefined   | supported_icon_type() | ?MNESIA_SELECTOR
                               }).

-record(bridge_connection_entry, { id :: binary() | ?MNESIA_SELECTOR %% Bridge id
                                 , pid :: pid()   | ?MNESIA_SELECTOR %% Connection pid
                                 , node :: atom() | ?MNESIA_SELECTOR %% node() %% Node where the connection pid lives
                                 }).

-record(on_flight_message_entry, { message_id :: binary()
                                 , pid :: pid() %% Asker pid. Process that asked the bridge.
                                 , node :: atom() %% node() %% Node where the "asker" pid lives
                                 }).


-record(channel_monitor_table_entry, { bridge_id :: binary() | ?MNESIA_SELECTOR
                                     , pid :: pid()          | ?MNESIA_SELECTOR
                                     , node :: node()        | ?MNESIA_SELECTOR
                                     }).

-record(user_to_bridge_connection_entry, { id :: binary() | ?MNESIA_SELECTOR
                                         , bridge_id :: binary() | ?MNESIA_SELECTOR
                                         , owner :: owner_id()   | ?MNESIA_SELECTOR | ?OWNER_ID_MNESIA_SELECTOR
                                         , channel_id :: binary() | ?MNESIA_SELECTOR
                                         , name :: binary() | undefined | ?MNESIA_SELECTOR
                                         , creation_time :: non_neg_integer() | ?MNESIA_SELECTOR
                                         , save_signals :: boolean() | ?MNESIA_SELECTOR
                                         }).

-record(user_to_bridge_pending_connection_entry, { id :: binary() | ?MNESIA_SELECTOR
                                                 , bridge_id :: binary() | ?MNESIA_SELECTOR
                                                 , owner :: owner_id() | ?OWNER_ID_MNESIA_SELECTOR
                                                 , channel_id :: binary() | ?MNESIA_SELECTOR
                                                 , creation_time :: non_neg_integer() | ?MNESIA_SELECTOR
                                                 }).

-record(bridge_resource_share_entry, { connection_id :: binary() | ?MNESIA_SELECTOR
                                     , resource      :: binary() | ?MNESIA_SELECTOR
                                     , value         :: binary() | ?MNESIA_SELECTOR
                                     , name          :: binary() | ?MNESIA_SELECTOR
                                     , shared_with   :: owner_id() | ?OWNER_ID_MNESIA_SELECTOR
                                     }).

-record(bridge_token_entry, { token_key  :: binary() | ?MNESIA_SELECTOR
                            , token_name :: binary() | ?MNESIA_SELECTOR
                            , bridge_id  :: binary() | ?MNESIA_SELECTOR
                            , creation_time        :: non_neg_integer() | ?MNESIA_SELECTOR
                            , expiration_time      :: non_neg_integer() | undefined | ?MNESIA_SELECTOR
                            , last_connection_time :: non_neg_integer() | undefined | ?MNESIA_SELECTOR
                            }).
