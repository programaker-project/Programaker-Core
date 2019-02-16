-record(service_port_entry, { id    :: binary()
                            , name  :: binary()
                            , owner :: binary() %% User id
                            , service_id :: binary()
                            }).

-record(service_port_entry_extra, { id    :: binary()
                                  , name  :: binary()
                                  , owner :: binary() %% User id
                                  , service_id :: binary()
%%%% Extra data --------------------------
                                  , is_connected :: boolean()
                                  }).

-type service_port_block_argument_type() :: string(). %% <<"string">>
                                          %% | <<"integer">>
                                          %% | <<"float">>
                                          %% | <<"boolean">>
                                          %%   .

-record(service_port_block_argument, { type :: service_port_block_argument_type()
                                     , default :: binary()
                                     }).

-record(service_port_block, { block_id :: binary()
                            , function_name :: binary()
                            , message :: binary()
                            , arguments :: [#service_port_block_argument{}]
                            , block_type :: binary()
                            , block_result_type :: binary()
                            }).

-record(service_port_configuration, { id :: binary() %% Service port Id
                                    , service_name :: binary()
                                    , service_id :: binary()
                                    , is_public :: boolean()
                                    , blocks :: [#service_port_block{}]
                                    }).


-record(service_port_user_obfuscation_entry, { id :: {binary(), binary()} %% { internal Id, service port Id }
                                             , obfuscated_id :: binary()
                                             }).
