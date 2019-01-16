-record(service_port_entry, { id    :: binary()
                            , name  :: binary()
                            , owner :: binary() %% User id
                            , service_id :: binary()
                            }).