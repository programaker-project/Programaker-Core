-record(chat_handler_module_entry, { chat_prefix_id        :: any
                                   , handler_module :: module()
                                   }).

-type chat_prefix() :: atom().
-record(chat_entry, { chat_id :: {chat_prefix(), binary()}
                    , chat_name :: binary()
                    }).
