-define(CHANNEL_MESSAGE_CONTENT, <<"content">>).

-record(live_channels_table_entry, { live_channel_id :: binary()
                                   , stats :: [_]
                                   }).

-record(listeners_table_entry, { live_channel_id :: binary() | '$1' | '$2' | '$3'
                               , pid :: pid() | '$1' | '$2' | '$3'
                               , node :: node() | '$1' | '$2' | '$3'
                               , key :: binary() | undefined | '$1' | '$2' | '$3'
                               , subkey :: binary() | undefined | '$1' | '$2' | '$3'
                               }).

-record(monitors_table_entry, { live_channel_id :: binary() | '$1' | '$2' | '$3'
                              , pid :: pid() | '$1' | '$2' | '$3'
                              , node :: node() | '$1' | '$2' | '$3'
                              }).
