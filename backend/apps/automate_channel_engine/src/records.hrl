-include("../../automate_common_types/src/types.hrl").

-define(CHANNEL_MESSAGE_CONTENT, <<"content">>).

-type common_channel_keys() :: undefined | ui_events | ui_events_show | variable_events.

-record(live_channels_table_entry, { live_channel_id :: binary()
                                   , stats :: [_]
                                   }).

-record(listeners_table_entry, { live_channel_id :: binary() | ?MNESIA_SELECTOR
                               , pid :: pid() | ?MNESIA_SELECTOR
                               , node :: node() | ?MNESIA_SELECTOR
                               , key :: binary() | undefined | ?MNESIA_SELECTOR
                               , subkey :: binary() | undefined | ?MNESIA_SELECTOR
                               }).

-record(monitors_table_entry, { live_channel_id :: binary() | ?MNESIA_SELECTOR
                              , pid :: pid() | ?MNESIA_SELECTOR
                              , node :: node() | ?MNESIA_SELECTOR
                              }).
