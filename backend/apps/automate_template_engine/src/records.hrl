-include("../../automate_common_types/src/types.hrl").

-record(template_entry, { id    :: binary() | ?MNESIA_SELECTOR
                        , name  :: binary() | ?MNESIA_SELECTOR
                        , owner :: binary() | ?MNESIA_SELECTOR %% User id
                        , content :: [any()] | ?MNESIA_SELECTOR
                        }).
