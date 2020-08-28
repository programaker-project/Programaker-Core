-include("../../automate_common_types/src/types.hrl").

-record(template_entry, { id    :: binary()   | ?MNESIA_SELECTOR
                        , name  :: binary()   | ?MNESIA_SELECTOR
                        , owner :: owner_id() | ?MNESIA_SELECTOR
                        , content :: [any()]  | ?MNESIA_SELECTOR
                        }).
