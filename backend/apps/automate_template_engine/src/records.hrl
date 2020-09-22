-include("../../automate_common_types/src/types.hrl").

-record(template_entry, { id    :: binary()   | ?MNESIA_SELECTOR
                        , name  :: binary()   | ?MNESIA_SELECTOR
                        , owner :: owner_id() | ?OWNER_ID_MNESIA_SELECTOR
                        , content :: [any()]  | ?MNESIA_SELECTOR
                        }).
