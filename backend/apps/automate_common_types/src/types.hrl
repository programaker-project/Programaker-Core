-ifndef(MNESIA_SELECTOR).
-define(MNESIA_SELECTOR, '_' | '$1' | '$2' | '$3' | '$4').
-endif.

-ifndef(COMMON_TYPES).
-type user_id() :: binary().
-type group_id() :: binary().

-type owner_id() :: { user, user_id() } | { group, group_id() }.

-define(COMMON_TYPES, defined).
-endif.
