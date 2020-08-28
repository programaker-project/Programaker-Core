-ifndef(MNESIA_SELECTOR).
-define(MNESIA_SELECTOR, '_' | '$1' | '$2' | '$3' | '$4').
-endif.

-ifndef(COMMON_TYPES).

%% Defining user_id() and group_id() led to error before.
%% Better to just use owner_id().
-type owner_id() :: { user, binary() } | { group, binary() }.

-define(COMMON_TYPES, defined).

-define(OWNER_ID_MNESIA_SELECTOR, { user, ?MNESIA_SELECTOR } | { group, ?MNESIA_SELECTOR } | { ?MNESIA_SELECTOR, ?MNESIA_SELECTOR }).
-endif.
