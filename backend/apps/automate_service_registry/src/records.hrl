-define(PRIMITIVE_TYPES, boolean() | binary() | number()).
-include("../../automate_storage/src/records.hrl").

-type call_type() :: 'string' | 'positive' | 'integer' | 'list' | 'boolean'.

-record(parameter, { name :: binary()
                   , type :: term
                   , default_value :: (?PRIMITIVE_TYPES | [?PRIMITIVE_TYPES])
                   }).

-record(service_action, { name :: binary()
                        , function :: fun()
                        , parameters :: [#parameter{}]
                        }).

-record(services_table_entry, { id :: binary()          | ?MNESIA_SELECTOR
                              , public :: boolean()     | ?MNESIA_SELECTOR
                              , name :: binary()        | ?MNESIA_SELECTOR
                              , description :: binary() | ?MNESIA_SELECTOR
                              , module :: module() | {module(), [_]} | ?MNESIA_SELECTOR
                              }).

-type service_entry() :: #{ name := binary()
                          , description := binary()
                          , module := module() | {module(), [_]}
                          }.
-type service_info_map() :: #{ binary() := service_entry() }.

-record(user_service_allowance_entry, { service_id :: binary() | ?MNESIA_SELECTOR
                                      , owner :: owner_id()    | ?OWNER_ID_MNESIA_SELECTOR
                                      }).

-record(services_configuration_entry, { configuration_id :: { binary(), atom() } %% Service id, property
                                      , value :: any()
                                      }).
