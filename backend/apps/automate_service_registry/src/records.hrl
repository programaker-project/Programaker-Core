-define(PRIMITIVE_TYPES, boolean() | binary() | number()).

-type call_type() :: 'string' | 'positive' | 'integer' | 'list' | 'boolean'.

-record(parameter, { name :: binary()
                   , type :: term
                   , default_value :: (?PRIMITIVE_TYPES | [?PRIMITIVE_TYPES])
                   }).

-record(service_action, { name :: binary()
                        , function :: fun()
                        , parameters :: [#parameter{}]
                        }).

-define(SELECTOR_VALUES, '_' | '$1' | '$2').

-record(services_table_entry, { id :: binary()          | ?SELECTOR_VALUES
                              , public :: boolean()     | ?SELECTOR_VALUES
                              , name :: binary()        | ?SELECTOR_VALUES
                              , description :: binary() | ?SELECTOR_VALUES
                              , module :: module()      | ?SELECTOR_VALUES
                              }).

-type service_entry() :: #{ name := binary()
                          , description := binary()
                          , module := module()
                          }.
-type service_info_map() :: #{ binary() := service_entry() }.

-record(user_service_allowance_entry, { service_id :: binary() | ?SELECTOR_VALUES
                                      , user_id :: binary()    | ?SELECTOR_VALUES
                                      }).
