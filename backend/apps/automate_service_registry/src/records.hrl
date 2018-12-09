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
