-record(user_stat_metrics, { count :: pos_integer()
                           , registered_last_day :: pos_integer()
                           , registered_last_week :: pos_integer()
                           , registered_last_month :: pos_integer()
                           , logged_last_hour :: pos_integer()
                           , logged_last_day :: pos_integer()
                           , logged_last_week :: pos_integer()
                           , logged_last_month :: pos_integer()
                           }).

-record(bridge_stat_metrics, { public_count :: pos_integer()
                             , private_count :: pos_integer()
                             , connections :: pos_integer()
                             , unique_connections :: pos_integer()
                             , messages_on_flight :: pos_integer()
                             }).

-record(internal_metrics, { services_active :: map()
                          , bot_count :: #{ active => number(), workers => number()}
                          , thread_count :: #{ active => number(), workers => number()}
                          , monitor_count :: #{ active => number(), workers => number()}
                          , service_count :: #{ public => number(), all => number()}
                          , user_stats :: #user_stat_metrics{}
                          , bridge_stats :: #bridge_stat_metrics{}
                          }).
