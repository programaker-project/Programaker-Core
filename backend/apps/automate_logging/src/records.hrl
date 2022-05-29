-ifndef(AUTOMATE_LOGGING_RECORDS).
-define(AUTOMATE_LOGGING_RECORDS, true).

-record(call_data, { call_start_time :: time_in_seconds()
                   , call_end_time :: time_in_seconds()
                   , block_id   :: binary()
                   , program_id :: binary()
                   , thread_id :: binary()
                   , succeeded :: boolean()
                   , operation :: binary()
                   , arguments :: [_] | tuple()
                   , result :: any()
        }).

-endif.
