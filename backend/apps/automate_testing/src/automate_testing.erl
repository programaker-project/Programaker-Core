-module(automate_testing).

-export([ apply_time/1
        , set_corrected_time/1
        , unset_corrected_time/0
        ]).

apply_time(X) ->
    case ets:whereis(time_tests) of
        undefined ->
            X;
        _ ->
            case ets:lookup(time_tests, correction) of
                [] ->
                    X;
                [{correction, C}] ->
                    { MegaS, S, MicroS } = X,
                    Corrected = { MegaS, S + C, MicroS },
                    Corrected
            end
    end.

set_corrected_time(CorrectedTimestamp) ->
    case ets:whereis(time_tests) of
        undefined ->
            ets:new(time_tests, [ordered_set, public, named_table]),
            ok;
        _ ->
            ok
    end,

    Now = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(erlang:timestamp())),
    Corrected = calendar:datetime_to_gregorian_seconds(CorrectedTimestamp),
    Diff = Corrected - Now,

    true = ets:insert(time_tests, { correction, Diff }),
    ok.

unset_corrected_time() ->
    erase(time_correction).
