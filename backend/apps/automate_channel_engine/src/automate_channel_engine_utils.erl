-module(automate_channel_engine_utils).

-export([ canonicalize_selector/1
        ]).

canonicalize_selector(Atom) when is_atom(Atom) ->
    Atom;
canonicalize_selector(Selector) ->
    string:lowercase(Selector).
