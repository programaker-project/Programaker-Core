-module(automate_channel_engine_utils).

-export([ canonicalize_selector/1
        ]).

canonicalize_selector(Atom) when is_atom(Atom) or is_tuple(Atom) ->
    Atom;
canonicalize_selector(Selector) when is_binary(Selector) or is_list(Selector) ->
    string:lowercase(Selector).
