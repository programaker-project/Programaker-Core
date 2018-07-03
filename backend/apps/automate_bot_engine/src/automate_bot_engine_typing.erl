-module(automate_bot_engine_typing).


-export([ is_equivalent/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================
%% ToDo: Implement support for lists (now the result it's just false)
-spec is_equivalent(binary(), binary()) -> boolean();   % Equality
                   (number(), number()) -> boolean(); % Equality
                   (boolean(), boolean()) -> boolean(); % Equality

                   (binary(), number()) -> boolean();  % Convert to float
                   (number(), binary()) -> boolean();

                   (binary(), boolean()) -> boolean();  % Convert to boolean
                   (boolean(), binary()) -> boolean();

                   (_, _) -> false. % Everything else is false
%% Simple
is_equivalent(V1, V2) when (is_binary(V1) and is_binary(V2)) or
                           (is_number(V1) and is_number(V2)) or
                           (is_boolean(V1) and is_boolean(V2)) ->
    V1 == V2;

%% Convert binary part to float
is_equivalent(F1, V2) when (is_number(F1) and is_binary(V2)) ->
    case string:to_float(V2) of
        {F2, <<"">>} ->
            F1 == F2;
        _ ->
            false
    end;

is_equivalent(V1, V2) when (is_binary(V1) and is_number(V2)) ->
    is_equivalent(V2, V1);

%% Convert binary part to boolean
is_equivalent(B1, V2) when (is_boolean(B1) and is_binary(V2)) ->
    case string:lowercase(V2) of
        <<"true">> ->
            B1 == true;
        <<"false">> ->
            B1 == false;
        _ ->
            false
    end;

is_equivalent(V1, V2) when (is_binary(V1) and is_boolean(V2)) ->
    is_equivalent(V2, V1);

%% Everything else is false
is_equivalent(_, _) ->
    false.
