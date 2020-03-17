-module(automate_storage_utils).

-export([ canonicalize/1
        , validate_username/1
        ]).


-define(VALID_CHARACTERS, "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").

-spec validate_username(binary()) -> boolean().
validate_username(String)
  when is_binary(String) and (byte_size(String) < 4) orelse (byte_size(String) > 50) ->
    %% Length error
    false;

validate_username(String) when is_binary(String) ->
    case string:take(binary_to_list(String), ?VALID_CHARACTERS) of
        { _, [] } ->
            true;
        _ ->
            false
    end;

validate_username(_) ->
    false.


-spec canonicalize(binary()) -> binary().
canonicalize(X) ->
    %% Lowercasing has to be applied to the non-binary form
    %% to properly support UTF8 characters
    Lowercased = string:lowercase(binary_to_list(X)),
    list_to_binary(Lowercased).
