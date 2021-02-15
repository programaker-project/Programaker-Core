-module(automate_storage_utils).

-export([ canonicalize/1
        , validate_username/1
        , validate_canonicalizable/1
        , role_has_min_level_in_group/2
        ]).

-include("./records.hrl").


-define(VALID_CHARACTERS, "_-abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").
-define(NUMBER_CHARACTERS, "0123456789-").


validate_canonicalizable(String) when is_binary(String) ->
    case string:take(binary_to_list(String), ?VALID_CHARACTERS) of
        { _, [] } ->
            true;
        _ ->
            false
    end;
validate_canonicalizable(_String) ->
    false.

validate_not_all_numbers(String) when is_binary(String) ->
    case string:take(binary_to_list(String), ?NUMBER_CHARACTERS) of
        { _, [] } ->
            false;
        _ ->
            true
    end.

-spec validate_username(binary()) -> boolean().
validate_username(String)
  when is_binary(String) and (byte_size(String) < 4) orelse (byte_size(String) > 50) ->
    %% Length error
    false;

validate_username(String) when is_binary(String) ->
    validate_canonicalizable(String) and validate_not_all_numbers(String);

validate_username(_) ->
    false.


-spec canonicalize(binary()) -> binary().
canonicalize(X) ->
    %% Lowercasing has to be applied to the non-binary form
    %% to properly support UTF8 characters
    Lowercased = string:lowercase(binary_to_list(X)),
    list_to_binary(Lowercased).


-spec role_has_min_level_in_group(Role :: user_in_group_role(), MinLevel :: user_in_group_role() | not_allowed) -> boolean().
role_has_min_level_in_group(_Role, not_allowed) ->
    false;
role_has_min_level_in_group(admin, _MinLevel) ->
    true;
role_has_min_level_in_group(_, admin) ->
    false;
role_has_min_level_in_group(editor, _) ->
    true;
role_has_min_level_in_group(viewer, editor) ->
    false;
role_has_min_level_in_group(_, viewer) ->
    true.
