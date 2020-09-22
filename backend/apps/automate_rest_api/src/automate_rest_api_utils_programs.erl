-module(automate_rest_api_utils_programs).

-export([ get_metadata_from_body/1
        ]).

-include("records.hrl").

get_metadata_from_body(Body) ->
    Map = jiffy:decode(Body, [return_maps]),
    { get_program_type_from_options(Map)
    , get_program_name_from_options(Map)
    }.


%% Util functions
get_program_type_from_options(#{ <<"type">> := <<"scratch_program">> }) ->
    scratch_program;
get_program_type_from_options(#{ <<"type">> := <<"flow_program">> }) ->
    flow_program;
get_program_type_from_options(#{ <<"type">> := Type }) when is_binary(Type) ->
    Type;
get_program_type_from_options(_) ->
    ?DEFAULT_PROGRAM_TYPE.

get_program_name_from_options(#{ <<"name">> := Name }) when is_binary(Name) ->
    case size(Name) < 4 of
        true ->
            generate_program_name();
        false ->
            Name
    end;
get_program_name_from_options(_) ->
    generate_program_name().

generate_program_name() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).
