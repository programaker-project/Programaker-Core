%%%-------------------------------------------------------------------
%% @doc automate_template_engine interface.
%% @end
%%%-------------------------------------------------------------------

-module(automate_template_engine_matching).

-behaviour(supervisor).

%% API
-export([ match/4
        ]).

-define(BACKEND, automate_template_engine_mnesia_backend).
-include("records.hrl").

%%====================================================================
%% API functions
%%====================================================================
match(UserId, Thread, TemplateId, InputValue) ->
    case ?BACKEND:get_template(UserId, TemplateId) of
        {ok, #template_entry{ content=Content
                            }} ->
            match_content(Thread, Content, InputValue)
    end.


%%====================================================================
%% Internal functions
%%====================================================================
match_content(Thread, Template, InputValue) ->
    Regex = build_regexp(Thread, Template),
    {ok, Pattern} = re:compile(["^", Regex, "$"], [ungreedy]),
    case re:run(InputValue, Pattern) of
        {match, [_AllMatch | Matches]} ->
            Variables = get_variables(Template),

            {ok, NewThread} = set_variables(InputValue, Matches, Variables, Thread),
            {ok, NewThread, none};
        nomatch ->
            {error, not_found}
    end.

set_variables(_Original, [], [], Thread) ->
    {ok, Thread};

set_variables(Original, [{Start, Len} | Matches], [Variable | Variables], Thread) ->
    Value = binary:part(Original, Start, Len),
    {ok, NewThread} = automate_bot_engine_variables:set_program_variable(Thread, Variable, Value),
    set_variables(Original, Matches, Variables, Thread).


get_variables(Template) ->
    lists:flatmap(fun get_variables_in_chunk/1, Template).

get_variables_in_chunk(#{ <<"type">> := <<"text">>
                        }) ->
    [];

get_variables_in_chunk(#{ <<"type">> := <<"line">>
                        , <<"content">> := SubChunks
                        }) ->
    lists:flatmap(fun get_variables_in_chunk/1, SubChunks);

get_variables_in_chunk(#{ <<"type">> := <<"variable">>
                        , <<"class">> := <<"input">>
                        }) ->
    [];

get_variables_in_chunk(#{ <<"type">> := <<"variable">>
                        , <<"class">> := <<"output">>
                        , <<"content">> := VariableName
                        }) ->
    [VariableName].


build_regexp(Thread, Template) ->
    lists:map(fun (Chunk) -> chunk_to_regex(Thread, Chunk) end, Template).

chunk_to_regex(_Thread, #{ <<"type">> := <<"text">>
                         , <<"content">> := Text
                         }) ->
    Text;

chunk_to_regex(Thread, #{ <<"type">> := <<"line">>
                        , <<"content">> := SubChunks
                        }) ->
    lists:map(fun (Chunk) -> chunk_to_regex(Thread, Chunk) end, SubChunks);

chunk_to_regex(Thread, #{ <<"type">> := <<"variable">>
                        , <<"class">> := <<"input">>
                        , <<"content">> := VariableName
                        }) ->
    {ok, Value} = automate_bot_engine_variables:get_program_variable(Thread, VariableName),
    Value;

chunk_to_regex(Thread, #{ <<"type">> := <<"variable">>
                        , <<"class">> := <<"output">>
                        , <<"content">> := VariableName
                        }) ->
    <<"(.*)">>.
