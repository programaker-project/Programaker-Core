%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_program_tags).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        , content_types_accepted/2
        , resource_exists/2
        ]).

-export([ accept_tags_update/2
        , to_json/2
        ]).

-include("./records.hrl").

-record(program_tag_opts, { user_id, program_id }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    ProgramId = cowboy_req:binding(program_id, Req),
    {cowboy_rest, Req
    , #program_tag_opts{ user_id=UserId
                       , program_id=ProgramId
                       }}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            { false, Req, State };
        _ ->
            { true, Req, State}
    end.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    io:fwrite("Asking for methods~n", []),
    {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };
        _ ->
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    { {false, <<"Authorization header not found">>} , Req1, State };
                X ->
                    #program_tag_opts{user_id=UserId} = State,
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            { true, Req1, State };
                        {true, _} -> %% Non matching user_id
                            { { false, <<"Unauthorized to create a program here">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% POST handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_tags_update}],
     Req, State}.

-spec accept_tags_update(_, #program_tag_opts{})
                        -> {'false',_,#program_tag_opts{}} | {'true',_,#program_tag_opts{}}.
accept_tags_update(Req, #program_tag_opts{user_id=UserId
                                         , program_id=ProgramId
                                         }) ->
    {ok, Body, _} = read_body(Req),
    #{<<"tags">> := Tags } = jiffy:decode(Body, [return_maps]),

    case automate_rest_api_backend:update_program_tags(UserId, ProgramId, Tags) of
        ok ->

            Output = jiffy:encode(#{ <<"success">> => true
                                   }),

            Res1 = cowboy_req:set_resp_body(Output, Req),
            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),

            { true, Res3, #program_tag_opts{ user_id=UserId
                                           , program_id=ProgramId
                                           }
            };
        {error, _} ->
            Output = jiffy:encode(#{ <<"success">> => false
                                   }),

            Res1 = cowboy_req:set_resp_body(Output, Req),
            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),

            { false, Res3, #program_tag_opts{ user_id=UserId
                                            , program_id=ProgramId
                                            }
            }
    end.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #program_tag_opts{})
             -> {binary(),cowboy_req:req(), #program_tag_opts{}}.
to_json(Req, State) ->
    #program_tag_opts{user_id=UserId, program_id=ProgramId} = State,
    case automate_rest_api_backend:get_program_tags(UserId, ProgramId) of
        { ok, Tags } ->
            Output = jiffy:encode(Tags),
            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.


read_body(Req0) ->
    read_body(Req0, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.