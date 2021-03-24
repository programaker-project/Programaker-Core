%%% @doc
%%% REST endpoint to validate connection tokens to programs.
%%% @end

-module(automate_rest_api_validate_connection_token_by_program_id).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        ]).

-export([ to_json/2
        ]).

-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").
-define(UTILS, automate_rest_api_utils).
-define(FORMATTING, automate_rest_api_utils_formatting).

-record(state, { program_id :: binary(), user_id :: binary() | undefined }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ProgramId = cowboy_req:binding(program_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ program_id=ProgramId
            , user_id=undefined
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{program_id=ProgramId}) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };

        %% Reading a public program
        Method ->
            {ok, #user_program_entry{ visibility=Visibility }} = automate_storage:get_program_from_id(ProgramId),
            IsPublic = ?UTILS:is_public(Visibility),
            Action = edit_program,
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    case {Method, IsPublic} of
                        {<<"GET">>, true} ->
                            { true, Req1, State };
                        _ ->
                            { {false, <<"Authorization header not found">>} , Req1, State }
                    end;
                X ->
                    case automate_rest_api_backend:is_valid_token_uid(X, {read_program, ProgramId}) of
                        {true, UId} ->
                            case automate_storage:is_user_allowed({user, UId}, ProgramId, Action) of
                                {ok, true} ->
                                    { true, Req1, State#state{user_id=UId} };
                                {ok, false} ->
                                    { { false, <<"Action not authorized">>}, Req1, State };
                                {error, Reason} ->
                                    automate_logging:log_api(warning, ?MODULE, {authorization_error, Reason}),
                                    { { false, <<"Error on authorization">>}, Req1, State }
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State=#state{program_id=_ProgramId, user_id=UserId}) ->
    { jiffy:encode(#{ success => true, user_id => UserId }), Req, State }.
