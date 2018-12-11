%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_services_how_to_enable).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        ]).

-export([ to_json/2
        ]).

-include("./records.hrl").

-record(state, { username, service_id }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    ServiceId = cowboy_req:binding(service_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ username=UserId
            , service_id=ServiceId
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    io:fwrite("[SPService]Asking for methods~n", []),
    {[<<"GET">>, <<"PUT">>, <<"OPTIONS">>], Req, State}.

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
                    #state{username=Username} = State,
                    case automate_rest_api_backend:is_valid_token(X) of
                        {true, Username} ->
                            { true, Req1, State };
                        {true, _} -> %% Non matching username
                            { { false, <<"Unauthorized to create a service here">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% Get handler
content_types_provided(Req, State) ->
    io:fwrite("User > service > ID~n", []),
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
                                   -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State) ->
    #state{username=Username, service_id=ServiceId} = State,
    io:fwrite("?? ~p~n", [ServiceId]),
    case automate_rest_api_backend:get_service_enable_how_to(Username, ServiceId) of
        { ok, HowTo } ->
            Output = how_to_to_json(HowTo),

            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State };
        {error, not_found} ->
            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            %% TODO: Return 404
            { jiffy:encode(#{ <<"success">> => false, <<"message">> => <<"Service not found">> }),
              Res2, State }
    end.


how_to_to_json(#service_enable_how_to{ service=Service
                                     , method=Method
                                     , extra=#service_enable_extra_telegram{ token=Token
                                                                           , bot_name=BotName
                                                                           }
                                     }) ->
    #service_metadata{ id=Id
                     , name=Name
                     , link=Link
                     , enabled=Enabled
                     } = Service,
    ServiceAsDictionary = #{ <<"id">> => Id
                           , <<"name">> => Name
                           , <<"link">> =>  Link
                           , <<"enabled">> => Enabled
                           },

    jiffy:encode(#{ <<"service">> => ServiceAsDictionary
                  , <<"method">> => Method
                  , <<"extra">> => #{ <<"token">> => Token
                                    , <<"bot_name">> => BotName
                                    , <<"service_type">> => <<"registration_bot">>
                                    }
                  }).

