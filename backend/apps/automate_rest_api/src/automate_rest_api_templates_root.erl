%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_templates_root).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        , content_types_accepted/2
        , resource_exists/2
        ]).

-export([ accept_json_create_template/2
        , to_json/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_template_engine/src/records.hrl").

-record(state, { user_id :: binary() }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    {cowboy_rest, Req
    , #state{ user_id=UserId }}.

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
                    #state{user_id=UserId} = State,
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            { true, Req1, State };
                        {true, _} -> %% Non matching user id
                            { { false, <<"Unauthorized to create a template here">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% POST handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_json_create_template}],
     Req, State}.

-spec accept_json_create_template(cowboy_req:req(), #state{})
                                 -> {'true',cowboy_req:req(), #state{}}.
accept_json_create_template(Req, State) ->
    #state{user_id=UserId} = State,

    {ok, Body, Req1} = ?UTILS:read_body(Req),
    Template = jiffy:decode(Body, [return_maps]),
    #{ <<"name">> := TemplateName, <<"content">> := TemplateContent } = Template,

    case automate_rest_api_backend:create_template({user, UserId}, TemplateName, TemplateContent) of
        { ok, TemplateId } ->

            Output = jiffy:encode(#{ <<"id">> => TemplateId
                                   }),

            Res1 = cowboy_req:set_resp_body(Output, Req1),
            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),

            { true, Res3, State }
    end.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State=#state{user_id=UserId}) ->
    case automate_template_engine:list_templates({user, UserId}) of
        { ok, Templates } ->

            Output = jiffy:encode(lists:map(fun template_to_map/1, Templates)),
            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.


template_to_map(#template_entry{ id=Id
                               , name=Name
                               , owner={OwnerType, OwnerId}
                               , content=_Content
                               }) ->
    #{ id => Id
     , name => Name
     , owner => OwnerId
     , owner_full => #{ type => OwnerType, id => OwnerId }
     }.
