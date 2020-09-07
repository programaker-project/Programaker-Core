%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_templates_specific).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_accepted/2
        , content_types_provided/2
        , delete_resource/2
        ]).

-export([ accept_json_template/2
        , to_json/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_template_engine/src/records.hrl").

-record(state, { user_id, template_id }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    TemplateId = cowboy_req:binding(template_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ user_id=UserId
            , template_id=TemplateId
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

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
                        {true, _} -> %% Non matching user_id
                            { { false, <<"Unauthorized to create a template here">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.


content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_json_template}],
     Req, State}.

accept_json_template(Req, State) ->
    case cowboy_req:method(Req) of
        <<"PUT">> ->
            update_template(Req, State)
    end.

%% Get handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State) ->
    #state{user_id=UserId, template_id=TemplateId} = State,
    case automate_rest_api_backend:get_template(UserId, TemplateId) of
        { ok, Template } ->

            Output = jiffy:encode(template_to_json(Template)),

            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State };
        {error, Reason} ->
            Code = 500,
            Output = jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }),
            Res = cowboy_req:reply(Code, #{ <<"content-type">> => <<"application/json">> }, Output, Req),
            { stop, Res, State }
    end.


%% PUT handler
update_template(Req, State) ->
    #state{template_id=TemplateId, user_id=UserId} = State,

    {ok, Body, Req1} = ?UTILS:read_body(Req),
    Parsed = jiffy:decode(Body, [return_maps]),
    #{ <<"name">> := TemplateName, <<"content">> := TemplateContent } = Parsed,

    case automate_rest_api_backend:update_template(UserId, TemplateId, TemplateName, TemplateContent) of
        ok ->
            Req2 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => true }), Req),
            { true, Req2, State };
        { error, Reason } ->
            Req2 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req1),
            { false, Req2, State }
    end.

%% DELETE handler
delete_resource(Req, State) ->
    #state{template_id=TemplateId, user_id=UserId} = State,
    case automate_rest_api_backend:delete_template(UserId, TemplateId) of
        ok ->
            Req1 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => true}), Req),
            { true, Req1, State };
        { error, Reason } ->
            Req1 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req),
            { false, Req1, State }
    end.

template_to_json(#template_entry{ id=Id
                                , name=Name
                                , owner=Owner
                                , content=Content
                                }) ->
    #{ id => Id
     , name => Name
     , owner => Owner
     , content => Content
     }.
