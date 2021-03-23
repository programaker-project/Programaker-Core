%%% @doc
%%% REST endpoint to manage groups.
%%% @end

-module(automate_rest_api_group_by_name).
-export([init/2]).
-export([ allowed_methods/2
        , is_authorized/2
        , content_types_provided/2
        , options/2
        ]).

-export([ to_json/2
        ]).

-define(UTILS, automate_rest_api_utils).
-define(FORMATTING, automate_rest_api_utils_formatting).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { user_id :: binary() | undefined
               , group_name :: binary()
               , group_info :: #user_group_entry{}
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    GroupName = cowboy_req:binding(group_name, Req),
    {ok, GroupInfo} = automate_storage:get_group_by_name(GroupName),
    {cowboy_rest, Req, #state{ user_id=undefined, group_name=GroupName, group_info=GroupInfo }}.

-spec is_authorized(cowboy_req:req(),_) -> {'true' | {'false', binary()}, cowboy_req:req(),_}.
is_authorized(Req, State=#state{ group_info=#user_group_entry{ id=GroupId } }) ->
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
                    case automate_rest_api_backend:is_valid_token_uid(X, { read_group_info, GroupId }) of
                        {true, UserId} ->
                            case automate_storage:can_user_view_as({user, UserId}, { group, GroupId }) of
                                true ->
                                    { true, Req1, State#state{ user_id=UserId } };
                                false ->
                                    { { false, <<"User cannot view this group">>}, Req1, State }
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.


-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State=#state{ group_info=GroupInfo }) ->
    Output = jiffy:encode(#{ success => true, group => ?FORMATTING:group_to_json(GroupInfo)}),
    Res = ?UTILS:send_json_format(Req),

    { Output, Res, State }.
