%%% @doc
%%% REST endpoint to manage groups.
%%% @end

-module(automate_rest_api_group_profile_by_name).
-export([init/2]).
-export([ allowed_methods/2
        , content_types_provided/2
        , options/2
        ]).

-export([ to_json/2
        ]).

-define(UTILS, automate_rest_api_utils).
-define(FORMATTING, automate_rest_api_utils_formatting).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").


-record(state, { group_name :: binary()
               , group_info :: #user_group_entry{}
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    GroupName = cowboy_req:binding(group_name, Req),
    {ok, GroupInfo} = automate_storage:get_group_by_name(GroupName),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1, #state{ group_name=GroupName
                             , group_info=GroupInfo
                             }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.


-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State=#state{group_info=GroupInfo}) ->
    #user_group_entry{ id=GroupId, name=GroupName } = GroupInfo,

    {ok, Programs } = automate_storage:list_programs({group, GroupId}),
    {ok, Bridges } = automate_service_port_engine:get_user_service_ports({group, GroupId}),
    {ok, Collaborators } = automate_storage:list_public_collaborators(GroupId),

    ProgramList = lists:filtermap(fun(Program) ->
                                          case Program of
                                              #user_program_entry{ visibility=public } ->
                                                  ProgramBridges = try automate_bot_engine:get_bridges_on_program(Program) of
                                                                       {ok, Result} ->
                                                                           Result
                                                                   catch ErrNS:Error:StackTrace ->
                                                                           automate_logging:log_platform(error, ErrNS, Error, StackTrace),
                                                                           []
                                                                   end,
                                                  {true, ?FORMATTING:program_listing_to_json(Program, ProgramBridges)};
                                              _ ->
                                                  false
                                          end
                            end, Programs),

    Output = jiffy:encode(#{ name => GroupName
                           , id => GroupId
                           , programs => ProgramList
                           , collaborators => lists:map(fun ?FORMATTING:user_to_json/1, Collaborators)
                           , bridges => lists:map(fun ?FORMATTING:bridge_to_json/1, Bridges)
                           }),
    Res = ?UTILS:send_json_format(Req),

    { Output, Res, State }.
