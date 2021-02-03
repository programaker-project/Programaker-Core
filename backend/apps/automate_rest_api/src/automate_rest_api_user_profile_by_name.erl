%%% @doc
%%% REST endpoint to manage groups.
%%% @end

-module(automate_rest_api_user_profile_by_name).
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

-record(state, { user_name :: binary()
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserName = cowboy_req:binding(user_name, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1, #state{ user_name=UserName }}.

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
to_json(Req, State=#state{user_name=UserName}) ->
    %% Get public profile
    {ok, {user, UserId}} = automate_storage:get_userid_from_username(UserName),

    {ok, Programs } = automate_storage:list_public_programs_from_userid(UserId),
    {ok, Groups} = automate_storage:get_user_groups({user, UserId}),
    {ListedPrograms, ListedGroups} = case automate_storage:get_owner_public_listings({user, UserId}) of
                                         {ok, #user_profile_listings_entry{ programs=LPrograms, groups=LGroups }} ->
                                             {LPrograms, LGroups};
                                         {error, not_found} ->
                                             {[], []}
                                     end,

    {ok, Bridges } = automate_service_port_engine:get_user_service_ports({user, UserId}),

    ProgramList = lists:map(fun(Program) ->
                                    ProgramBridges = try automate_bot_engine:get_bridges_on_program(Program) of
                                                         {ok, Result} ->
                                                             Result
                                                     catch ErrNS:Error:StackTrace ->
                                                             automate_logging:log_platform(error, ErrNS, Error, StackTrace),
                                                             []
                                                     end,
                                    ?FORMATTING:program_listing_to_json(Program, ProgramBridges)
                            end,
                            lists:filter(fun(#user_program_entry{ id=ProgramId }) ->
                                                 lists:any(fun(It) ->
                                                                   ProgramId == It
                                                           end, ListedPrograms)
                                         end, Programs)),

    GroupList = lists:map(fun ?FORMATTING:group_and_role_to_json/1,
                          lists:filter(fun({#user_group_entry{ id=GroupId }, _}) ->
                                               lists:any(fun(It) ->
                                                                 GroupId == It
                                                         end, ListedGroups)
                                       end, Groups)),

    Output = jiffy:encode(#{ name => UserName
                           , id => UserId
                           , programs => ProgramList
                           , groups => GroupList
                           , bridges => lists:map(fun ?FORMATTING:bridge_to_json/1, Bridges)
                           }),
    Res = ?UTILS:send_json_format(Req),

    { Output, Res, State }.
