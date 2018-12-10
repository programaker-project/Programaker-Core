%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_registry_mnesia_backend).

-export([ start_link/0
        , register/3
        , list_all_public/0
        , get_all_services_for_user/1
        , allow_user/2
        ]).

-include("records.hrl").
-define(SERVICE_REGISTRY_TABLE, automate_service_registry_services_table).
-define(USER_SERVICE_ALLOWANCE_TABLE, automate_service_registry_user_service_allowance_table).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    Nodes = [node()],

    %% Live channels table
    ok = case mnesia:create_table(?SERVICE_REGISTRY_TABLE,
                                  [ { attributes, record_info(fields, services_table_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, services_table_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    ok = case mnesia:create_table(?USER_SERVICE_ALLOWANCE_TABLE,
                                  [ { attributes, record_info(fields, user_service_allowance_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, user_service_allowance_entry }
                                  , { type, bag }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    ignore.

-spec register(binary(), boolean(), #{ name := binary(), description := binary(), module := module()}) -> ok | {error, term(), string()}.
register(ServiceUuid, Public, #{ name := Name, description := Description, module := Module }) ->
    Entry = #services_table_entry{ id=ServiceUuid
                                 , public=Public
                                 , name=Name
                                 , description=Description
                                 , module=Module
                                 },

    Transaction = fun() ->
                          ok = mnesia:write(?SERVICE_REGISTRY_TABLE, Entry, write)
                  end,

    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec list_all_public() -> {ok, [service_info_map()]} | {error, term(), string()}.
list_all_public() ->
    MatchHead = #services_table_entry{ id='_'
                                     , public='$1'
                                     , name='_'
                                     , description='_'
                                     , module='_'
                                     },
    %% Check that the public setting is the one selected
    Guards = [{'==', '$1', true}],
    ResultColumn = '$_',
    Matcher = [{MatchHead, Guards, [ResultColumn]}],

    Transaction = fun() ->
                          mnesia:select(?SERVICE_REGISTRY_TABLE, Matcher)
                  end,

    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            {ok, convert_to_map(Result)};
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec allow_user(binary(), binary()) -> ok | {error, service_not_found}.
allow_user(ServiceId, UserId) ->
    Transaction = fun() ->
                           ok = mnesia:write(?USER_SERVICE_ALLOWANCE_TABLE,
                                             #user_service_allowance_entry{ service_id=ServiceId
                                                                          , user_id=UserId
                                                                          },
                                             write)
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec get_all_services_for_user(binary()) -> {ok, [service_info_map()]} | {error, term(), string()}.
get_all_services_for_user(UserId) ->
    MatchHead = #services_table_entry{ id='_'
                                     , public='$1'
                                     , name='_'
                                     , description='_'
                                     , module='_'
                                     },
    %% Check that the public setting is the one selected
    Guards = [{'==', '$1', true}],
    ResultColumn = '$_',
    PublicMatcher = [{MatchHead, Guards, [ResultColumn]}],

    AllowancesMatcherHead = #user_service_allowance_entry{ service_id='$1', user_id='$2' },
    AllowancesGuards = [{'==', '$2', UserId }],
    AllowancesResultColumn = '$1',
    AllowancesMatcher = [{AllowancesMatcherHead, AllowancesGuards, [AllowancesResultColumn]}],

    Transaction = fun() ->
                          Public = mnesia:select(?SERVICE_REGISTRY_TABLE, PublicMatcher),
                          UserAllowanceIds = mnesia:select(?USER_SERVICE_ALLOWANCE_TABLE, AllowancesMatcher),
                          UserAllowances = [ lists:nth(1, mnesia:read(?SERVICE_REGISTRY_TABLE, ServiceId))
                                             || ServiceId <- UserAllowanceIds
                                           ],
                          {Public, UserAllowances}
                  end,

    case mnesia:transaction(Transaction) of
        {atomic, {Public, UserAllowances}} ->
            {ok, convert_to_map(Public ++ UserAllowances)};
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec convert_to_map([#services_table_entry{}]) -> service_info_map().
convert_to_map(TableEntries) ->
    io:fwrite("Entries: ~p~n", [TableEntries]),
    convert_to_map(TableEntries, #{}).

-spec convert_to_map([#services_table_entry{}], service_info_map()) ->
    service_info_map().
convert_to_map([], Acc) ->
    Acc;

convert_to_map([#services_table_entry{ id=Id
                                     , name=Name
                                     , description=Description
                                     , module=Module}
                | T], Acc) ->
    convert_to_map(
      T,
      Acc#{ Id => #{ name => Name
                   , description => Description
                   , module => Module
                   }}
     ).
