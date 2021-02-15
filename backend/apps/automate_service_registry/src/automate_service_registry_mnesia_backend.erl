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
        , get_service_by_id/1
        , update_visibility/2
        , update_service_module/2

        , get_config_for_service/2
        , set_config_for_service/3

        , count_all_services/0
        , delete_service/2
        ]).

-include("records.hrl").
-include("databases.hrl").

%%====================================================================
%% API
%%====================================================================
start_link() ->
    Nodes = automate_configuration:get_sync_peers(),

    ok = automate_storage_versioning:apply_versioning(automate_service_registry_configuration:get_versioning(Nodes),
                                                      Nodes, ?MODULE),

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

-spec update_service_module(binary(),
                            #{ name := binary(), description := binary(), module := {module(), [_]} })
                           -> ok.
update_service_module(Uuid, #{ name := Name
                             , description := Description
                             , module := Module
                             }) ->
    Transaction = fun() ->
                          [Entry] = mnesia:read(?SERVICE_REGISTRY_TABLE, Uuid),
                          ok = mnesia:write(?SERVICE_REGISTRY_TABLE,
                                            Entry#services_table_entry{ name=Name
                                                                      , description=Description
                                                                      , module=Module
                                                                      }, write)
                  end,

    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec list_all_public() -> {ok, service_info_map()} | {error, term(), string()}.
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

-spec allow_user(binary(), owner_id()) -> ok | {error, service_not_found}.
allow_user(ServiceId, Owner) ->
    Transaction = fun() ->
                          ok = mnesia:write(?USER_SERVICE_ALLOWANCE_TABLE,
                                            #user_service_allowance_entry{ service_id=ServiceId
                                                                         , owner=Owner
                                                                         },
                                            write)
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec update_visibility(binary(), boolean()) -> ok | {error, service_not_found}.
update_visibility(ServiceId, IsPublic) ->
    Transaction = fun() ->
                          case mnesia:read(?SERVICE_REGISTRY_TABLE, ServiceId) of
                              [Entry] ->
                                  ok = mnesia:write(?SERVICE_REGISTRY_TABLE,
                                                    Entry#services_table_entry{ public=IsPublic
                                                                              }, write);
                              [] ->
                                  {error, service_not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec get_all_services_for_user(owner_id()) -> {ok, service_info_map()} | {error, term(), string()}.
get_all_services_for_user({OwnerType, OwnerId}) ->
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

    AllowancesMatcherHead = #user_service_allowance_entry{ service_id='$1', owner={'$2', '$3'} },
    AllowancesGuards = [ {'==', '$2', OwnerType }
                       , {'==', '$3', OwnerId }
                       ],
    AllowancesResultColumn = '$1',
    AllowancesMatcher = [{AllowancesMatcherHead, AllowancesGuards, [AllowancesResultColumn]}],

    Transaction = fun() ->
                          %% Public programs
                          Public = mnesia:select(?SERVICE_REGISTRY_TABLE, PublicMatcher),

                          %% Programs where the user is allowed
                          UserAllowanceIds = mnesia:select(?USER_SERVICE_ALLOWANCE_TABLE, AllowancesMatcher),
                          UserAllowances = lists:filtermap(fun (ServiceId) ->
                                                                   case mnesia:read(?SERVICE_REGISTRY_TABLE, ServiceId) of
                                                                       [] ->
                                                                           false;
                                                                       [Result] ->
                                                                           {true, Result}
                                                                   end
                                                           end, UserAllowanceIds),

                          %% Programs from a user group where it has the role needed to use them
                          GroupAllowances = case OwnerType of
                                                group -> []; %% Right now a group cannot be part of another group
                                                user ->
                                                    {ok, UserGroups} = automate_storage:get_user_groups({user, OwnerId}),

                                                    %% Obtain groups from where the user is allowed to take connections from
                                                    AllowedInGroups = lists:filtermap(
                                                                        fun({#user_group_entry{ id=GroupId
                                                                                              , min_level_for_private_bridge_usage=MinLevel }
                                                                            , Role}) ->
                                                                                case automate_storage_utils:role_has_min_level_in_group(Role, MinLevel) of
                                                                                    false -> false;
                                                                                    true -> { true, GroupId }
                                                                                end
                                                                        end, UserGroups),

                                                    %% Find the allowances of the groups found
                                                    lists:flatmap(fun(GroupId) ->
                                                                          GroupHead = #user_service_allowance_entry{ service_id='$1'
                                                                                                                   , owner={'$2', '$3'}
                                                                                                                   },
                                                                          GroupGuard = [ {'==', '$2', group }
                                                                                       , {'==', '$3', GroupId }
                                                                                       ],
                                                                          GroupResultColumn = '$1',
                                                                          GroupMatcher = [{GroupHead, GroupGuard, [GroupResultColumn]}],
                                                                          AllowanceIds = mnesia:select(?USER_SERVICE_ALLOWANCE_TABLE, GroupMatcher),
                                                                          Allowances = lists:filtermap(
                                                                                         fun (ServiceId) ->
                                                                                                 case mnesia:read(?SERVICE_REGISTRY_TABLE, ServiceId) of
                                                                                                     [] ->
                                                                                                         false;
                                                                                                     [Result] ->
                                                                                                         {true, Result}
                                                                                                 end
                                                                                         end, AllowanceIds),
                                                                          Allowances
                                                                  end, AllowedInGroups)
                                            end,

                          {Public, UserAllowances, GroupAllowances}
                  end,

    case mnesia:ets(Transaction) of
        {Public, UserAllowances, GroupAllowances} ->
            {ok, convert_to_map(Public ++ UserAllowances ++ GroupAllowances)};
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec get_service_by_id(binary()) -> {ok, service_entry()} | {error, not_found}.
get_service_by_id(ServiceId) ->
    Transaction = fun() ->
                          %% TODO: Check user permissions
                          case mnesia:read(?SERVICE_REGISTRY_TABLE, ServiceId) of
                              [] ->
                                  {error, not_found};
                              [Result] ->
                                  {ok, Result}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, {ok, Result}} ->
            {ok, entry_to_map(Result)};
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.


-spec get_config_for_service(binary(), atom()) -> {ok, any()} | {error, not_found}.
get_config_for_service(ServiceId, Property) ->
    Transaction = fun() ->
                          %% TODO: Check user permissions
                          case mnesia:read(?SERVICE_CONFIGURATION_TABLE, {ServiceId, Property}) of
                              [] ->
                                  {error, not_found};
                              [#services_configuration_entry{value=Value}] ->
                                  {ok, Value}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec set_config_for_service(binary(), atom(), any()) -> ok | {error, atom()}.
set_config_for_service(ServiceId, Property, Value) ->
    Transaction = fun() ->
                          %% TODO: Check user permissions
                          ok = mnesia:write(?SERVICE_CONFIGURATION_TABLE,
                                            #services_configuration_entry{ configuration_id={ServiceId, Property}
                                                                         , value=Value
                                                                         }, write)
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec count_all_services() -> number().
count_all_services() ->
    length(mnesia:dirty_all_keys(?SERVICE_REGISTRY_TABLE)).


-spec delete_service(owner_id(), binary()) -> ok.
delete_service(_Owner, ServiceId) ->
    %% HACK: Note that service registry table does not capture owners
    Transaction = fun() ->
                          ok = mnesia:delete(?SERVICE_REGISTRY_TABLE, ServiceId, write)
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec convert_to_map([#services_table_entry{}]) -> service_info_map().
convert_to_map(TableEntries) ->
    convert_to_map(TableEntries, #{}).

-spec convert_to_map([#services_table_entry{}], service_info_map()) ->
                            service_info_map().
convert_to_map([], Acc) ->
    Acc;

convert_to_map([H = #services_table_entry{ id=Id } | T], Acc) ->
    convert_to_map(
      T,
      Acc#{ Id => entry_to_map(H) }
     ).

entry_to_map(#services_table_entry{ id=_
                                  , name=Name
                                  , description=Description
                                  , module=Module}
            ) ->
    #{ name => Name
     , description => Description
     , module => Module
     }.
