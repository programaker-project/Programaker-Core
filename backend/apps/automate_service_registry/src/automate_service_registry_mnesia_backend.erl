%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_registry_mnesia_backend).

-export([ start_link/0
        , register/2
        , list_all/0
        ]).

-define(SERVICE_REGISTRY_TABLE, automate_service_registry_services_table).

-record(services_table_entry, { id :: binary()          | '$1'
                              , name :: binary()        | '$_'
                              , description :: binary() | '$_'
                              , module :: module()      | '$_'
                              }).

-type service_info_map() :: #{ binary() := #{ name := binary()
                                            , description := binary()
                                            , module := module()
                                            } }.

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
    ignore.

-spec register(binary(), #{ name := binary(), description := binary(), module := module()}) -> ok | {error, term(), string()}.
register(ServiceUuid, #{ name := Name, description := Description, module := Module }) ->
    Entry = #services_table_entry{ id=ServiceUuid
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

-spec list_all() -> {ok, [service_info_map()]} | {error, term(), string()}.
list_all() ->
    Transaction = fun() ->
                          on_all(?SERVICE_REGISTRY_TABLE,
                                 fun(Id) ->
                                         [Result] = mnesia:read(?SERVICE_REGISTRY_TABLE, Id),
                                         Result
                                 end)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            {ok, convert_to_map(Result)};
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

on_all(Tab, Callback) ->
    Results = mnesia:all_keys(Tab),
    [Callback(Object) || Object <- Results].

-spec convert_to_map([#services_table_entry{}]) -> service_info_map().
convert_to_map(TableEntries) ->
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
      maps:put(Id,
               #{ name => Name, description => Description, module => Module },
               Acc)).
