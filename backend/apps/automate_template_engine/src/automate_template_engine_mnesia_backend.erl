%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_template_engine_mnesia_backend).

-export([ start_link/0
        ]).

%% API
-export([ list_templates/1
        , create_template/3
        , delete_template/2
        , update_template/4
        , get_template/2
        ]).

-include("records.hrl").
-include("databases.hrl").

%%====================================================================
%% API
%%====================================================================
start_link() ->
    Nodes = automate_configuration:get_sync_peers(),

    ok = automate_storage_versioning:apply_versioning(automate_template_engine_configuration:get_versioning(Nodes),
                                                      Nodes, ?MODULE),

    ignore.

-spec list_templates(owner_id()) -> {ok, [map()]}.
list_templates({OwnerType, OwnerId}) ->
    Transaction = fun() ->
                          %% Find userid with that name
                          MatchHead = #template_entry{ id='_'
                                                     , name='_'
                                                     , owner={'$1', '$2'}
                                                     , content='_'
                                                     },
                          Guards = [ {'==', '$1', OwnerType}
                                   , {'==', '$2', OwnerId}
                                   ],
                          ResultColumn = '$_',
                          Matcher = [{MatchHead, Guards, [ResultColumn]}],

                          {ok, mnesia:select(?TEMPLATE_TABLE, Matcher)}
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.


-spec create_template(owner_id(), binary(), [any()]) -> {ok, binary()}.
create_template(Owner, TemplateName, TemplateContent) ->
    Id = generate_id(),
    Entry = #template_entry{ id=Id
                           , name=TemplateName
                           , owner=Owner
                           , content=TemplateContent
                           },

    Transaction = fun() ->
                          ok = mnesia:write(?TEMPLATE_TABLE, Entry, write),
                          {ok, Id}
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

-spec delete_template(owner_id(), binary()) -> ok | {error, binary()}.
delete_template(Owner, TemplateId) ->
    Transaction = fun() ->
                          case mnesia:read(?TEMPLATE_TABLE, TemplateId) of
                              [#template_entry{ owner=OwnerId
                                              }] ->
                                  case OwnerId of
                                      Owner ->
                                          ok = mnesia:delete(?TEMPLATE_TABLE, TemplateId, write),
                                          ok;
                                      _ ->
                                          {error, unauthorized}
                                  end;
                              _ ->
                                  {error, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.



-spec update_template(owner_id(), binary(), binary(), [any()]) -> ok | {error, binary()}.
update_template(Owner, TemplateId, TemplateName, TemplateContent) ->
    Entry = #template_entry{ id=TemplateId
                           , name=TemplateName
                           , owner=Owner
                           , content=TemplateContent
                           },
    Transaction = fun() ->
                          case mnesia:read(?TEMPLATE_TABLE, TemplateId) of
                              [#template_entry{ owner=OwnerId
                                              }] ->
                                  case OwnerId of
                                      Owner ->
                                          ok = mnesia:write(?TEMPLATE_TABLE, Entry, write),
                                          ok;
                                      _ ->
                                          {error, unauthorized}
                                  end;
                              _ ->
                                  {error, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.



-spec get_template(owner_id(), binary()) -> {ok, #template_entry{}} | {error, binary()}.
get_template(Owner, TemplateId) ->
    Transaction = fun() ->
                          case mnesia:read(?TEMPLATE_TABLE, TemplateId) of

                              [Entry=#template_entry{ owner=OwnerId
                                                    }] ->
                                  case OwnerId of
                                      Owner ->
                                          {ok, Entry};
                                      _ ->
                                          {error, unauthorized}
                                  end;
                              _ ->
                                  {error, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.


%%====================================================================
%% Internal functions
%%====================================================================
generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).
