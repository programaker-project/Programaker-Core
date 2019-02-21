%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_template_engine_mnesia_backend).

-export([ start_link/0
        ]).

%% API
-export([ list_templates_from_user_id/1
        , create_template/3
        ]).

-include("records.hrl").
-define(TEMPLATE_TABLE, automate_template_engine_templates_table).
%%====================================================================
%% API
%%====================================================================
start_link() ->
    Nodes = [node()],

    %% Service port identity table
    ok = case mnesia:create_table(?TEMPLATE_TABLE,
                                  [ { attributes, record_info(fields, template_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, template_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    ignore.

-spec list_templates_from_user_id(binary()) -> {ok, [map()]}.
list_templates_from_user_id(UserId) ->
    Transaction = fun() ->
                          %% Find userid with that name
                          MatchHead = #template_entry{ id='_'
                                                         , name='_'
                                                         , owner='$1'
                                                         , content='_'
                                                         },
                          Guard = {'==', '$1', UserId},
                          ResultColumn = '$_',
                          Matcher = [{MatchHead, [Guard], [ResultColumn]}],

                          {ok, mnesia:select(?TEMPLATE_TABLE, Matcher)}
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.


-spec create_template(binary(), binary(), [any()]) -> {ok, binary()}.
create_template(UserId, TemplateName, TemplateContent) ->
    Id = generate_id(),
    Entry = #template_entry{ id=Id
                           , name=TemplateName
                           , owner=UserId
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


generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).

