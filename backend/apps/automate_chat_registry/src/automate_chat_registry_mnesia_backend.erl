%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_chat_registry_mnesia_backend).

-export([ start_link/0
        , count_chats/0
        , register_prefix/2
        , get_all_chat_handlers/0
        ]).

-include("records.hrl").
-define(CHAT_HANDLER_MODULE_TABLE, automate_chat_handler_module_table).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    Nodes = [node()],

    %% Live channels table
    ok = case mnesia:create_table(?CHAT_HANDLER_MODULE_TABLE,
                                  [ { attributes, record_info(fields, chat_handler_module_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, chat_handler_module_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    ignore.

-spec count_chats() -> #{ chats := number(), services := non_neg_integer() }.
count_chats() ->
    #{ chats => lists:foldl(fun(#chat_handler_module_entry{handler_module=Module},
                                Acc) ->
                                    Acc + Module:count_chats()
                            end,
                            0,
                            get_all_chat_handlers())

     , services => length(mnesia:dirty_all_keys(?CHAT_HANDLER_MODULE_TABLE))
     }.


-spec register_prefix(atom(), module()) -> ok.
register_prefix(Prefix, Module) ->
    Transaction = fun() ->
                          mnesia:write(?CHAT_HANDLER_MODULE_TABLE,
                                       #chat_handler_module_entry{ chat_prefix_id=Prefix
                                                                 , handler_module=Module
                                                                 }, write)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec get_all_chat_handlers() -> [#chat_handler_module_entry{}].
get_all_chat_handlers() ->
    Transaction = fun() ->
                          Keys = mnesia:all_keys(?CHAT_HANDLER_MODULE_TABLE),
                          {ok, lists:flatmap(fun (K) ->
                                                 mnesia:read(?CHAT_HANDLER_MODULE_TABLE, K)
                                         end, Keys)}
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
