%%%-------------------------------------------------------------------
%% @doc automate_chat_registry API.
%% @end
%%%-------------------------------------------------------------------

-module(automate_chat_registry).

%% API
-export([ count_chats/0
        , register_prefix/2
        , get_all_chats_for_user/1
        ]).

-define(SERVER, ?MODULE).
-include("records.hrl").
-define(BACKEND, automate_chat_registry_mnesia_backend).

%%====================================================================
%% API functions
%%====================================================================

-spec count_chats() -> number().
count_chats() ->
    ?BACKEND:count_chats().

-spec register_prefix(atom(), module()) -> ok.
register_prefix(Prefix, Module) ->
    ?BACKEND:register_prefix(Prefix, Module).

-spec get_all_chats_for_user(binary()) -> {ok, [#chat_entry{}]}.
get_all_chats_for_user(UserId) ->
    {ok, Handlers} = ?BACKEND:get_all_chat_handlers(),
    {ok, lists:flatmap(fun (#chat_handler_module_entry{ handler_module=Module }) ->
                               {ok, Chats} = Module:get_chats_for_user(UserId),
                               Chats
                       end, Handlers)}.

%%====================================================================
%% Internal functions
%%====================================================================
