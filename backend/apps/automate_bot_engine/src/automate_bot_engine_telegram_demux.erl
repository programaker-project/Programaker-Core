%%%-------------------------------------------------------------------
%%% @author kenkeiras <kenkeiras@becho>
%%% @copyright (C) 2018, kenkeiras
%%% @doc
%%%
%%% @end
%%% Created : 11 Jun 2018 by kenkeiras <kenkeiras@becho>
%%%-------------------------------------------------------------------
-module(automate_bot_engine_telegram_demux).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, { bot_name }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(string()) -> {ok, Pid :: pid()} |
                              {error, Error :: {already_started, pid()}} |
                              {error, Error :: term()} |
                              ignore.
start_link(BotName) ->
    gen_server:start_link( {local, ?SERVER}, ?MODULE
                         , [BotName]
                         , []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init([BotName]) ->
    process_flag(trap_exit, true),
    pe4kin_receiver:subscribe(BotName, self()),
    pe4kin_receiver:start_http_poll(BotName, #{limit=>100, timeout=>60}),
    {ok, #state{ bot_name=BotName }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    io:format("[Telegram Demux][Call] ~p~n", [_Request]),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    io:format("[Telegram Demux][Cast] ~p~n", [_Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({pe4kin_update, BotName, Message}, State) ->
    handle_telegram_update({pe4kin_update, BotName, Message}),
    {noreply, State};

handle_info(_Info, State) ->
    io:format("Info ~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_telegram_update({pe4kin_update,
                        BotName,
                        Message = #{<<"message">> :=
                                        #{<<"chat">> := #{ <<"id">> := ChatId },
                                          <<"date">> := _MessageDate,
                                          <<"from">> := #{ <<"id">> := FromId },
                                          <<"message_id">> := _MessageId,
                                          <<"text">> := Content},
                                    <<"update_id">> := _UpdateId}}) ->
    case automate_bot_engine_telegram:telegram_user_to_internal(FromId) of
        {ok, InternalUser} ->
            automate_bot_engine_launcher:user_sent_telegram_message(InternalUser, ChatId, Content, BotName);
        {error, not_found} ->
            handle_from_new_user(Message, Content, FromId, ChatId, BotName)
    end;

handle_telegram_update({pe4kin_update, _BotName, Message}) ->
    io:format("[Telegram Demux]Unknown message format: ~p~n", [Message]).

handle_from_new_user(_Message, <<"/register ", RawRegistrationToken/binary>>, UserId, ChatId, BotName) ->
    RegistrationToken = string:trim(RawRegistrationToken, both, " \n"),
    case automate_bot_engine_telegram:register_user(UserId, RegistrationToken) of
        ok ->
            ResponseText = <<"Welcome! You're registered!\nNow you can use this bot in your programs.">>,
            {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => ResponseText});
        {error, not_found} ->
            ResponseText = <<"Hm... We didn't found a match for that token.\nMaybe check your spelling ;)">>,
            {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => ResponseText})
    end;

handle_from_new_user(_Message, <<"/start">>, _UserId, ChatId, BotName) ->
    ResponseText = <<"Hi! I'm a bot in the making, ask @kenkeiras for more info if you want to know how to program me ;).">>,
    {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => ResponseText});

handle_from_new_user(_Message, _Content, _UserId, ChatId, BotName) ->
    ResponseText = <<"Sorry, I didnt get that!\nSay /start to see what can I do.">>,
    {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => ResponseText}).

