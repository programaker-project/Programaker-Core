%%%-------------------------------------------------------------------
%% @doc automate_services_telegram top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(automate_services_telegram_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(APPLICATION, automate_services_telegram).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, BotName} = application:get_env(?APPLICATION, telegram_bot_name),
    {ok, BotToken} = application:get_env(?APPLICATION, telegram_token),

    {ok, { {one_for_all, 0, 1},
           [ #{ id => pe4kin_telegram_handler
              , start => {pe4kin, launch_bot, [BotName, BotToken, #{receiver => true}]}
              , restart => permanent
              , shutdown => 2000
              , type => worker
              , modules => [pe4kin_telegram_handler]
              }
           , # { id => automate_services_telegram_demux
               , start => {automate_services_telegram_demux, start_link, [BotName]}
               , restart => permanent
               , shutdown => 2000
               , type => worker
               , modules => [automate_services_telegram_demux]
               }
           ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
