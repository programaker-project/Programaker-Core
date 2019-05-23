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
-include("../../automate_common_types/src/definitions.hrl").

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({global, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, BotName} = application:get_env(?APPLICATION, telegram_bot_name),
    {ok, BotToken} = application:get_env(?APPLICATION, telegram_token),

    {ok, { {one_for_all, ?AUTOMATE_SUPERVISOR_INTENSITY, ?AUTOMATE_SUPERVISOR_PERIOD},
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
           , # { id => automate_services_telegram_storage
               , start => {automate_services_telegram_storage, start_link, []}
               , restart => permanent
               , shutdown => 2000
               , type => worker
               , modules => [automate_services_telegram_storage]
               }
           ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
