%%%-------------------------------------------------------------------
%% @doc automate_configuration module
%% @end
%%%-------------------------------------------------------------------

-module(automate_configuration).

-export([get_table_wait_time/0]).

-define(APPLICATION, automate).
-define(DEFAULT_WAIT_TIME, 10000).

%%====================================================================
%% Utils functions 
%%====================================================================
-spec get_table_wait_time() -> non_neg_integer().
get_table_wait_time() ->
    application:get_env(?APPLICATION, table_wait_time, ?DEFAULT_WAIT_TIME).
