%%% @doc
%%% Auto-mate REST API server.
%%% @end

-module(automate_rest_api_server).

%% API
-export([start_link/0]).

-define(PORT_ENV_VARIABLE, "AUTOMATE_HTTP_PORT").

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    Dispatch = cowboy_router:compile(
                 [{'_', [ %% Metrics
                          {"/metrics", automate_rest_api_metrics, []}

                          %% API
                        , {"/api/v0/sessions/register", automate_rest_api_sessions_register, []}
                        , {"/api/v0/sessions/check", automate_rest_api_sessions_check, []}
                        , {"/api/v0/sessions/login", automate_rest_api_sessions_login, []}

                        , {"/api/v0/ping", automate_rest_api_ping, []}

                        , {"/api/v0/users", automate_rest_api_users_root, []}
                        , {"/api/v0/users/:user_id", automate_rest_api_users_specific, []}

                        , {"/api/v0/users/id/:user_id/templates/", automate_rest_api_templates_root, []}
                        , {"/api/v0/users/id/:user_id/templates/id/:template_id", automate_rest_api_templates_specific, []}
                        , {"/api/v0/users/:user_id/custom-blocks/", automate_rest_api_custom_blocks_root, []}
                        , {"/api/v0/users/:user_id/programs", automate_rest_api_programs_root, []}
                        , {"/api/v0/users/:user_id/programs/:program_id", automate_rest_api_programs_specific, []}
                        , {"/api/v0/users/id/:user_id/programs/id/:program_id/tags", automate_rest_api_program_tags, []}
                        , {"/api/v0/users/id/:user_id/programs/id/:program_id/stop-threads", automate_rest_api_program_stop, []}
                        , {"/api/v0/users/id/:user_id/programs/id/:program_id/status", automate_rest_api_program_status, []}
                        , {"/api/v0/users/:user_id/bridges/", automate_rest_api_service_ports_root, []}
                        , {"/api/v0/users/id/:user_id/bridges/id/:bridge_id", automate_rest_api_service_ports_specific, []}
                        , {"/api/v0/users/id/:user_id/bridges/id/:bridge_id/callback/:callback", automate_rest_api_bridge_callback, []}
                        , {"/api/v0/users/id/:user_id/bridges/id/:service_port_id/communication"
                          , automate_rest_api_service_ports_specific_communication, []}
                        , {"/api/v0/users/id/:user_id/bridges/id/:service_port_id/oauth_return"
                          , automate_rest_api_service_port_oauth_return, []}

                        , {"/api/v0/users/:user_id/services", automate_rest_api_services_root, []}
                        , {"/api/v0/users/:user_id/services/id/:service_id/how-to-enable", automate_rest_api_services_how_to_enable, []}
                        , {"/api/v0/users/:user_id/services/id/:service_id/register", automate_rest_api_services_register, []}
                        , {"/api/v0/users/:user_id/monitors", automate_rest_api_monitors_root, []}
                        ]}
                 ]),

    Port = get_port(),
    Start = cowboy:start_clear(http, [{port, Port}],
                               #{
                                 env => #{dispatch => Dispatch}
                                }
                              ),

    case Start of
        {ok, Pid} ->
            {ok,
             spawn(fun () -> handler(Pid) end)};

        %% For debug purposes
        {error, eaddrinuse} ->
            {ok, spawn(fun() -> handler(none) end)}
    end.



%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Wrap cowboy HTTP server to be supervisor-friendly.
%%      Just wait for an exit call, and stop the cowboy HTTP server.
%% @end
handler(none) ->
    %% For debug purposes
    try timer:sleep(infinity)
    catch exit:_Reason ->
            ok
    end;

handler(Pid) ->
    %% Wait for an exit call
    try timer:sleep(infinity)
    catch exit:_Reason ->
            %% Stop the server
            cowboy:stop_listener(Pid)
    end.


get_port() ->
    case os:getenv(?PORT_ENV_VARIABLE) of
        false ->
            {ok, Port} = application:get_env(automate_rest_api, port),
            Port;
        Value ->
            list_to_integer(Value)
    end.
