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
                        , {"/api/v0/ping", automate_rest_api_ping, []}

                          %% Administration
                        , {"/api/v0/admin/stats", automate_rest_api_admin_stats_root, []}

                          %% Assets
                        , {"/api/v0/assets/icons/[...]", cowboy_static, {dir, automate_configuration:asset_directory("public/icons")}}

                          %% Registration
                        , {"/api/v0/sessions/register", automate_rest_api_sessions_register, []}
                        , {"/api/v0/sessions/register/verify", automate_rest_api_sessions_register_verify, []}

                          %% Session management
                        , {"/api/v0/sessions/check", automate_rest_api_sessions_check, []}
                        , {"/api/v0/sessions/login", automate_rest_api_sessions_login, []}
                        , {"/api/v0/sessions/login/reset", automate_rest_api_sessions_reset_password, []}
                        , {"/api/v0/sessions/login/reset/validate", automate_rest_api_sessions_reset_password_validate, []}
                        , {"/api/v0/sessions/login/reset/update", automate_rest_api_sessions_reset_password_update, []}

                          %% Users
                        , {"/api/v0/users", automate_rest_api_users_root, []}
                        , {"/api/v0/users/:user_id", automate_rest_api_users_specific, []}

                          %% Miscellaneous
                        , {"/api/v0/users/id/:user_id/custom_signals/", automate_rest_api_custom_signals_root, []}

                        , {"/api/v0/users/id/:user_id/templates/", automate_rest_api_templates_root, []}
                        , {"/api/v0/users/id/:user_id/templates/id/:template_id", automate_rest_api_templates_specific, []}
                        , {"/api/v0/users/:user_id/custom-blocks/", automate_rest_api_custom_blocks_root, []}

                          %% Settings
                        , {"/api/v0/users/id/:user_id/settings", automate_rest_api_user_settings, []}

                          %% Programs
                        , {"/api/v0/programs/id/:program_id", automate_rest_api_program_specific_by_id, []}

                        , {"/api/v0/users/:user_id/programs", automate_rest_api_programs_root, []}
                        , {"/api/v0/users/:user_id/programs/:program_id", automate_rest_api_programs_specific, []}
                        , {"/api/v0/users/id/:user_id/programs/id/:program_id/communication", automate_rest_api_program_specific_logs_stream, []}
                        , {"/api/v0/users/id/:user_id/programs/id/:program_id/logs-stream", automate_rest_api_program_specific_logs_stream, []}
                        , {"/api/v0/users/id/:user_id/programs/id/:program_id/editor-events", automate_rest_api_program_specific_editor_events, []}
                          %% Program operation
                        , {"/api/v0/users/id/:user_id/programs/id/:program_id/logs", automate_rest_api_program_logs, []}
                        , {"/api/v0/users/id/:user_id/programs/id/:program_id/tags", automate_rest_api_program_tags, []}
                        , {"/api/v0/users/id/:user_id/programs/id/:program_id/stop-threads", automate_rest_api_program_stop, []}
                        , {"/api/v0/users/id/:user_id/programs/id/:program_id/status", automate_rest_api_program_status, []}

                          %% Connection management
                        , {"/api/v0/users/id/:user_id/connections/available", automate_rest_api_connections_available_root, []}
                        , {"/api/v0/users/id/:user_id/connections/established", automate_rest_api_connections_established_root, []}
                        , {"/api/v0/users/id/:user_id/connections/pending/:connection_id/wait", automate_rest_api_connections_pending_wait, []}

                          %% Bridges
                        , {"/api/v0/users/:user_id/bridges", automate_rest_api_service_ports_root, []}
                        , {"/api/v0/users/id/:user_id/bridges/id/:bridge_id", automate_rest_api_service_ports_specific, []}
                        , {"/api/v0/users/id/:user_id/bridges/id/:bridge_id/callback/:callback", automate_rest_api_bridge_callback, []}
                        , {"/api/v0/users/id/:user_id/bridges/id/:bridge_id/functions/:function", automate_rest_api_bridge_function_specific, []}
                        , {"/api/v0/users/id/:user_id/bridges/id/:bridge_id/signals", automate_rest_api_bridge_signal_root, []}
                        , {"/api/v0/users/id/:user_id/bridges/id/:bridge_id/signals/:key", automate_rest_api_bridge_signal_specific, []}
                        , {"/api/v0/users/id/:user_id/bridges/id/:service_port_id/communication"
                          , automate_rest_api_service_ports_specific_communication, []}
                        , {"/api/v0/users/id/:user_id/bridges/id/:service_port_id/oauth_return"
                          , automate_rest_api_service_port_oauth_return, []}

                          %% Services
                        , {"/api/v0/users/:user_id/services", automate_rest_api_services_root, []}
                        , {"/api/v0/users/:user_id/services/id/:service_id/how-to-enable", automate_rest_api_services_how_to_enable, []}
                        , {"/api/v0/users/:user_id/services/id/:service_id/register", automate_rest_api_services_register, []}

                          %% Monitor
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
