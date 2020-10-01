-module(automate_service_port_engine_test_utils).

-define(BACKEND, automate_service_port_engine_mnesia_backend).

-export([ establish_connection/2
        ]).

establish_connection(BridgeId, UserId) ->
    {ok, ConnectionId} = ?BACKEND:gen_pending_connection(BridgeId, UserId),
    ok = ?BACKEND:establish_connection(BridgeId, UserId, ConnectionId, <<"test connection">>),
    {ok, ConnectionId}.
