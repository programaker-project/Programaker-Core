-module(automate_service_port_engine_test_utils).

-define(BACKEND, automate_service_port_engine_mnesia_backend).

-export([ establish_connection/2
        , create_random_user/0
        , set_admin_user/1
        ]).

establish_connection(BridgeId, UserId) ->
    case ?BACKEND:gen_pending_connection(BridgeId, UserId) of
        {ok, ConnectionId} ->
            ok = ?BACKEND:establish_connection(BridgeId, UserId, ConnectionId, <<"test connection">>),
            {ok, ConnectionId};
        {error, Reason} ->
            {error, Reason}
    end.


create_random_user() ->
    Username = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    Password = undefined,
    Email = binary:list_to_bin(uuid:to_string(uuid:uuid4())),

    {ok, UserId} = automate_storage:create_user(Username, Password, Email, ready),
    {Username, {user, UserId}}.


set_admin_user({user, UserId}) ->
    automate_storage:promote_user_to_admin(UserId).
