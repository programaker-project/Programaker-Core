%%%-------------------------------------------------------------------
%% @doc automate_coordination utils
%% @end
%%%-------------------------------------------------------------------

-module(automate_coordination_utils).

%% Module API
-export([ is_process_alive/1
        , is_process_alive/2
        ]).

%% Check if a (possibly remote) process is alive
-spec is_process_alive(pid()) -> boolean().
is_process_alive(Pid) ->
    is_process_alive(Pid, node(Pid)).

-spec is_process_alive(pid(), node()) -> boolean().
is_process_alive(Pid, Node) ->
    case rpc:call(Node, erlang, is_process_alive, [Pid]) of
        true -> true;
        false -> false;
        {error, nodedown} -> false
    end.
