-module(automate_monitor_engine).

%% API
-export([ get_last_monitor_result/1
        , get_monitor_result/2
        ]).

-define(SERVER, ?MODULE).
-include("../../automate_storage/src/records.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec get_last_monitor_result(binary()) -> {ok, any()} | {error, not_found}.
get_last_monitor_result(MonitorId) ->
    case automate_storage:get_monitor_from_id(MonitorId) of
        #monitor_entry{ type=Type, value=Value } ->
            Result = get_monitor_result(Type, Value),
            io:format("Result: ~p~n", [Result]),
            Result;
        {error, _} ->
            {error, not_found}
    end.

-spec get_monitor_result(binary(), any()) -> {ok, binary()}.
get_monitor_result(<<"xpath_v1">>, #{ <<"url">> := Url
                                    , <<"xpath">> := XPath
                                    }) ->
    io:format("Querying ~p for ~p~n", [Url, XPath]),
    %% TODO modularize this
    %% This is done in a separate thread to allow the GC clean it up fast as it
    %% may have a relatively large memory footprint
    Orig = self(),
    process_flag(trap_exit, true),
    spawn_link(fun() ->
                       Result = resolve_xpath(Url, XPath),
                       Orig ! Result
               end),
    receive
        {'EXIT', _, _} ->
            {error, not_found};
        {error, not_found} ->
            {error, not_found};
        {ok, Result} ->
            {ok, Result};
        X ->
            X
    end;

get_monitor_result(Type, Value) ->
    io:format("Unknown monitor: ~p~n  ~p~n", [Type, Value]),
    erlang:error(badarg).


%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec resolve_xpath(binary(), binary()) -> {ok, binary()} | {error, _}.
resolve_xpath(Url, XPath) ->
    case httpc:request(binary_to_list(Url)) of
        {ok, {_Status, _Headers, Body}} ->
            BodyUtf8 = fix_utf8(Body),
            Tree = mochiweb_html:parse(BodyUtf8),
            Contents = mochiweb_xpath:execute(binary_to_list(XPath), Tree),
            contents_to_result(Contents);
        Error = {error, _} ->
            Error
    end.

fix_utf8(S) ->
    binary_to_list(unicode:characters_to_binary(S, unicode)).

contents_to_result([]) ->
    {error, not_found};

contents_to_result([Element | _]) ->
    content_node_to_result(Element).

content_node_to_result({<<"img">>, Params, _Content}) ->
    case lists:keyfind(<<"src">>, 1, Params) of
        false ->
            case lists:keyfind(<<"alt">>, 1, Params) of
                false ->
                    {error, not_found};
                {<<"alt">>, Alt} ->
                    {ok, Alt}
            end;
        {<<"src">>, Src} ->
            {ok, Src}
    end;

content_node_to_result({_, _, List}) when length(List) > 0 ->
    {ok, binary:list_to_bin(lists:flatten(List))};

content_node_to_result(X) ->
    io:format("Result node: ~p~n", [X]),
    {ok, X}.

