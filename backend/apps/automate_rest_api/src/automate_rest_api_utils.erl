-module(automate_rest_api_utils).
-export([ read_body/1
        , send_json_output/2
        , send_json_format/1
        ]).

read_body(Req0) ->
    read_body(Req0, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

send_json_output(Output, Req) ->
    Res1 = cowboy_req:set_resp_body(Output, Req),
    Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
    cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2).

send_json_format(Req) ->
    Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1).
