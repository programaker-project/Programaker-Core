-module(automate_rest_api_cors).

-export([set_headers/1]).

set_headers(Req) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, POST, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"Access-Control-Max-Age">>, <<"3600">>, Req2),
    Req4 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>,
                                      <<"authorization, content-type, xsrf-token">>, Req3),
    Req5 = cowboy_req:set_resp_header(<<"Access-Control-Expose-Headers">>,
                                      <<"xsrf-token">>, Req4),
    Req5.
