-module(sample_middleware).

-behaviour(cowboy_middleware).

-export([execute/2]).


execute(Req, Env) ->
    Req0 = set_cors_headers(Req),
    case cowboy_req:method(Req) of
        <<"OPTIONS">> ->
            Req1 = cowboy_req:reply(200, Req0),
            {stop, Req1};
        _ ->
            {ok, Req0, Env}
    end.


set_cors_headers(Req) ->
    Headers = [{<<"access-control-allow-origin">>, <<"*">>},
               {<<"access-control-allow-methods">>, <<"POST, OPTIONS">>},
               {<<"access-control-allow-headers">>,
                <<"Origin, X-Requested-With, Content-Type, Accept, x-sora-target">>},
               {<<"access-control-max-age">>, <<"1000">>}],
    set_headers(Headers, Req).


set_headers(Headers, Req) ->
    F = fun({Header, Value}, Req0) ->
                cowboy_req:set_resp_header(Header, Value, Req0)
        end,
    lists:foldl(F, Req, Headers).
