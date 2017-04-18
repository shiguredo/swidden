-module(swidden_middleware).

-export([failure/2, failure/3]).


failure(Req, Type) when is_binary(Type) ->
    cowboy_req:reply(400, [{<<"content-type">>, <<"application/json">>}],
                     jsone:encode(#{error_type => Type}), Req).


failure(Req, Type, Reason) when is_binary(Type) andalso is_map(Reason) ->
    cowboy_req:reply(400, [{<<"content-type">>, <<"application/json">>}],
                     jsone:encode(#{error_type => Type, error_reason => Reason}), Req).

