-module(swidden_client).

-export([request/6]).

%% TODO(nakai): headers を指定出来るようにする

%% Host は 127.0.0.1 固定にする


-spec request(inet:port_number(), binary(), binary(), binary(), binary(), [{atom(), binary()}]) -> {ok, [{binary(), binary()}]} | {error, term()}.
request(Port, Target, Service, Version, Operation, JSON) when is_integer(Port) ->
    request(integer_to_binary(Port), Target, Service, Version, Operation, JSON);
request(RawPort, Target, Service, Version, Operation, JSON) ->
    URL = <<"http://127.0.0.1", $:, RawPort/binary, $/>>,
    Headers = [{Target, list_to_binary([Service, $_, Version, $., Operation])}],
    RawJSON = jsonx:encode(JSON),
    Options = [],
    case hackney:post(URL, Headers, RawJSON, Options) of
        {ok, StatusCode, _RespHeaders, ClientRef} when StatusCode =:= 200 orelse StatusCode =:= 400 ->
            {ok, Body} = hackney:body(ClientRef),
            {ok, StatusCode, jsonx:decode(Body)};
        {ok, StatusCode, _RespHeaders, _ClientRef} ->
            {error, {status_code, StatusCode}}
    end.
