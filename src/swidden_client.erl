-module(swidden_client).

-export([request/6]).

%% TODO(nakai): headers を指定出来るようにする

%% Host は 127.0.0.1 固定にする


-spec request(inet:port_number(), binary(), binary(), binary(), binary(), [{atom(), binary()}]) -> {ok, [{binary(), binary()}]} | {error, term()}.
request(Port, Target, Service, Version, Action, JSON) when is_integer(Port) ->
    request(integer_to_binary(Port), Target, Service, Version, Action, JSON);
request(RawPort, Target, Service, Version, Action, JSON) ->
    URL = <<"http://127.0.0.1", $:, RawPort/binary, $/>>,
    Headers = [{Target, list_to_binary([Service, $_, Version, $., Action])}],
    RawJSON = jsonx:encode(JSON),
    Options = [],
    case hackney:post(URL, Headers, RawJSON, Options) of
        {ok, 200, _RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            ok = hackney:close(ClientRef),
            {ok, jsonx:decode(Body)};
        {ok, 400, _RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            ok = hackney:close(ClientRef),
            {ok, jsonx:decode(Body)};
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            ok = hackney:close(ClientRef),
            {error, {status_code, StatusCode}}
    end.
