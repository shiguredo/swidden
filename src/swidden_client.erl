-module(swidden_client).

-export([request/5, request/6]).

%% TODO(nakai): headers を指定出来るようにする

%% Host は 127.0.0.1 固定にする


request(Port, Target, Service, Version, Operation) ->
    request0(Port, Target, Service, Version, Operation, []).


request(Port, Target, Service, Version, Operation, JSON) ->
    RawJSON = jsone:encode(JSON),
    request0(Port, Target, Service, Version, Operation, RawJSON).


request0(Port, Target, Service, Version, Operation, RawJSON) when is_integer(Port) ->
    request0(integer_to_binary(Port), Target, Service, Version, Operation, RawJSON);
request0(RawPort, Target, Service, Version, Operation, RawJSON) ->
    URL = <<"http://127.0.0.1", $:, RawPort/binary, $/>>,
    Headers = [{Target, list_to_binary([Service, $_, Version, $., Operation])}],
    Options = [],
    case hackney:post(URL, Headers, RawJSON, Options) of
        {ok, StatusCode, _RespHeaders, ClientRef} when StatusCode =:= 200 orelse StatusCode =:= 400 ->
            case hackney:body(ClientRef) of
                {ok, <<>>} ->
                    {ok, StatusCode};
                {ok, Body} ->
                    {ok, StatusCode, jsone:decode(Body, [{format, proplist}])}
            end;
        {ok, StatusCode, _RespHeaders, _ClientRef} ->
            {error, {status_code, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.
