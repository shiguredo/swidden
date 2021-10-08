-module(swidden_client).

-export([request/5, request/6]).
-export([request_with_headers/6, request_with_headers/7]).

%% Host は 127.0.0.1 固定にする


request(Port, Target, Service, Version, Operation) ->
    request0(Port, [], Target, Service, Version, Operation, []).


request(Port, Target, Service, Version, Operation, Json) ->
    RawJson = jsone:encode(Json),
    request0(Port, [], Target, Service, Version, Operation, RawJson).


request_with_headers(Port, Headers, Target, Service, Version, Operation) ->
    request0(Port, Headers, Target, Service, Version, Operation, []).

request_with_headers(Port, Headers, Target, Service, Version, Operation, Json) ->
    RawJson = jsone:encode(Json),
    request0(Port, Headers, Target, Service, Version, Operation, RawJson).


request0(Port, Headers, Target, Service, Version, Operation, RawJson) when is_integer(Port) ->
    request0(integer_to_binary(Port), Headers, Target, Service, Version, Operation, RawJson);
request0(RawPort, Headers0, Target, Service, Version, Operation, RawJson) ->
    URL = <<"http://127.0.0.1", $:, RawPort/binary, $/>>,
    Headers = [{Target, list_to_binary([Service, $_, Version, $., Operation])}|Headers0],
    Options = [{pool, false}],
    case hackney:post(URL, Headers, RawJson, Options) of
        {ok, StatusCode, _RespHeaders, ClientRef} when StatusCode =:= 200 orelse
                                                       StatusCode =:= 400 orelse
                                                       StatusCode =:= 403 ->
            case hackney:body(ClientRef) of
                {ok, <<>>} ->
                    hackney:close(ClientRef),
                    {ok, StatusCode};
                {ok, Body} ->
                    hackney:close(ClientRef),
                    {ok, StatusCode, jsone:decode(Body)}
            end;
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            hackney:close(ClientRef),
            {error, {status_code, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.
