-module(swidden_client).

-feature(maybe_expr, enable).

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


request0(Port, Headers0, Target, Service, Version, Operation, RawJson) when is_integer(Port) ->
    ReqHeaders = [{Target, list_to_binary([Service, $_, Version, $., Operation])} | Headers0],
    Hostname = {127, 0, 0, 1},
    maybe
        {ok, ConnPid} ?= gun:open(Hostname,
                                  Port,
                                  #{
                                    %% リトライはしない
                                    retry => 0,
                                    %% http/1.1
                                    protocols => [http],
                                    %% http
                                    transport => tcp
                                   }),
        {ok, _Protocols} ?= gun:await_up(ConnPid),
        StreamRef = gun:post(ConnPid,
                             <<$/>>,
                             [{<<"content-type">>, <<"application/json">>} | ReqHeaders],
                             RawJson),
        {response, nofin, Status} ?= case gun:await(ConnPid, StreamRef) of
                                         {response, nofin, Status0, _} when Status0 =:= 200 orelse
                                                                            Status0 =:= 400 orelse
                                                                            Status0 =:= 403 ->
                                             {response, nofin, Status0};
                                         {response, nofin, Status0, _} ->
                                             {error, {status_code, Status0}};
                                         {response, fin, Status0, _} ->
                                             {response, fin, Status0};
                                         {error, Reason0} ->
                                             {error, Reason0}
                                     end,
        {ok, Body} ?= gun:await_body(ConnPid, StreamRef),
        ok = gun:shutdown(ConnPid),
        ok = gun:close(ConnPid),
        {ok, Status, jsone:decode(Body)}
    else
        {response, fin, Status1} ->
            {ok, Status1};
        {error, Reason} ->
            {error, Reason}
    end.
