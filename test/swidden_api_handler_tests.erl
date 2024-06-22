-module(swidden_api_handler_tests).

-include("swidden.hrl").

-include_lib("eunit/include/eunit.hrl").

%% 下記二つを使ったテスト
%% swidden/priv/swidden/dispatch.conf
%% swidden/priv/swidden/schemas

-define(APPS, [gun, ranch, cowlib, cowboy, swidden]).


start_apps() ->
    [ application:ensure_all_started(App) || App <- ?APPS ].


stop_apps() ->
    [ application:stop(App) || App <- lists:reverse(?APPS) ].


all_test_() ->
    {foreach,
     fun() ->
             start_apps(),
             ok
     end,
     fun(_) ->
             stop_apps(),
             ok
     end,
     [fun success/0,
      fun services_success/0,
      fun failure/0,
      fun middlewares/0,
      fun redirect/0,
      fun interceptor/0,
      fun crash/0]}.


success() ->
    ?assertMatch({ok, _Pid}, swidden:start(swidden, [{port, 40000}])),

    ?assertEqual(200,
                 request(<<"Spam">>,
                         <<"20141101">>,
                         <<"GetUser">>,
                         #{username => <<"yakihata">>})),
    ?assertEqual(200,
                 request(<<"Spam">>,
                         <<"20141101">>,
                         <<"CreateUser">>,
                         [{username, <<"yakihata">>}, {password, <<"nogyo">>}])),
    ?assertEqual(200,
                 request(<<"Spam">>,
                         <<"20141101">>,
                         <<"UpdateUser">>,
                         [{username, <<"yakihata">>}, {password, <<"nogyo">>}])),
    ?assertEqual(200,
                 request(<<"Spam">>,
                         <<"20141101">>,
                         <<"DeleteUser">>,
                         [{username, <<"yakihata">>}])),
    ?assertEqual(200, request(<<"Spam">>, <<"20141101">>, <<"ListUsers">>)),

    %% JSON あり、ヘッダー追加バージョン
    ?assertEqual(200,
                 request_with_headers([{<<"x-swidden-token">>, <<"token">>}],
                                      <<"Spam">>,
                                      <<"20141101">>,
                                      <<"GetUser">>,
                                      #{username => <<"yakihata">>})),

    %% JSON なし、ヘッダー追加バージョン
    ?assertEqual(200,
                 request_with_headers([{<<"x-swidden-token">>, <<"token">>}],
                                      <<"Spam">>,
                                      <<"20141101">>,
                                      <<"ListUsers">>)),

    ?assertEqual(400,
                 request(<<"Spam">>,
                         <<"20150701">>,
                         <<"CreateUser">>,
                         #{username => <<"yakihata">>, password => <<"nogyo">>, group => <<"amazon">>})),

    ?assertEqual(400,
                 request(<<"Spam">>,
                         <<"20150701">>,
                         <<"CreateUser">>,
                         #{username => <<"error_code">>, password => <<"nogyo">>, group => <<"amazon">>})),

    ?assertEqual(200,
                 request(<<"SpamAdmin">>,
                         <<"20141101">>,
                         <<"GetMetrics">>,
                         #{reset => false})),

    ?assertEqual(ok, swidden:stop(40000)),
    ok.


services_success() ->
    ?assertMatch({ok, _Pid1}, swidden:start(swidden, [{port, 40000}, {services, [<<"Spam">>]}])),
    ?assertMatch({ok, _Pid2}, swidden:start(swidden, [{port, 50000}, {services, [<<"SpamAdmin">>]}])),

    ?assertEqual(200, request(40000, <<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"yakihata">>}])),
    ?assertEqual(400, request(50000, <<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"yakihata">>}])),

    ?assertEqual(200, request(50000, <<"SpamAdmin">>, <<"20141101">>, <<"GetMetrics">>, [{reset, false}])),
    ?assertEqual(400, request(40000, <<"SpamAdmin">>, <<"20141101">>, <<"GetMetrics">>, [{reset, false}])),

    ?assertEqual(ok, swidden:stop(40000)),
    ?assertEqual(ok, swidden:stop(50000)),
    ok.


failure() ->
    ?assertMatch({ok, _Pid}, swidden:start(swidden, [{port, 40000}])),
    %% よくわからないサービス
    ?assertEqual(400, request(<<"Bacon">>, <<"20141101">>, <<"GetUser">>, [{username, <<"yakihata">>}])),
    %% Body なしで送る
    ?assertEqual(400, no_body_request(<<"Bacon">>, <<"20141101">>, <<"GetUser">>)),

    %% PUT メソッドで送る
    ?assertEqual(400, put_method_request(<<"Bacon">>, <<"20141101">>, <<"GetUser">>, [{username, <<"yakihata">>}])),

    %% x-swd-target ヘッダーなし
    ?assertEqual(400, no_header_request([{username, <<"yakihata">>}])),
    %% x-swd-target ヘッダーの値がおかしい
    ?assertEqual(400, bad_header_request([{username, <<"yakihata">>}])),
    %% Body が空を期待しているのに Body を送った場合
    ?assertEqual(400, request(<<"Spam">>, <<"20141101">>, <<"ListUsers">>, [{type, <<"all">>}])),
    %% JSON ですらない値を送った場合
    ?assertEqual(400, raw_payload_request(<<"Spam">>, <<"20141101">>, <<"GetUser">>, <<"abc">>)),

    %% %% JSON ですらない値を送った場合
    %% ?assertEqual(400, raw_payload_request(<<"Spam">>, <<"20141101">>, <<"GetUser">>, <<"">>)),

    ?assertEqual(ok, swidden:stop(40000)),
    ok.


middlewares() ->
    ?assertMatch({ok, _Pid},
                 swidden:start(swidden,
                               [{middlewares, [cowboy_router,
                                               sample_middleware,
                                               cowboy_handler]},
                                {port, 40000}])),

    ?assertEqual(200, request(<<"SpamAdmin">>, <<"20141101">>, <<"GetMetrics">>, [{reset, false}])),

    ?assertEqual(200, request(<<"Spam">>, <<"20141101">>, <<"GetAuthenticatedUser">>)),
    ?assertEqual(200, request(<<"Spam">>, <<"20141101">>, <<"UpdateAuthenticatedUser">>, [{username, <<"NewName">>}])),

    ?assertEqual(400, request(<<"Spam">>, <<"20141101">>, <<"UpdateAuthenticatedUser">>, [{bad_key, <<"NewName">>}])),

    ?assertEqual(ok, swidden:stop(40000)),
    ok.


redirect() ->
    ?assertMatch({ok, _Pid}, swidden:start(swidden, [{port, 40000}])),

    ?assertEqual(307, request(<<"Spam">>, <<"20141101">>, <<"Redirect">>)),

    ?assertEqual(ok, swidden:stop(40000)),
    ok.


crash() ->
    ?assertMatch({ok, _Pid}, swidden:start(swidden, [{port, 40000}])),

    ?assertEqual(400, request(<<"Spam">>, <<"20141101">>, <<"Crash">>)),

    ?assertEqual(ok, swidden:stop(40000)),
    ok.


interceptor() ->
    ?assertMatch({ok, _Pid}, swidden:start(swidden, [{port, 40000}, {interceptor, sample_interceptor}])),

    %% 素通しする
    ?assertEqual({200,
                  #{
                    <<"password">> => <<"password">>,
                    <<"good_or_bad">> => <<"good">>
                   }},
                 request2(<<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"Hermione">>}])),

    %% ok
    ?assertEqual(200,
                 request2(<<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"Ron">>}])),
    %% ok, JSON 付き
    ?assertEqual({200,
                  #{
                    <<"everyboby">> => <<"know him">>,
                    <<"good_or_bad">> => <<"good">>
                   }},
                 request2(<<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"Harry">>}])),

    %% リダイレクト
    ?assertEqual({307, <<"http://example.com/albus?foo=bar">>},
                 request2(<<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"Dumbledore">>}])),

    %% エラー
    ?assertEqual({400, #{<<"error_type">> => <<"He-Who-Must-Not-Be-Named, You-Know-Who">>}},
                 request2(<<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"Voldemort">>}])),
    %% エラー, JSON 付き
    ?assertEqual({400,
                  #{
                    <<"error_type">> => <<"insufficient privilege">>,
                    <<"error_reason">> => #{
                                            <<"caution">> => <<"Slytherin only">>,
                                            <<"good_or_bad">> => <<"bad">>
                                           }
                   }},
                 request2(<<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"Snape">>}])),

    %% 引数なしパターン
    ?assertEqual({200,
                  [#{
                     <<"password">> => <<"password">>,
                     <<"username">> => <<"username">>
                    }]},
                 request2(<<"Spam">>, <<"20141101">>, <<"ListUsers">>)),
    ?assertEqual({400, #{<<"error_type">> => <<"not allowed">>}},
                 request2(<<"Spam">>, <<"20141101">>, <<"Redirect">>)),

    ?assertEqual(ok, swidden:stop(40000)),
    ok.


request(Service, Version, Operation, JSON) ->
    request(40000, Service, Version, Operation, JSON).


request(Port, Service, Version, Operation, JSON) ->
    case swidden_client:request(Port, <<"x-swd-target">>, Service, Version, Operation, JSON) of
        {ok, StatusCode} ->
            StatusCode;
        {ok, StatusCode, _Body} ->
            StatusCode;
        {error, {status_code, StatusCode}} ->
            StatusCode
    end.


request(Service, Version, Operation) ->
    case swidden_client:request(40000, <<"x-swd-target">>, Service, Version, Operation) of
        {ok, StatusCode} ->
            StatusCode;
        {ok, StatusCode, _Body} ->
            StatusCode;
        {error, {status_code, StatusCode}} ->
            StatusCode
    end.


request2(Service, Version, Operation) ->
    request2(Service, Version, Operation, <<>>).


request2(Service, Version, Operation, Json) when is_list(Json) ->
    Body = jsone:encode(Json),
    request2(Service, Version, Operation, Body);
request2(Service, Version, Operation, ReqBody) when is_binary(ReqBody) ->
    Url = <<"http://127.0.0.1:40000/">>,
    Headers = [{<<"x-swd-target">>, list_to_binary([Service, $_, Version, $., Operation])}],
    case post(Url, Headers, ReqBody) of
        {ok, StatusCode, _RespHeaders, Body} when StatusCode =:= 200 orelse
                                                  StatusCode =:= 400 orelse
                                                  StatusCode =:= 403 ->
            case Body of
                undefined ->
                    StatusCode;
                Body ->
                    {StatusCode, jsone:decode(Body)}
            end;
        {ok, 307, RespHeaders, _Body} ->
            Location = proplists:get_value(<<"location">>, RespHeaders),
            {307, Location};
        {ok, StatusCode, _RespHeaders, Body} ->
            case Body of
                undefined ->
                    {error, StatusCode};
                Body ->
                    {StatusCode, jsone:decode(Body)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


request_with_headers(Headers, Service, Version, Operation, Json) ->
    request_with_headers(40000, Headers, Service, Version, Operation, Json).


request_with_headers(Port, Headers, Service, Version, Operation, Json) ->
    case swidden_client:request_with_headers(Port, Headers, <<"x-swd-target">>, Service, Version, Operation, Json) of
        {ok, StatusCode} ->
            StatusCode;
        {ok, StatusCode, _Body} ->
            StatusCode;
        {error, {status_code, StatusCode}} ->
            StatusCode
    end.


request_with_headers(Headers, Service, Version, Operation) ->
    case swidden_client:request_with_headers(40000, Headers, <<"x-swd-target">>, Service, Version, Operation) of
        {ok, StatusCode} ->
            StatusCode;
        {ok, StatusCode, _Body} ->
            StatusCode;
        {error, {status_code, StatusCode}} ->
            StatusCode
    end.


%% TODO(v); これ以降のリクエスト関連、リファクタすること


raw_payload_request(Service, Version, Operation, Payload) ->
    Url = <<"http://127.0.0.1:40000/">>,
    Headers = #{<<"x-swd-target">> => list_to_binary([Service, $_, Version, $., Operation])},
    {ok, StatusCode, _RespHeaders, _Body} = post(Url, Headers, Payload),
    StatusCode.


no_body_request(Service, Version, Operation) ->
    Url = <<"http://127.0.0.1:40000/">>,
    Headers = #{<<"x-swd-target">> => list_to_binary([Service, $_, Version, $., Operation])},
    {ok, StatusCode, _RespHeaders, _Body} = post(Url, Headers, <<>>),
    StatusCode.


put_method_request(Service, Version, Operation, Json) ->
    Url = <<"http://127.0.0.1:40000/">>,
    Headers = #{<<"x-swd-target">> => list_to_binary([Service, $_, Version, $., Operation])},
    Payload = jsone:encode(Json),
    {ok, StatusCode, _RespHeaders, _Body} = put(Url, Headers, Payload),
    StatusCode.


no_header_request(Json) ->
    Url = <<"http://127.0.0.1:40000/">>,
    Headers = #{},
    Payload = jsone:encode(Json),
    {ok, StatusCode, _RespHeaders, _Body} = post(Url, Headers, Payload),
    StatusCode.


bad_header_request(Json) ->
    Url = <<"http://127.0.0.1:40000/">>,
    Headers = #{<<"x-swd-target">> => <<"spam.egg.ham">>},
    Payload = jsone:encode(Json),
    {ok, StatusCode, _RespHeaders, _Body} = post(Url, Headers, Payload),
    StatusCode.


post(Url, ReqHeaders, Payload) ->
    maybe
        #{hostname := Hostname, port := Port, path := Path, transport := Transport} ?= parse_url(Url),
        {ok, ConnPid} ?= gun:open(Hostname, Port, #{transport => Transport}),
        {ok, _Protocol} ?= gun:await_up(ConnPid),
        StreamRef = gun:post(ConnPid, Path, ReqHeaders, Payload),
        {response, Fin, Status, RespHeaders} ?= gun:await(ConnPid, StreamRef),
        {ok, Body} = case Fin of
                         fin ->
                             {ok, undefined};
                         nofin ->
                             gun:await_body(ConnPid, StreamRef)
                     end,
        ok ?= gun:shutdown(ConnPid),
        %% flush しないと gun_down が通知される
        ok ?= gun:flush(ConnPid),
        {ok, Status, RespHeaders, Body}
    else
        _ ->
            error
    end.


put(Url, ReqHeaders, Payload) ->
    maybe
        #{hostname := Hostname, port := Port, path := Path, transport := Transport} ?= parse_url(Url),
        {ok, ConnPid} ?= gun:open(Hostname, Port, #{transport => Transport}),
        {ok, _Protocol} ?= gun:await_up(ConnPid),
        StreamRef = gun:put(ConnPid, Path, ReqHeaders, Payload),
        {response, _, Status, RespHeaders} ?= gun:await(ConnPid, StreamRef),
        {ok, Body} ?= gun:await_body(ConnPid, StreamRef),
        ok ?= gun:shutdown(ConnPid),
        %% flush しないと gun_down が通知される
        ok ?= gun:flush(ConnPid),
        {ok, Status, RespHeaders, Body}
    else
        _ ->
            error
    end.


-spec parse_url(binary()) -> #{
                               hostname := inet:hostname(),
                               port := inet:port_number(),
                               path := binary(),
                               transport := tcp | tls
                              }.
parse_url(Url) ->
    Uri = #{scheme := Scheme, host := Host, path := OriginPath} = uri_string:parse(Url),
    %% https://www.rfc-editor.org/rfc/rfc9112#section-3.2.1
    %% the client MUST send "/" as the path within the origin-form of request-target.
    %% とあるので、 OriginPath が空文字列の場合は "/" にする
    Path = case OriginPath of
               <<>> ->
                   <<$/>>;
               _ ->
                   OriginPath
           end,
    {Transport, Port} = case Scheme of
                            <<"http">> ->
                                Port0 = maps:get(port, Uri, 80),
                                {tcp, Port0};
                            <<"https">> ->
                                Port0 = maps:get(port, Uri, 443),
                                {tls, Port0}
                        end,
    Hostname = binary_to_list(Host),
    #{hostname => Hostname, port => Port, path => Path, transport => Transport}.
