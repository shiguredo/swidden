-module(swidden_api_handler_tests).

-include("swidden.hrl").

-include_lib("eunit/include/eunit.hrl").

%% 下記二つを使ったテスト
%% swidden/priv/swidden/dispatch.conf
%% swidden/priv/swidden/schemas

-define(APPS, [gun, ranch, cowlib, cowboy, swidden]).

%% swidden_api_handler の ?MAX_BODY_SIZE と揃える
-define(MAX_BODY_SIZE, 8000000).


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
    {ok, _Pid} = swidden:start(swidden, [{port, 0}]),
    Port = swidden:get_port(swidden),

    ?assertEqual(200,
                 request(Port,
                         <<"Spam">>,
                         <<"20141101">>,
                         <<"GetUser">>,
                         #{username => <<"yakihata">>})),
    ?assertEqual(200,
                 request(Port,
                         <<"Spam">>,
                         <<"20141101">>,
                         <<"CreateUser">>,
                         [{username, <<"yakihata">>}, {password, <<"nogyo">>}])),
    ?assertEqual(200,
                 request(Port,
                         <<"Spam">>,
                         <<"20141101">>,
                         <<"UpdateUser">>,
                         [{username, <<"yakihata">>}, {password, <<"nogyo">>}])),
    ?assertEqual(200,
                 request(Port,
                         <<"Spam">>,
                         <<"20141101">>,
                         <<"DeleteUser">>,
                         [{username, <<"yakihata">>}])),
    ?assertEqual(200, request(Port, <<"Spam">>, <<"20141101">>, <<"ListUsers">>)),

    %% JSON あり、ヘッダー追加バージョン
    ?assertEqual(200,
                 request_with_headers(Port,
                                      [{<<"x-swidden-token">>, <<"token">>}],
                                      <<"Spam">>,
                                      <<"20141101">>,
                                      <<"GetUser">>,
                                      #{username => <<"yakihata">>})),

    %% JSON なし、ヘッダー追加バージョン
    ?assertEqual(200,
                 request_with_headers(Port,
                                      [{<<"x-swidden-token">>, <<"token">>}],
                                      <<"Spam">>,
                                      <<"20141101">>,
                                      <<"ListUsers">>)),

    ?assertEqual(400,
                 request(Port,
                         <<"Spam">>,
                         <<"20150701">>,
                         <<"CreateUser">>,
                         #{username => <<"yakihata">>, password => <<"nogyo">>, group => <<"amazon">>})),

    ?assertEqual(400,
                 request(Port,
                         <<"Spam">>,
                         <<"20150701">>,
                         <<"CreateUser">>,
                         #{username => <<"error_code">>, password => <<"nogyo">>, group => <<"amazon">>})),

    ?assertEqual(200,
                 request(Port,
                         <<"SpamAdmin">>,
                         <<"20141101">>,
                         <<"GetMetrics">>,
                         #{reset => false})),

    ?assertEqual(ok, swidden:stop(swidden)),
    ok.


services_success() ->
    ?assertMatch({ok, _Pid1}, swidden:start(swidden1, [{port, 0}, {app, swidden}, {services, [<<"Spam">>]}])),
    Port1 = swidden:get_port(swidden1),
    ?assertMatch({ok, _Pid2}, swidden:start(swidden2, [{port, 0}, {app, swidden}, {services, [<<"SpamAdmin">>]}])),
    Port2 = swidden:get_port(swidden2),

    %% Spam サービスは 400、SpamAdmin サービスは 200

    ?assertEqual(200, request(Port1, <<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"yakihata">>}])),
    ?assertEqual(400, request(Port2, <<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"yakihata">>}])),

    ?assertEqual(200, request(Port2, <<"SpamAdmin">>, <<"20141101">>, <<"GetMetrics">>, [{reset, false}])),
    ?assertEqual(400, request(Port1, <<"SpamAdmin">>, <<"20141101">>, <<"GetMetrics">>, [{reset, false}])),

    ?assertEqual(ok, swidden:stop(swidden1)),
    ?assertEqual(ok, swidden:stop(swidden2)),
    ok.


failure() ->
    {ok, _Pid} = swidden:start(swidden, [{port, 0}]),
    Port = swidden:get_port(swidden),
    %% よくわからないサービス
    ?assertEqual(400, request(Port, <<"Bacon">>, <<"20141101">>, <<"GetUser">>, [{username, <<"yakihata">>}])),
    %% Body なしで送る
    ?assertEqual(400, no_body_request(Port, <<"Bacon">>, <<"20141101">>, <<"GetUser">>)),

    %% PUT メソッドで送る
    ?assertEqual(400, put_method_request(Port, <<"Bacon">>, <<"20141101">>, <<"GetUser">>, [{username, <<"yakihata">>}])),

    %% x-swd-target ヘッダーなし
    ?assertEqual(400, no_header_request(Port, [{username, <<"yakihata">>}])),
    %% x-swd-target ヘッダーの値がおかしい
    ?assertEqual(400, bad_header_request(Port, [{username, <<"yakihata">>}])),
    %% Body が空を期待しているのに Body を送った場合
    ?assertEqual(400, request(Port, <<"Spam">>, <<"20141101">>, <<"ListUsers">>, [{type, <<"all">>}])),
    %% JSON ですらない値を送った場合
    ?assertEqual(400, raw_payload_request(Port, <<"Spam">>, <<"20141101">>, <<"GetUser">>, <<"abc">>)),

    %% Body が上限ぴったりの場合は読み取りを通過する（JSON 不正なので InvalidJSON）
    ?assertMatch({400, #{<<"error_type">> := <<"InvalidJSON">>}},
                 large_body_request2(Port, <<"Spam">>, <<"20141101">>, <<"GetUser">>, ?MAX_BODY_SIZE)),
    %% Body が大きすぎる場合
    ?assertEqual({413, #{<<"error_type">> => <<"PayloadTooLarge">>}},
                 large_body_request2(Port, <<"Spam">>, <<"20141101">>, <<"GetUser">>, ?MAX_BODY_SIZE + 1)),

    ?assertEqual(ok, swidden:stop(swidden)),
    ok.


middlewares() ->
    {ok, _Pid} = swidden:start(swidden,
                               [{middlewares, [cowboy_router,
                                               sample_middleware,
                                               cowboy_handler]},
                                {port, 0}]),
    Port = swidden:get_port(swidden),

    ?assertEqual(200, request(Port, <<"SpamAdmin">>, <<"20141101">>, <<"GetMetrics">>, [{reset, false}])),

    ?assertEqual(200, request(Port, <<"Spam">>, <<"20141101">>, <<"GetAuthenticatedUser">>)),
    ?assertEqual(200, request(Port, <<"Spam">>, <<"20141101">>, <<"UpdateAuthenticatedUser">>, [{username, <<"NewName">>}])),

    ?assertEqual(400, request(Port, <<"Spam">>, <<"20141101">>, <<"UpdateAuthenticatedUser">>, [{bad_key, <<"NewName">>}])),

    ?assertEqual(ok, swidden:stop(swidden)),
    ok.


redirect() ->
    {ok, _Pid} = swidden:start(swidden, [{port, 0}]),
    Port = swidden:get_port(swidden),

    ?assertEqual(307, request(Port, <<"Spam">>, <<"20141101">>, <<"Redirect">>)),

    ?assertEqual(ok, swidden:stop(swidden)),
    ok.


crash() ->
    {ok, _Pid} = swidden:start(swidden, [{port, 0}]),
    Port = swidden:get_port(swidden),

    %% Body なし: crash/0 が存在しないため MissingTargetFunction
    ?assertEqual({400,
                  #{
                    <<"error_type">> => <<"MissingTargetFunction">>,
                    <<"error_reason">> => #{
                                            <<"service">> => <<"Spam">>,
                                            <<"version">> => <<"20141101">>,
                                            <<"operation">> => <<"Crash">>
                                           }
                   }},
                 request2(Port, <<"Spam">>, <<"20141101">>, <<"Crash">>)),

    %% Body あり: crash/1 が例外を起こし構造化エラーで 500 を返す
    ?assertEqual({500, #{<<"error_type">> => <<"HandlerException">>}},
                 request2(Port, <<"Spam">>, <<"20141101">>, <<"Crash">>, <<"{}">>)),

    ?assertEqual(ok, swidden:stop(swidden)),
    ok.


interceptor() ->
    {ok, _Pid} = swidden:start(swidden, [{port, 0}, {interceptor, sample_interceptor}]),
    Port = swidden:get_port(swidden),

    %% 素通しする
    ?assertEqual({200,
                  #{
                    <<"password">> => <<"password">>,
                    <<"good_or_bad">> => <<"good">>
                   }},
                 request2(Port, <<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"Hermione">>}])),
    %% ok
    ?assertEqual(200,
                 request2(Port, <<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"Ron">>}])),

    %% ok, JSON 付き
    ?assertEqual({200,
                  #{
                    <<"everyboby">> => <<"know him">>,
                    <<"good_or_bad">> => <<"good">>
                   }},
                 request2(Port, <<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"Harry">>}])),

    %% リダイレクト
    ?assertEqual({307, <<"http://example.com/albus?foo=bar">>},
                 request2(Port, <<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"Dumbledore">>}])),

    %% エラー
    ?assertEqual({400, #{<<"error_type">> => <<"He-Who-Must-Not-Be-Named, You-Know-Who">>}},
                 request2(Port, <<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"Voldemort">>}])),
    %% エラー, JSON 付き
    ?assertEqual({400,
                  #{
                    <<"error_type">> => <<"insufficient privilege">>,
                    <<"error_reason">> => #{
                                            <<"caution">> => <<"Slytherin only">>,
                                            <<"good_or_bad">> => <<"bad">>
                                           }
                   }},
                 request2(Port, <<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"Snape">>}])),

    %% 引数なしパターン
    ?assertEqual({200,
                  [#{
                     <<"password">> => <<"password">>,
                     <<"username">> => <<"username">>
                    }]},
                 request2(Port, <<"Spam">>, <<"20141101">>, <<"ListUsers">>)),
    ?assertEqual({400, #{<<"error_type">> => <<"not allowed">>}},
                 request2(Port, <<"Spam">>, <<"20141101">>, <<"Redirect">>)),

    %% preprocess/3 が例外を起こした場合
    ?assertEqual({500, #{<<"error_type">> => <<"HandlerException">>}},
                 request2(Port, <<"Spam">>, <<"20141101">>, <<"GetUser">>, [{username, <<"InterceptCrash">>}])),
    %% preprocess/2 が例外を起こした場合
    ?assertEqual({500, #{<<"error_type">> => <<"HandlerException">>}},
                 request2(Port, <<"Spam">>, <<"20141101">>, <<"GetAuthenticatedUser">>)),
    %% postprocess/3 が例外を起こした場合
    ?assertEqual({500, #{<<"error_type">> => <<"HandlerException">>}},
                 request2(Port,
                           <<"Spam">>,
                           <<"20141101">>,
                           <<"GetUser">>,
                           [{username, <<"PostprocessCrash">>}])),
    ?assertEqual(ok, swidden:stop(swidden)),
    ok.


request(Port, Service, Version, Operation, JSON) ->
    case swidden_client:request(Port, <<"x-swd-target">>, Service, Version, Operation, JSON) of
        {ok, StatusCode} ->
            StatusCode;
        {ok, StatusCode, _Body} ->
            StatusCode;
        {error, {status_code, StatusCode}} ->
            StatusCode
    end.


request(Port, Service, Version, Operation) ->
    case swidden_client:request(Port, <<"x-swd-target">>, Service, Version, Operation) of
        {ok, StatusCode} ->
            StatusCode;
        {ok, StatusCode, _Body} ->
            StatusCode;
        {error, {status_code, StatusCode}} ->
            StatusCode
    end.


request2(Port, Service, Version, Operation) ->
    request2(Port, Service, Version, Operation, <<>>).


request2(Port, Service, Version, Operation, Json) when is_list(Json) ->
    Body = jsone:encode(Json),
    request2(Port, Service, Version, Operation, Body);
request2(Port, Service, Version, Operation, ReqBody) when is_binary(ReqBody) ->
    Url = url(Port),
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


request_with_headers(Port, Headers, Service, Version, Operation, Json) ->
    case swidden_client:request_with_headers(Port, Headers, <<"x-swd-target">>, Service, Version, Operation, Json) of
        {ok, StatusCode} ->
            StatusCode;
        {ok, StatusCode, _Body} ->
            StatusCode;
        {error, {status_code, StatusCode}} ->
            StatusCode
    end.


request_with_headers(Port, Headers, Service, Version, Operation) ->
    case swidden_client:request_with_headers(Port, Headers, <<"x-swd-target">>, Service, Version, Operation) of
        {ok, StatusCode} ->
            StatusCode;
        {ok, StatusCode, _Body} ->
            StatusCode;
        {error, {status_code, StatusCode}} ->
            StatusCode
    end.


%% TODO(v); これ以降のリクエスト関連、リファクタすること


url(Port) ->
    iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port), "/"]).


raw_payload_request(Port, Service, Version, Operation, Payload) ->
    Url = url(Port),
    Headers = #{<<"x-swd-target">> => list_to_binary([Service, $_, Version, $., Operation])},
    {ok, StatusCode, _RespHeaders, _Body} = post(Url, Headers, Payload),
    StatusCode.


large_body_request2(Port, Service, Version, Operation, Size) ->
    Url = url(Port),
    Headers = #{<<"x-swd-target">> => list_to_binary([Service, $_, Version, $., Operation])},
    Payload = binary:copy(<<$a>>, Size),
    {ok, StatusCode, _RespHeaders, Body} = post(Url, Headers, Payload),
    {StatusCode, jsone:decode(Body)}.


no_body_request(Port, Service, Version, Operation) ->
    Url = url(Port),
    Headers = #{<<"x-swd-target">> => list_to_binary([Service, $_, Version, $., Operation])},
    {ok, StatusCode, _RespHeaders, _Body} = post(Url, Headers, <<>>),
    StatusCode.


put_method_request(Port, Service, Version, Operation, Json) ->
    Url = url(Port),
    Headers = #{<<"x-swd-target">> => list_to_binary([Service, $_, Version, $., Operation])},
    Payload = jsone:encode(Json),
    {ok, StatusCode, _RespHeaders, _Body} = put(Url, Headers, Payload),
    StatusCode.


no_header_request(Port, Json) ->
    Url = url(Port),
    Headers = #{},
    Payload = jsone:encode(Json),
    {ok, StatusCode, _RespHeaders, _Body} = post(Url, Headers, Payload),
    StatusCode.


bad_header_request(Port, Json) ->
    Url = url(Port),
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
