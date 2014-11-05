-module(swidden_api_handler_tests).

-include("swidden.hrl").

-include_lib("eunit/include/eunit.hrl").

%% 下記二つを使ったテスト
    %% swidden/priv/swidden/dispatch.conf
    %% swidden/priv/swidden/schemas

-define(APPS, [ranch, cowlib, cowboy, swidden]).


start_apps() ->
    [ application:start(App) || App <- ?APPS ].


stop_apps() ->
    [ application:stop(App) || App <- lists:reverse(?APPS) ].


all_test_() ->
    {foreach,
     fun() ->
             start_apps(),
             hackney:start(),
             ok
     end,
     fun(_) ->
             stop_apps(),
             hackney:stop(),
             ok
     end,
     [
      fun success/0,
      fun failure/0
     ]
    }.


success() ->
    ?assertMatch({ok, _Pid}, swidden:start(swidden, [{port, 40000}])),

    ?assertEqual(200, request(<<"Spam">>, <<"20141101">>, <<"GetUser">>,
                              [{username, <<"yakihata">>}])),
    ?assertEqual(200, request(<<"Spam">>, <<"20141101">>, <<"CreateUser">>,
                              [{username, <<"yakihata">>}, {password, <<"nogyo">>}])),
    ?assertEqual(200, request(<<"Spam">>, <<"20141101">>, <<"UpdateUser">>,
                              [{username, <<"yakihata">>}, {password, <<"nogyo">>}])),
    ?assertEqual(200, request(<<"Spam">>, <<"20141101">>, <<"DeleteUser">>,
                              [{username, <<"yakihata">>}])),

    ?assertEqual(400, request(<<"Spam">>, <<"20150701">>, <<"CreateUser">>,
                              [{username, <<"yakihata">>}, {password, <<"nogyo">>}, {group, <<"amazon">>}])),

    ?assertEqual(200, request(<<"SpamAdmin">>, <<"20141101">>, <<"GetMetrics">>,
                              [{reset, false}])),

    ?assertEqual(ok, swidden:stop()),
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

    ?assertEqual(ok, swidden:stop()),
    ok.


request(Service, Verision, Operation, JSON) ->
    URL = <<"http://127.0.0.1:40000/">>,
    Headers = [{<<"x-swd-target">>, list_to_binary([Service, $_, Verision, $., Operation])}],
    Payload = jsonx:encode(JSON),
    Options = [],
    {ok, StatusCode, _RespHeaders, ClientRef} = hackney:post(URL, Headers, Payload, Options),
    hackney:close(ClientRef),
    StatusCode.


%% TODO(nakai): これ以降のリクエスト関連、リファクタすること


no_body_request(Service, Verision, Operation) ->
    URL = <<"http://127.0.0.1:40000/">>,
    Headers = [{<<"x-swd-target">>, list_to_binary([Service, $_, Verision, $., Operation])}],
    Options = [],
    {ok, StatusCode, _RespHeaders, ClientRef} = hackney:post(URL, Headers, [], Options),
    hackney:close(ClientRef),
    StatusCode.


put_method_request(Service, Verision, Operation, JSON) ->
    URL = <<"http://127.0.0.1:40000/">>,
    Headers = [{<<"x-swd-target">>, list_to_binary([Service, $_, Verision, $., Operation])}],
    Payload = jsonx:encode(JSON),
    Options = [],
    {ok, StatusCode, _RespHeaders, ClientRef} = hackney:put(URL, Headers, Payload, Options),
    hackney:close(ClientRef),
    StatusCode.


no_header_request(JSON) ->
    URL = <<"http://127.0.0.1:40000/">>,
    Headers = [],
    Payload = jsonx:encode(JSON),
    Options = [],
    {ok, StatusCode, _RespHeaders, ClientRef} = hackney:post(URL, Headers, Payload, Options),
    hackney:close(ClientRef),
    StatusCode.


bad_header_request(JSON) ->
    URL = <<"http://127.0.0.1:40000/">>,
    Headers = [{<<"x-swd-target">>, <<"spam.egg.ham">>}],
    Payload = jsonx:encode(JSON),
    Options = [],
    {ok, StatusCode, _RespHeaders, ClientRef} = hackney:post(URL, Headers, Payload, Options),
    hackney:close(ClientRef),
    StatusCode.
