-module(swidden_api_handler).

-behaviour(cowboy_handler).

-export([init/2,
         handle/3,
         terminate/3]).

-include("swidden.hrl").

%% > re:run(<<"XYZ_20001210.CreateUser">>, <<"^([a-zA-Z]+)\_(\\d{4}\\d{2}\\d{2})\.([a-zA-Z]+)$">>, [{capture, all_but_first, binary}]).
%% {match,[<<"XYZ">>,<<"20001210">>,<<"CreateUser">>]}
-define(REGEXP, <<"^([a-zA-Z]+)\_(\\d{4}\\d{2}\\d{2})\.([a-zA-Z]+)$">>).

-define(DEFAULT_HEADERS, [{<<"content-type">>, <<"application/json">>}]).


init(Req, Opts) ->
    HeaderName = proplists:get_value(header_name, Opts),
    Method = cowboy_req:method(Req),
    Req2 = handle(Method, HeaderName, Req),
    {ok, Req2, Opts}.


handle(<<"POST">>, HeaderName, Req) ->
    case cowboy_req:header(HeaderName, Req) of
        undefined ->
            %% ヘッダーがみつからない
            %% XXX(nakai): 400 としたが 404 がいいか？
            RawJSON = jsx:encode([{type, <<"MissingHeaderName">>}]),
            cowboy_req:reply(400, ?DEFAULT_HEADERS, RawJSON, Req);
        Value ->
            %% Service_Version.Operation として分解する
            case re:run(Value, ?REGEXP, [{capture, all_but_first, binary}]) of
                {match, [Service, Version, Operation]} ->
                    %% TODO(nakai): リファクタリング
                    case cowboy_req:has_body(Req) of
                        true ->
                            {ok, Body, Req2} = cowboy_req:body(Req),
                            {StatusCode, RawJSON} = validate_json(Service, Version, Operation, Body),
                            cowboy_req:reply(StatusCode, ?DEFAULT_HEADERS, RawJSON, Req2);
                        false ->
                            {StatusCode, RawJSON} = dispatch(Service, Version, Operation),
                            cowboy_req:reply(StatusCode, ?DEFAULT_HEADERS, RawJSON, Req)
                    end;
                nomatch ->
                    %% TODO(nakai): ヘッダーが期待したメッセージではない
                    cowboy_req:reply(400, ?DEFAULT_HEADERS, jsx:encode([{error_type, <<"InvalidTarget">>}]), Req)
            end
    end;
handle(_Method, _HeaderName, Req) ->
    %% POST 以外受け付けていないのでエラーメッセージ
    RawJSON = jsx:encode([{type, <<"UnexpectedMethod">>}]),
    cowboy_req:reply(400, ?DEFAULT_HEADERS, RawJSON, Req).


terminate(normal, _Req, _State) ->
    ok.


dispatch(Service, Version, Operation) ->
    case swidden_dispatch:lookup(Service, Version, Operation) of
        not_found ->
            {400, jsx:encode([{error_type, <<"MissingTarget">>}])};
        {Module, Function} ->   
            case Module:Function() of
                ok ->
                    {200, <<>>};
                {ok, RespJSON} ->
                    {200, jsx:encode(RespJSON)};
                {error, Type} ->
                    {400, jsx:encode([{error_type, Type}])}
            end
    end.


validate_json(Service, Version, Operation, RawJSON) ->
    case swidden_json_schema:validate_json(Service, Version, Operation, RawJSON) of
        {ok, Module, Function, JSON} ->
            %% ここは swidden:success/0,1 と swidden:failure/1 の戻り値
            case Module:Function(JSON) of
                ok ->
                    {200, <<>>};
                {ok, RespJSON} ->
                    {200, jsx:encode(RespJSON)};
                {error, Type} ->
                    {400, jsx:encode([{error_type, Type}])}
            end;
        {error, Reason} ->
            ?debugVal2(Reason),
            %% TODO(nakai): エラー処理
            {400, jsx:encode([{error_type, <<"MissingTarget">>}])}
    end.
