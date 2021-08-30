-module(swidden_api_handler).

%% -behaviour(cowboy_handler).

-export([init/2,
         terminate/3]).

-include("swidden.hrl").

%% > re:run(<<"XYZ_20001210.CreateUser">>, <<"^([a-zA-Z]+)\_(\\d{4}\\d{2}\\d{2})\.([a-zA-Z]+)$">>, [{capture, all_but_first, binary}]).
%% {match,[<<"XYZ">>,<<"20001210">>,<<"CreateUser">>]}
-define(REGEXP, <<"^([a-zA-Z]+)\_(\\d{4}\\d{2}\\d{2})\.([a-zA-Z]+)$">>).

-define(DEFAULT_HEADERS, #{<<"content-type">> => <<"application/json">>}).

-define(REDIRECT_STATUS_CODE, 307).


init(Req, Opts) ->
    HeaderName = proplists:get_value(header_name, Opts),
    Services = proplists:get_value(services, Opts),
    case cowboy_req:method(Req) of
        <<"POST">> ->
            case cowboy_req:header(HeaderName, Req) of
                undefined ->
                    %% ヘッダーがみつからない
                    %% XXX(nakai): 400 としたが 404 がいいか？
                    RawJSON = jsone:encode(#{type => <<"MissingHeaderName">>}, [skip_undefined]),
                    Req2 = cowboy_req:reply(400, ?DEFAULT_HEADERS, RawJSON, Req),
                    {ok, Req2, Opts};
                HeaderValue ->
                    %% Service_Version.Operation として分解する
                    case re:run(HeaderValue, ?REGEXP, [{capture, all_but_first, binary}]) of
                        {match, [Service, Version, Operation]} ->
                            case lists:member(Service, Services) of
                                true ->
                                    Req2 = handle(Service, Version, Operation, Req),
                                    {ok, Req2, Opts};
                                false when Services == [] ->
                                    Req2 = handle(Service, Version, Operation, Req),
                                    {ok, Req2, Opts};
                                false ->
                                    Req2 = cowboy_req:reply(400, ?DEFAULT_HEADERS,
                                                            jsone:encode(#{error_type => <<"InvalidTarget">>},
                                                                         [skip_undefined]), Req),
                                    {ok, Req2, Opts}
                            end;
                        _ ->
                            %% サービスに対応してなかったよ
                            Req2 = cowboy_req:reply(400, ?DEFAULT_HEADERS,
                                                    jsone:encode(#{error_type => <<"MissingService">>},
                                                                 [skip_undefined]), Req),
                            {ok, Req2, Opts}
                    end
            end;
        _Other ->
            %% POST 以外受け付けていないのでエラーメッセージ
            RawJSON = jsone:encode(#{type => <<"UnexpectedMethod">>}, [skip_undefined]),
            Req2 = cowboy_req:reply(400, ?DEFAULT_HEADERS, RawJSON, Req),
            {ok, Req2, Opts}
    end.

read_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, Req2} ->
             {ok, <<Acc/binary, Data/binary>>, Req2};
        {more, Data, Req2} ->
            read_body(Req2, <<Acc/binary, Data/binary>>)
    end.


handle(Service, Version, Operation, Req) ->
    %% TODO(nakai): リファクタリング
    case cowboy_req:has_body(Req) of
        true ->
            case read_body(Req, <<>>) of
                {ok, <<>>, Req2} ->
                    case dispatch(Service, Version, Operation) of
                        200 ->
                            cowboy_req:reply(200, ?DEFAULT_HEADERS, [], Req2);
                        {?REDIRECT_STATUS_CODE, Location} ->
                            cowboy_req:reply(?REDIRECT_STATUS_CODE, #{<<"location">> => Location}, [], Req);
                        {StatusCode, JSON} ->
                            RawJSON = jsone:encode(JSON, [skip_undefined]),
                            cowboy_req:reply(StatusCode, ?DEFAULT_HEADERS, RawJSON, Req2)
                    end;
                {ok, Body, Req2} ->
                    case validate_json(Service, Version, Operation, Body) of
                        200 ->
                            cowboy_req:reply(200, ?DEFAULT_HEADERS, [], Req2);
                        {?REDIRECT_STATUS_CODE, Location} ->
                            cowboy_req:reply(?REDIRECT_STATUS_CODE, #{<<"location">> => Location}, [], Req);
                        {StatusCode, JSON} ->
                            RawJSON = jsone:encode(JSON, [skip_undefined]),
                            cowboy_req:reply(StatusCode, ?DEFAULT_HEADERS, RawJSON, Req2)
                    end
            end;
        false ->
            case dispatch(Service, Version, Operation) of
                200 ->
                    cowboy_req:reply(200, ?DEFAULT_HEADERS, [], Req);
                {?REDIRECT_STATUS_CODE, Location} ->
                    cowboy_req:reply(?REDIRECT_STATUS_CODE, #{<<"location">> => Location}, [], Req);
                {StatusCode, JSON} ->
                    RawJSON = jsone:encode(JSON, [skip_undefined]),
                    cowboy_req:reply(StatusCode, ?DEFAULT_HEADERS, RawJSON, Req)
            end
    end.


terminate(normal, _Req, _State) ->
    ok;
terminate(Reason, _Req, _State) ->
    ?debugVal3(Reason),
    ok.


dispatch(Service, Version, Operation) ->
    case swidden_dispatch:lookup(Service, Version, Operation) of
        not_found ->
            {400, #{error_type => <<"MissingTarget">>}};
        {Module, Function} ->
            case code:which(Module) of
                non_existing ->
                    {400, #{error_type => <<"MissingTargetModule">>}};
                _ ->
                    case lists:member({Function, 0}, Module:module_info(exports)) of
                        true ->
                            apply0(Module, Function, []);
                        false ->
                            {400, #{error_type => <<"MissingTargetFunction">>,
                                    error_reason => #{service => Service,
                                                      version => Version,
                                                      operation => Operation}}}
                    end
            end
    end.


validate_json(Service, Version, Operation, RawJSON) ->
    case swidden_json_schema:validate_json(Service, Version, Operation, RawJSON) of
        {ok, Module, Function, JSON} ->
            %% ここは swidden:success/0,1 と swidden:failure/1 の戻り値
            case code:which(Module) of
                non_existing ->
                    {400, #{error_type => <<"MissingTargetModule">>}};
                _ ->
                    case lists:member({Function, 1}, Module:module_info(exports)) of
                        true ->
                            apply0(Module, Function, [JSON]);
                        false ->
                            {400, #{error_type => <<"MissingTargetFunction">>,
                                    error_reason => #{service => Service,
                                                      version => Version,
                                                      operation => Operation}}}
                    end
            end;
        {error, {data_error, _Reason}} ->
            ?debugVal(_Reason),
            {400, #{error_type => <<"MalformedJSON">>}};
        {error, {database_error, _Key, schema_not_found}} ->
            %% TODO(nakai): この部分は外だしする
            {400, #{error_type => <<"SchemaNotFound">>,
                    error_reason => #{service => Service,
                                      version => Version,
                                      operation => Operation}}};
        {error, Reasons} ->
            ErrorReasons = swidden_json_schema:to_json(Reasons),
            {400, #{error_type => <<"InvalidJSON">>,
                    error_reason => ErrorReasons}}
    end.


apply0(Module, Function, Args) ->
    case apply(Module, Function, Args) of
        {ok, {redirect, Location}} ->
            {?REDIRECT_STATUS_CODE, Location};
        ok ->
            200;
        {ok, RespJSON} ->
            {200, RespJSON};
        {error, Type} ->
            {400, #{error_type => Type}};
        {error, Type, Reason} ->
            {400, #{error_type => Type, error_reason => Reason}}
    end.
