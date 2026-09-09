-module(swidden_api_handler).

-export([init/2,
         terminate/3]).

-include("swidden.hrl").

%% > re:run(<<"XYZ_20001210.CreateUser">>, <<"^([a-zA-Z]+)\_(\\d{4}\\d{2}\\d{2})\.([a-zA-Z]+)$">>, [{capture, all_but_first, binary}]).
%% {match,[<<"XYZ">>,<<"20001210">>,<<"CreateUser">>]}
-define(REGEXP, <<"^([a-zA-Z]+)\_(\\d{4}\\d{2}\\d{2})\.([a-zA-Z]+)$">>).

-define(DEFAULT_HEADERS, #{<<"content-type">> => <<"application/json">>}).

-define(REDIRECT_STATUS_CODE, 307).

%% Cowboy read_body/2 の length オプションのデフォルト（1 回あたりの chunk サイズ）
-define(READ_BODY_CHUNK_SIZE, 8000000).
%% リクエスト Body 全体の上限（Cowboy は chunk 単位の length しか持たないため自前で enforce する）
-define(MAX_BODY_SIZE, 8000000).

-type state() :: cowboy_handler:opts().


-spec init(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
init(Req, Opts) ->
    HeaderName = proplists:get_value(header_name, Opts),
    Services = proplists:get_value(services, Opts),
    Interceptor = proplists:get_value(interceptor, Opts),
    case cowboy_req:method(Req) of
        <<"POST">> ->
            init_post(HeaderName, Services, Req, Opts, Interceptor);
        _Other ->
            %% POST 以外受け付けていないのでエラーメッセージ
            RawJSON = jsone:encode(#{type => <<"UnexpectedMethod">>}, [skip_undefined]),
            Req2 = cowboy_req:reply(400, ?DEFAULT_HEADERS, RawJSON, Req),
            {ok, Req2, Opts}
    end.


init_post(HeaderName, Services, Req, Opts, Interceptor) ->
    case cowboy_req:header(HeaderName, Req) of
        undefined ->
            %% ヘッダーがみつからない
            RawJSON = jsone:encode(#{type => <<"MissingHeaderName">>}, [skip_undefined]),
            Req2 = cowboy_req:reply(400, ?DEFAULT_HEADERS, RawJSON, Req),
            {ok, Req2, Opts};
        HeaderValue ->
            init_service(HeaderValue, Services, Req, Opts, Interceptor)
    end.


init_service(HeaderValue, Services, Req, Opts, Interceptor) ->
    %% Service_Version.Operation として分解する
    case re:run(HeaderValue, ?REGEXP, [{capture, all_but_first, binary}]) of
        {match, [Service, Version, Operation]} ->
            case lists:member(Service, Services) of
                true ->
                    Req2 = handle(Service, Version, Operation, Req, Interceptor),
                    {ok, Req2, Opts};
                false when Services == [] ->
                    Req2 = handle(Service, Version, Operation, Req, Interceptor),
                    {ok, Req2, Opts};
                false ->
                    reply_init_error(#{error_type => <<"InvalidTarget">>}, Req, Opts)
            end;
        _ ->
            %% サービスに対応してなかったよ
            reply_init_error(#{error_type => <<"MissingService">>}, Req, Opts)
    end.


reply_init_error(JSON, Req, Opts) ->
    RawJSON = jsone:encode(JSON, [skip_undefined]),
    Req2 = cowboy_req:reply(400, ?DEFAULT_HEADERS, RawJSON, Req),
    {ok, Req2, Opts}.


read_body(Req) ->
    read_body(Req, <<>>, 0).


read_body(Req, Acc, AccSize) ->
    case AccSize >= ?MAX_BODY_SIZE of
        true ->
            {error, payload_too_large};
        false ->
            %% length は chunk サイズ（best effort）。各 read_body 呼び出しごとに渡す。
            ChunkSize = erlang:min(?READ_BODY_CHUNK_SIZE, ?MAX_BODY_SIZE - AccSize),
            try cowboy_req:read_body(Req, #{length => ChunkSize}) of
                {ok, Data, Req2} ->
                    %% fin 時は chunk サイズを超える Body が届く（cowboy_stream_h の IsFin =:= fin 分岐）
                    NewSize = AccSize + byte_size(Data),
                    case NewSize > ?MAX_BODY_SIZE of
                        true ->
                            {error, payload_too_large};
                        false ->
                            {ok, iolist_to_binary([Acc, Data]), Req2}
                    end;
                {more, Data, Req2} ->
                    DataSize = byte_size(Data),
                    NewSize = AccSize + DataSize,
                    %% {more, ...} は nofin: これ以上 Body が続く。
                    %% read_urlencoded_body/2 と同様、chunk が満杯なら上限超過とみなす。
                    case (NewSize > ?MAX_BODY_SIZE) orelse (DataSize >= ChunkSize) of
                        true ->
                            {error, payload_too_large};
                        false ->
                            read_body(Req2, [Acc, Data], NewSize)
                    end
            catch
                exit:timeout ->
                    {error, timeout};
                exit:{{request_error, payload_too_large, _}, _} ->
                    {error, payload_too_large}
            end
    end.


handle(Service, Version, Operation, Req, Interceptor) ->
    case cowboy_req:has_body(Req) of
        true ->
            handle_with_body(Service, Version, Operation, Req, Interceptor);
        false ->
            reply_dispatch(dispatch(Service, Version, Operation, Interceptor), Req)
    end.


handle_with_body(Service, Version, Operation, Req, Interceptor) ->
    case read_body(Req) of
        {error, payload_too_large} ->
            reply_json(413, #{error_type => <<"PayloadTooLarge">>}, Req);
        {error, timeout} ->
            reply_json(408, #{error_type => <<"RequestTimeout">>}, Req);
        {ok, <<>>, Req2} ->
            reply_dispatch(dispatch(Service, Version, Operation, Interceptor), Req2);
        {ok, Body, Req2} ->
            reply_dispatch(validate_json(Service, Version, Operation, Body, Interceptor), Req2)
    end.


reply_dispatch(200, Req) ->
    cowboy_req:reply(200, ?DEFAULT_HEADERS, [], Req);
reply_dispatch({?REDIRECT_STATUS_CODE, Location}, Req) ->
    cowboy_req:reply(?REDIRECT_STATUS_CODE, #{<<"location">> => Location}, [], Req);
reply_dispatch({StatusCode, JSON}, Req) ->
    RawJSON = jsone:encode(JSON, [skip_undefined]),
    cowboy_req:reply(StatusCode, ?DEFAULT_HEADERS, RawJSON, Req).


-spec terminate(term(), cowboy_req:req(), state()) -> ok.
terminate(normal, _Req, _State) ->
    ok;
terminate(_Reason, _Req, _State) ->
    ok.


dispatch(Service, Version, Operation, Interceptor) ->
    case swidden_dispatch:lookup(Service, Version, Operation) of
        not_found ->
            {400, #{error_type => <<"MissingTarget">>}};
        {Module, Function} ->
            case target_function(Module, Function, 0) of
                ok ->
                    preprocess0(Module, Function, Interceptor);
                {error, missing_module} ->
                    {400, #{error_type => <<"MissingTargetModule">>}};
                {error, missing_function} ->
                    missing_target_function_error(Service, Version, Operation)
            end
    end.


%% モジュールと関数が存在するかを確認する
target_function(Module, Function, Arity) ->
    case code:which(Module) of
        non_existing ->
            {error, missing_module};
        _ ->
            case lists:member({Function, Arity}, Module:module_info(exports)) of
                true ->
                    ok;
                false ->
                    {error, missing_function}
            end
    end.


missing_target_function_error(Service, Version, Operation) ->
    {400,
     #{
       error_type => <<"MissingTargetFunction">>,
       error_reason => #{
                         service => Service,
                         version => Version,
                         operation => Operation
                        }
      }}.


validate_json(Service, Version, Operation, RawJSON, Interceptor) ->
    case swidden_json_schema:validate_json(Service, Version, Operation, RawJSON) of
        {ok, Module, Function, JSON} ->
            case target_function(Module, Function, 1) of
                ok ->
                    preprocess1(Module, Function, JSON, Interceptor);
                {error, missing_module} ->
                    {400, #{error_type => <<"MissingTargetModule">>}};
                {error, missing_function} ->
                    missing_target_function_error(Service, Version, Operation)
            end;
        {error, {data_error, _Reason}} ->
            {400, #{error_type => <<"MalformedJSON">>}};
        {error, {database_error, _Key, schema_not_found}} ->
            %% TODO(v); この部分は外だしする
            {400,
             #{
               error_type => <<"SchemaNotFound">>,
               error_reason => #{
                                 service => Service,
                                 version => Version,
                                 operation => Operation
                                }
              }};
        {error, Reasons} ->
            ErrorReasons = swidden_json_schema:to_json(Reasons),
            {400,
             #{
               error_type => <<"InvalidJSON">>,
               error_reason => ErrorReasons
              }}
    end.


preprocess0(Module, Function, undefined) ->
    apply_mfa(Module, Function, []);
preprocess0(Module, Function, Interceptor) ->
    run_interceptor(fun() ->
                            case Interceptor:preprocess(Module, Function) of
                                continue ->
                                    apply_mfa(Module, Function, [], Interceptor);
                                {stop, Result} ->
                                    safe_response(Result)
                            end
                    end).


preprocess1(Module, Function, JSON, undefined) ->
    apply_mfa(Module, Function, [JSON]);
preprocess1(Module, Function, JSON0, Interceptor) ->
    run_interceptor(fun() ->
                            case Interceptor:preprocess(Module, Function, JSON0) of
                                {continue, JSON1} ->
                                    apply_mfa(Module, Function, [JSON1], Interceptor);
                                {stop, Result} ->
                                    postprocess(Module, Function, Result, Interceptor)
                            end
                    end).


apply_mfa(Module, Function, Args) ->
    try
        Result = apply(Module, Function, Args),
        response(Result)
    catch
        _Class:_Reason:_Stack ->
            {500, #{error_type => <<"HandlerException">>}}
    end.


apply_mfa(Module, Function, Args, Interceptor) ->
    try
        Result = apply(Module, Function, Args),
        postprocess(Module, Function, Result, Interceptor)
    catch
        _Class:_Reason:_Stack ->
            {500, #{error_type => <<"HandlerException">>}}
    end.


postprocess(Module, Function, Result0, Interceptor) ->
    run_interceptor(fun() ->
                            Result1 = Interceptor:postprocess(Module, Function, Result0),
                            safe_response(Result1)
                    end).


run_interceptor(Fun) ->
    try Fun() of
        Result ->
            Result
    catch
        _Class:_Reason:_Stack ->
            handler_exception()
    end.


safe_response(Result) ->
    try
        response(Result)
    catch
        _Class:_Reason:_Stack ->
            handler_exception()
    end.


handler_exception() ->
    {500, #{error_type => <<"HandlerException">>}}.


reply_json(StatusCode, JSON, Req) ->
    RawJSON = jsone:encode(JSON, [skip_undefined]),
    cowboy_req:reply(StatusCode, ?DEFAULT_HEADERS, RawJSON, Req).


response({ok, {redirect, Location}}) ->
    {?REDIRECT_STATUS_CODE, Location};
response(ok) ->
    200;
response({ok, RespJSON}) ->
    {200, RespJSON};
response({error, Type}) ->
    {400, #{error_type => Type}};
response({error, Type, Reason}) ->
    {400, #{error_type => Type, error_reason => Reason}}.
