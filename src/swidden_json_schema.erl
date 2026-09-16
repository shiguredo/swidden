-module(swidden_json_schema).

-export([start/1]).
-export([validate_json/4]).
-export([to_json/1]).

-include("swidden.hrl").
-include("swidden_dispatch.hrl").

%% TODO(v); req/res 両方のスキーマが必要なのでは ...
%% TODO(v); レスポンスのバリデーションチェックはどうする？


-spec start(atom()) -> ok | no_return().
start(Name) ->
    case load_schemas(Name) of
        ok ->
            ok;
        {error, Reason} ->
            %% FIXME(nakai): 手抜き
            error(Reason)
    end.


-spec validate_json(binary(), binary(), binary(), binary()) ->
          {ok, module(), atom(), jsone:json_term()} | {error, term()}.
validate_json(Service, Version, Operation, RawJSON) ->
    Key = binary_to_list(list_to_binary([Service, $_, Version, $., Operation])),
    maybe
        {ok, JSON} ?= validate(Key, RawJSON),
        {Module, Function} ?= swidden_dispatch:lookup(Service, Version, Operation),
        {ok, Module, Function, JSON}
    else
        {error, Reason} ->
            {error, Reason};
        not_found ->
            {error, missing_routing}
    end.


%% INTERNAL


-spec load_schemas(atom()) -> ok | {error, term()}.
load_schemas(Name) ->
    case code:priv_dir(Name) of
        {error, Reason} ->
            {error, Reason};
        PrivPath ->
            %% <application>/priv/swidden/schemas/<service>/<version>/<schema>.json
            Path = filename:join([PrivPath, "swidden"]),
            Dispatchs = swidden_dispatch:get_dispatches(),
            load_schemas(Path, Dispatchs)
    end.


load_schemas(_Path, []) ->
    ok;
load_schemas(Path,
             [#swidden_dispatch{
                id = {Service, Version, Operation},
                schema = Schema
               } | Rest]) ->
    FileName = lists:flatten(lists:join(".", [atom_to_list(Schema), "json"])),
    PascalCaseService = swidden_misc:pascal2snake(Service),
    %% <application>/priv/swidden/schemas/<service>/<version>/<schema>.json
    FilePath = filename:join([Path, "schemas", PascalCaseService, Version, FileName]),
    case file:read_file(FilePath) of
        {ok, Binary} ->
            Key = binary_to_list(list_to_binary([Service, $_, Version, $., Operation])),
            %% スキーマのパースエラーもスキーマの内容のエラーも {error, Reason} で返る
            case add_schema(Key, Binary) of
                ok ->
                    load_schemas(Path, Rest);
                {error, Reason} ->
                    {error, {FilePath, Reason}}
            end;
        {error, Reason} ->
            {error, {FilePath, Reason}}
    end.


-spec add_schema(string(), binary()) -> ok | {error, term()}.
add_schema(_Key, <<>>) ->
    ok;
add_schema(Key, RawJSON) ->
    %% スキーマは jsone:decode/1 でパースされ、不正な場合は
    %% {error, {parse_error, Reason}} か {error, {invalid_schema, Value}} が返る
    jsone_schema:add_schema(Key, RawJSON, #{}).


-spec validate(string(), binary()) ->
          {ok, jsone_schema:json_value()} |
          {error, malformed_json | {schema_not_found, binary()} | [jsone_schema_error:reason()]}.
validate(Key, RawJSON) ->
    try jsone:decode(RawJSON) of
        JSON ->
            jsone_schema:validate_key(Key, JSON)
    catch
        %% jsone:decode/1 は不正な JSON を error クラスで通知する
        %% 不正な JSON は呼び出し元で MalformedJSON として扱う
        error:_Reason ->
            {error, malformed_json}
    end.


-spec to_json([jsone_schema_error:reason()]) -> [map()].
to_json(Reasons) ->
    lists:map(fun reason_to_json/1, Reasons).


%% データの検証エラー
reason_to_json(#{kind := data} = Reason) ->
    #{
      invalid => data,
      schema => maps:get(schema, Reason),
      error => error_name(maps:get(error, Reason)),
      data => maps:get(value, Reason),
      path => maps:get(path, Reason)
     };
%% スキーマ自体のエラー
reason_to_json(#{kind := schema} = Reason) ->
    #{
      invalid => schema,
      schema => maps:get(schema, Reason),
      error => error_name(maps:get(error, Reason))
     }.


%% 詳細を持つエラーは名前だけを応答に載せる
error_name({Name, _Details}) ->
    Name;
error_name(Name) ->
    Name.
