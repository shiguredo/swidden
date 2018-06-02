-module(swidden_json_schema).

-export([start/1]).
-export([validate_json/4]).
-export([to_json/1]).

-include("swidden.hrl").
-include("swidden_dispatch.hrl").

%% TODO(nakai): req/res 両方のスキーマが必要なのでは ...
    %% TODO(nakai): レスポンスのバリデーションチェックはどうする？


-spec start(atom()) -> ok | {error, term()}.
start(Name) ->
    case load_schemas(Name) of
        ok ->
            ok;
        {error, Reason} ->
            %% FIXME(nakai): 手抜き
            error(Reason)
    end.


-spec validate_json(binary(), binary(), binary(), binary()) -> {ok, module(), atom(), jsone:json_term()} | {error, term()}.
validate_json(Service, Version, Operation, RawJSON) ->
    Key = binary_to_list(list_to_binary([Service, $_, Version, $., Operation])),
    case validate(Key, RawJSON) of
        {ok, JSON} ->
            case swidden_dispatch:lookup(Service, Version, Operation) of
                not_found ->
                    {error, missing_routing};
                {Module, Function} ->
                    {ok, Module, Function, JSON}
            end;
        {error, Reason} ->
            {error, Reason}
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
load_schemas(Path, [#swidden_dispatch{id = {Service, Version, Operation},
                                      schema = Schema}|Rest]) ->
    FileName = lists:flatten(lists:join(".", [atom_to_list(Schema), "json"])),
    PascalCaseService = swidden_misc:pascal2snake(Service),
    %% <application>/priv/swidden/schemas/<service>/<version>/<schema>.json
    FilePath = filename:join([Path, "schemas", PascalCaseService, Version, FileName]),
    case file:read_file(FilePath) of
        {ok, Binary} ->
            Key = binary_to_list(list_to_binary([Service, $_, Version, $., Operation])),
            case add_schema(Key, Binary) of
                ok ->
                    load_schemas(Path, Rest);
                [{[],[],[]}] ->
                    load_schemas(Path, Rest);
                [{_, _, {error, invalid_json, LineNumber}}] ->
                    {error, {invalid_json, FileName, LineNumber}}
            end;
        {error, Reason} ->
            {error, {FilePath, Reason}}
    end.


-spec add_schema(string(), binary()) -> ok | jesse_error:error().
add_schema(_Key, <<>>) ->
    ok;
add_schema(Key, RawJSON) ->
    jesse:add_schema(Key, RawJSON, [{parser_fun, parse_fun()}]).


-spec validate(string(), binary()) -> {ok, jesse:json_term()} | jesse_error:error() | jesse_database:error().
validate(Key, RawJSON) ->
    jesse:validate(Key, RawJSON, [{parser_fun, parse_fun()}]).


-spec parse_fun() -> function().
parse_fun() ->
    fun(Binary) -> jsone:decode(Binary) end.


to_json(Reasons) ->
    F = fun({data_invalid, Schema, {Error, _}, Data, Path}) ->
                #{invalid => data,
                  schema => Schema,
                  error => Error,
                  data => Data,
                  path => Path};
           ({data_invalid, Schema, Error, Data, Path}) ->
                #{invalid => data,
                  schema => Schema,
                  error => Error,
                  data => Data,
                  path => Path}
        end,
    lists:map(F, Reasons).
