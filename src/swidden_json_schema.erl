-module(swidden_json_schema).

-export([start/1]).
-export([validate_json/4]).

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
    

-spec validate_json(binary(), binary(), binary(), binary()) -> ok | {error, term()}.
validate_json(Service, Version, Operation, RawJSON) ->
    case validate(Service, Version, Operation, RawJSON) of
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
    FileName = string:join([atom_to_list(Schema), "json"], "."),
    PascalCaseService = swidden_misc:pascal2snake(Service),
    %% <application>/priv/swidden/schemas/<service>/<version>/<schema>.json
    FilePath = filename:join([Path, "schemas", PascalCaseService, Version, FileName]),
    case file:read_file(FilePath) of
        {ok, Binary} ->
            case add_schema(Service, Version, Operation, Binary) of
                ok ->
                    load_schemas(Path, Rest);
                [{[],[],[]}] ->
                    load_schemas(Path, Rest);
                [{_, _, {error, invalid_json, LineNumber}}] ->
                    {error, {invalid_json, FileName, LineNumber}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


-spec add_schema(binary(), binary(), binary(), binary()) -> ok | {error, term()}.
add_schema(_Service, _Version, _Operation, <<>>) ->
    ok;
add_schema(Service, Version, Operation, RawJSON) ->
    jesse:add_schema({Service, Version, Operation}, RawJSON, [{parser_fun, parse_fun()}]).


-spec validate(binary(), binary(), binary(), binary()) -> ok | {error, {database_error, atom(), schema_not_found} | term()}.
validate(Service, Version, Operation, RawJSON) ->
    jesse:validate({Service, Version, Operation}, RawJSON, [{parser_fun, parse_fun()}]).


-spec parse_fun() -> function().
parse_fun() ->
    fun(Binary) -> jsone:decode(Binary, [{format, proplist}]) end.
