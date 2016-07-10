-module(swidden_dispatch).

-export([get_dispatches/0]).
-export([lookup/3]).

-export([start/1]).

-export_type([service/0, operation/0, version/0, id/0]).

-include("swidden.hrl").
-include("swidden_dispatch.hrl").

-type service() :: binary().
-type operation() :: binary().
-type version() :: binary().

-type id() :: {service(), version(), operation()}.

-define(TABLE, swidden_dispatch_table).


-spec get_dispatches() -> [#swidden_dispatch{}].
get_dispatches() ->
    ets:tab2list(?TABLE).


-spec lookup(service(), version(), operation()) -> not_found | {module(), function()}.
lookup(Service, Version, Operation) ->
    case ets:lookup(?TABLE, {Service, Version, Operation}) of
        [] ->
            not_found;
        [#swidden_dispatch{module = Module, schema = Schema}] ->
            %% Schema == Function
            {Module, Schema}
    end.


-spec start(atom()) -> ok.
start(Name) when is_atom(Name) ->
    case ets:info(?TABLE) of
        undefined ->
            _Tid = ets:new(?TABLE, [set, public, named_table, {keypos, #swidden_dispatch.id}]), 
            case load_dispatch_conf(Name) of
                ok ->
                    ok;
                {error, Reason} ->
                    error(Reason)
            end;
        _Info ->
            ?debugVal3(_Info),
            ok
    end.


%% INTERNAL

load_dispatch_conf(Name) ->
    case code:priv_dir(Name) of
        {error, Reason} ->
            {error, Reason};
        PrivPath ->
            %% <application>/priv/swidden/dispatch.conf
            Path = filename:join([PrivPath, "swidden", "dispatch.conf"]),
            case file:consult(Path) of
                {ok, Dispatch} ->
                    ok = load_services(Dispatch);
                {error, Reason} ->
                    {error, Reason}
            end
    end.


load_services([]) ->
    ok;
load_services([{Service, Versions}|Rest]) when is_binary(Service) ->
    %% TODO(nakai): Service が [a-zA-Z]+ かどうかチェックすること
    ok = load_versions(Service, Versions),
    load_services(Rest).


load_versions(_Service, []) ->
    ok;
load_versions(Service, [{Version, Operations}|Rest]) when is_binary(Version) ->
    %% TODO(nakai): Version が YYYYMMDD かどうかをチェックすること
    ok = load_operations(Service, Version, Operations),
    load_versions(Service, Rest).


load_operations(_Service, _Version, []) ->
    ok;
load_operations(Service, Version, [{Operation, Module}|Rest])
  when is_binary(Operation), is_atom(Module) ->
    %% TODO(nakai): Operation が CamelCase かチェックする
    %% atom 生成しているが、起動時の一回だけなので問題ない
    %% FIXME(nakai): ここで pascal2snake のエラーを探し出す
    Schema = binary_to_atom(swidden_misc:pascal2snake(Operation), utf8),
    true = ets:insert(?TABLE, #swidden_dispatch{id = {Service, Version, Operation},
                                                module = Module, schema = Schema}),
    load_operations(Service, Version, Rest).
