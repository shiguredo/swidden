-module(swidden).

-export([start/1, start/2, stop/1]).
-export([success/0, success/1]).
-export([failure/1, failure/2]).
-export_type([json_object/0]).

-define(DEFAULT_HEADER_NAME, <<"x-swd-target">>).

-define(REF, swidden_http_api).

-type json_object() :: jsone:json_object().

%% TODO(nakai): Target も Opts に入れてしまうかどうか検討すること

start(Name) when is_atom(Name) ->
    start(Name, []).

start(Name, Opts) ->
    %% FIXME(nakai): error/1 で対応しているが本来は {error, Reason} で返すべき
    ok = swidden_dispatch:start(Name),
    ok = swidden_json_schema:start(Name),

    HeaderName = proplists:get_value(header_name, Opts, ?DEFAULT_HEADER_NAME),

    %% dispatch.conf には Spam, SpamAdmin, Egg, EggAdmin があるとする
    %% Services に指定した文字列 [{services, [<<"Spam">>, <<"SpamAdmin">>]}]
    %% これが有効になり他は有効にならない
        %% バリデーション頑張ってないので要注意
    %% [] は全部に対応するという意味にする
    Services = proplists:get_value(services, Opts, []),

    Dispatch = cowboy_router:compile([
        {'_', [{"/", swidden_api_handler, [{header_name, HeaderName}, {services, Services}]}]}
    ]),

    Port = proplists:get_value(port, Opts, 8000),

    %% コード的に意味不明
    Env = #{env => #{dispatch => Dispatch}},

    cowboy:start_clear({?REF, Port}, 10, [{port, Port}], Env).


-spec stop(inet:port_number()) -> ok.
stop(Port) ->
    %% TODO(nakai): ets 周りも削除する
    ok = cowboy:stop_listener({?REF, Port}). 


-spec success() -> ok.
success() ->
    ok.


-spec success(json_object()) -> {ok, json_object()}.
success(JSON) ->
    {ok, JSON}.


%% TODO(nakai): 戻り値を考える
-spec failure(binary()) -> {error, binary()}.
failure(Type) when is_binary(Type) ->
    {error, Type}.


failure(Type, Reason) when is_binary(Type) andalso is_map(Reason) ->
    {error, Type, Reason}.

