-module(swidden).

-export([start/1, start/2, start/3, stop/1]).
-export([get_port/1]).
-export([success/0, success/1]).
-export([failure/1, failure/2]).
-export([redirect/1]).

-export_type([json_object/0, start_opt/0, start_opts/0]).

-define(DEFAULT_HEADER_NAME, <<"x-swd-target">>).

-define(REF, swidden_http_api).

-type json_object() :: jsone:json_object().

-type start_opt() :: {port, inet:port_number()}
                   | {app, atom()}
                   | {header_name, binary()}
                   | {services, [binary()]}
                   | {interceptor, module()}
                   | {ip, inet:ip_address()}
                   | {middlewares, [module()]}.

-type start_opts() :: [start_opt()].

-define(DEFAULT_IP_ADDRESS, {0, 0, 0, 0}).
-define(DEFAULT_PORT,       8000).


-spec start(atom()) -> {ok, pid()} | {error, term()} | no_return().
start(Name) when is_atom(Name) ->
    start(Name, ?DEFAULT_PORT, []).


-spec start(atom(), start_opts()) -> {ok, pid()} | {error, term()} | no_return().
start(Name, Opts) when is_atom(Name), is_list(Opts) ->
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),
    StartOpts = lists:keydelete(port, 1, Opts),
    start(Name, Port, StartOpts).


-spec start(atom(), inet:port_number(), start_opts()) -> {ok, pid()} | {error, term()} | no_return().
start(Name, Port, Opts) ->
    %% FIXME(nakai): error/1 で対応しているが本来は {error, Reason} で返すべき
    AppName = proplists:get_value(app, Opts, Name),
    ok = swidden_dispatch:start(AppName),
    ok = swidden_json_schema:start(AppName),

    HeaderName = proplists:get_value(header_name, Opts, ?DEFAULT_HEADER_NAME),

    %% dispatch.conf には Spam, SpamAdmin, Egg, EggAdmin があるとする
    %% Services に指定した文字列 [{services, [<<"Spam">>, <<"SpamAdmin">>]}]
    %% これが有効になり他は有効にならない
    %% バリデーション頑張ってないので要注意
    %% [] は全部に対応するという意味にする
    Services = proplists:get_value(services, Opts, []),
    Interceptor = proplists:get_value(interceptor, Opts, undefined),

    Dispatch = cowboy_router:compile(
                 [{'_', [{"/",
                          swidden_api_handler,
                          [{header_name, HeaderName},
                           {services, Services},
                           {interceptor, Interceptor}]}]}]),

    IpAddress = proplists:get_value(ip, Opts, ?DEFAULT_IP_ADDRESS),

    ProtoOpts = case proplists:get_value(middlewares, Opts, not_found) of
                    not_found ->
                        #{};
                    Middlewares ->
                        #{middlewares => Middlewares}
                end,

    %% コード的に意味不明
    Env = ProtoOpts#{env => #{dispatch => Dispatch}},

    cowboy:start_clear({?REF, Name}, [{ip, IpAddress}, {port, Port}], Env).


-spec stop(atom()) -> ok.
stop(Name) ->
    %% TODO(v); ets 周りも削除する
    ok = cowboy:stop_listener({?REF, Name}).


-spec success() -> ok.
success() ->
    ok.


-spec success(json_object()) -> {ok, json_object()}.
success(JSON) ->
    {ok, JSON}.


%% TODO(v); 戻り値を考える
-spec failure(binary()) -> {error, binary()}.
failure(Type) when is_binary(Type) ->
    {error, Type}.


-spec failure(binary(), map()) -> {error, binary(), map()}.
failure(Type, Reason) when is_binary(Type) andalso is_map(Reason) ->
    {error, Type, Reason}.


-spec redirect(binary()) -> {ok, {redirect, binary()}}.
redirect(Location) when is_binary(Location) ->
    {ok, {redirect, Location}}.


%% 0 番 port でリッスンした場合のポート番号
-spec get_port(atom()) -> inet:port_number().
get_port(Name) when is_atom(Name) ->
    ranch:get_port({?REF, Name}).
