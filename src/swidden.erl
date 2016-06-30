-module(swidden).

-export([start/1, start/2, stop/0]).
-export([success/0, success/1, failure/1]).
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

    Dispatch = cowboy_router:compile([
        {'_', [{"/", swidden_api_handler, [{header_name, HeaderName}]}]}
    ]),

    Port = proplists:get_value(port, Opts, 8000),

    ProtoOpts = case proplists:get_value(middlewares, Opts, not_found) of
                    not_found ->
                        [];
                    Middlewares ->
                        [{middlewares, Middlewares}]
                end,
    ProtoOpts2 = case proplists:get_value(onresponse, Opts, not_found) of
                     not_found ->
                         ProtoOpts;
                     OnResponse ->
                         [{onresponse, OnResponse} | ProtoOpts]
                 end,

    Env = {env, [{dispatch, Dispatch}]},

    cowboy:start_http(?REF, 10, [{port, Port}], [Env|ProtoOpts2]).


-spec stop() -> ok.
stop() ->
    %% TODO(nakai): ets 周りも削除する
    ok = cowboy:stop_listener(?REF). 


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
