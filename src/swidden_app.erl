-module(swidden_app).

-behaviour(application).

-export([start/2, stop/1]).


-spec start(application:start_type(), any()) -> {ok, pid()} | {error, term()} | ignore.
start(_StartType, _StartArgs) ->
    swidden_sup:start_link().


-spec stop(any()) -> ok.
stop(_State) ->
    ok.
