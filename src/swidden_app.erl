-module(swidden_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    swidden_sup:start_link().


stop(_State) ->
    ok.
