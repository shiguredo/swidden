-module(spam_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    {ok, _Pid} = swidden:start(spam, [{port, 5000}, {header_name, <<"x-spam-target">>}]),

    ok = spam_user:start(),

    spam_sup:start_link().


stop(_State) ->
    ok = swidden:stop(),
    ok.
