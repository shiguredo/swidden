-module(sample_middleware).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    HandlerOpts = proplists:get_value(handler_opts, Env),
    UserID = 1,
    Env2 = lists:keyreplace(handler_opts, 1, Env, {handler_opts, [{user_id, UserID} | HandlerOpts]}),
    {ok, Req, Env2}.
