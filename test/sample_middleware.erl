-module(sample_middleware).

-behaviour(cowboy_middleware).

-export([execute/2]).

-include_lib("eunit/include/eunit.hrl").

execute(Req, #{handler_opts := HandlerOpts} = Env) ->
    UserID = 1,
    Env2 = Env#{handler_opts => [{user_id, UserID} | HandlerOpts]},
    {ok, Req, Env2}.
