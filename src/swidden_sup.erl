-module(swidden_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).


-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec init(any()) ->
          {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
