-module(spam_admin_handler).

-export([get_metrics/2]).


get_metrics(JSON, _Opts) ->
    _Reset = proplists:get_value(<<"reset">>, JSON),
    swidden:success([{counter, 10}]).
