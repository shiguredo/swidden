-module(spam_admin_handler).

-export([get_metrics/1]).


get_metrics(JSON) ->
    _Reset = proplists:get_value(<<"reset">>, JSON),
    swidden:success([{counter, 10}]).
