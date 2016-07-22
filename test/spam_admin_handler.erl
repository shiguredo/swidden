-module(spam_admin_handler).

-export([get_metrics/1]).


get_metrics(#{<<"reset">> := _Reset}) ->
    swidden:success([{counter, 10}]).
