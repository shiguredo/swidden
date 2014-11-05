-module(spam_admin).

-export([get_metrics/1]).


get_metrics(JSON) ->
    _Reset = proplists:get_value(reset, JSON),
    %% FIXME(nakai): ダミーデータ
    swidden:success([{counter, 0}]).
