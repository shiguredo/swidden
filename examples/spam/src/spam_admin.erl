-module(spam_admin).

-export([get_metrics/2]).


get_metrics(JSON, _Opts) ->
    _Reset = proplists:get_value(reset, JSON),
    %% FIXME(nakai): ダミーデータ
    swidden:success([{counter, 0}]).
