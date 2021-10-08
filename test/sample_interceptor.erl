-module(sample_interceptor).

-behaviour(swidden_interceptor).

-export([execute/2,
         execute/3]).


execute(spam_user_handler, get_user, #{<<"username">> := <<"Ron">>}) ->
    {stop, swidden:success()};
execute(spam_user_handler, get_user, #{<<"username">> := <<"Harry">>}) ->
    {stop, swidden:success(#{<<"everyboby">> => <<"know him">>})};
execute(spam_user_handler, get_user, #{<<"username">> := <<"Dumbledore">>}) ->
    {stop, swidden:redirect(<<"http://example.com/albus">>)};
execute(spam_user_handler, get_user, #{<<"username">> := <<"Voldemort">>}) ->
    {stop, swidden:failure(<<"He-Who-Must-Not-Be-Named">>)};
execute(spam_user_handler, get_user, #{<<"username">> := <<"Snape">>}) ->
    {stop, swidden:failure(<<"insufficient privilege">>, #{<<"caution">> => <<"Slytherin only">>})};
execute(_M, _F, JSON) ->
    {continue, JSON}.


execute(spam_user_handler, redirect) ->
    {stop, swidden:failure(<<"not allowed">>)};
execute(_M, _F) ->
    continue.
