-module(sample_interceptor).

-behaviour(swidden_interceptor).

-export([preprocess/2,
         preprocess/3,
         postprocess/3]).


preprocess(spam_user_handler, get_user, #{<<"username">> := <<"Ron">>}) ->
    {stop, swidden:success()};
preprocess(spam_user_handler, get_user, #{<<"username">> := <<"Harry">>}) ->
    {stop, swidden:success(#{<<"everyboby">> => <<"know him">>})};
preprocess(spam_user_handler, get_user, #{<<"username">> := <<"Dumbledore">>}) ->
    {stop, swidden:redirect(<<"http://example.com/albus">>)};
preprocess(spam_user_handler, get_user, #{<<"username">> := <<"Voldemort">>}) ->
    {stop, swidden:failure(<<"He-Who-Must-Not-Be-Named">>)};
preprocess(spam_user_handler, get_user, #{<<"username">> := <<"Snape">>}) ->
    {stop, swidden:failure(<<"insufficient privilege">>, #{<<"caution">> => <<"Slytherin only">>})};
preprocess(_M, _F, JSON) ->
    {continue, JSON}.


preprocess(spam_user_handler, redirect) ->
    {stop, swidden:failure(<<"not allowed">>)};
preprocess(_M, _F) ->
    continue.


postprocess(spam_user_handler, get_user, ok) ->
    ok;
postprocess(spam_user_handler, get_user, {ok, {redirect, Location}}) ->
    io:format(user, "redirect: ~p~n", [redirect]),
    {ok, {redirect, <<Location/binary, "?foo=bar">>}};
postprocess(spam_user_handler, get_user, {ok, Result}) ->
    {ok, Result#{<<"good_or_bad">> => <<"good">>}};
postprocess(spam_user_handler, get_user, {error, Type}) ->
    {error, <<Type/binary, ", You-Know-Who">>};
postprocess(spam_user_handler, get_user, {error, Type, Reason}) ->
    {error, Type, Reason#{<<"good_or_bad">> => <<"bad">>}};
postprocess(_M, _F, Result) ->
    Result.

