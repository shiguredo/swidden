-module(spam_user_handler).

-export([get_user/2, create_user/2, update_user/2, delete_user/2, list_users/1]).


get_user(JSON, _Opts) ->
    _Username = proplists:get_value(<<"username">>, JSON),
    swidden:success([{password, <<"password">>}]).


create_user(JSON, _Opts) ->
    _Username = proplists:get_value(<<"username">>, JSON),
    _Password = proplists:get_value(<<"password">>, JSON),
    swidden:success().


update_user(JSON, _Opts) ->
    _Username = proplists:get_value(<<"username">>, JSON),
    _Password = proplists:get_value(<<"password">>, JSON),
    swidden:success().


delete_user(JSON, _Opts) ->
    _Username = proplists:get_value(<<"username">>, JSON),
    swidden:success().


list_users(_Opts) ->
    swidden:success([[{username, <<"username">>}, {password, <<"password">>}]]).
