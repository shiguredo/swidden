-module(spam_user_handler).

-export([get_user/1, create_user/1, update_user/1, delete_user/1]).


get_user(JSON) ->
    _Username = proplists:get_value(<<"username">>, JSON),
    swidden:success([{password, <<"password">>}]).


create_user(JSON) ->
    _Username = proplists:get_value(<<"username">>, JSON),
    _Password = proplists:get_value(<<"password">>, JSON),
    swidden:success().


update_user(JSON) ->
    _Username = proplists:get_value(<<"username">>, JSON),
    _Password = proplists:get_value(<<"password">>, JSON),
    swidden:success().


delete_user(JSON) ->
    _Username = proplists:get_value(<<"username">>, JSON),
    swidden:success().


