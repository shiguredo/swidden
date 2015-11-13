-module(spam_user_handler).

-export([get_user/1, create_user/1, update_user/1, delete_user/1,
         get_authenticated_user/1, update_authenticated_user/2,
         list_users/0]).


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


get_authenticated_user(Opts) ->
    _UserID = proplists:get_value(user_id, Opts),
    swidden:success().


update_authenticated_user(_JSON, Opts) ->
    _UserID = proplists:get_value(user_id, Opts),
    swidden:success().


list_users() ->
    swidden:success([[{username, <<"username">>}, {password, <<"password">>}]]).
