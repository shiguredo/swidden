-module(spam_user_handler).

-export([get_user/1, create_user/1, update_user/1, delete_user/1,
         get_authenticated_user/1, update_authenticated_user/2,
         list_users/0]).
-export([crash/1]).


get_user(#{<<"username">> := _Username}) ->
    swidden:success([{password, <<"password">>}]).


create_user(#{<<"username">> := _Username, <<"password">> := _Password}) ->
    swidden:success().


update_user(#{<<"username">> := _Username, <<"password">> := _Password}) ->
    swidden:success().


delete_user(#{<<"username">> := _Username}) ->
    swidden:success().


get_authenticated_user(Opts) ->
    _UserID = proplists:get_value(user_id, Opts),
    swidden:success().


update_authenticated_user(_JSON, Opts) ->
    _UserID = proplists:get_value(user_id, Opts),
    swidden:success().


list_users() ->
    swidden:success([#{username => <<"username">>, password => <<"password">>}]).


crash(JSON) ->
    case JSON of
        error ->
            ok
    end.
