-module(spam_user_with_group).

-export([create_user/1]).

-define(TABLE, spam_user_with_group_table).


create_user(JSON) ->
    Username = proplists:get_value(<<"username">>, JSON),
    Password = proplists:get_value(<<"password">>, JSON),
    Group = proplists:get_value(<<"group">>, JSON),
    case ets:insert_new(?TABLE, {Username, Password, Group}) of
        true ->
            swidden:success();
        false ->
            swidden:failure(<<"DuplicateUserException">>)
    end.
