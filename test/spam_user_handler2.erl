-module(spam_user_handler2).

-export([create_user/1]).


create_user(JSON) ->
    Username = proplists:get_value(<<"username">>, JSON),
    _Password = proplists:get_value(<<"password">>, JSON),
    _Group = proplists:get_value(<<"group">>, JSON),
    case Username of
        <<"error_code">> ->
            swidden:failure(<<"DuplicatedUser">>, #{error_code => 666});
        _ ->
            swidden:failure(<<"DuplicatedUser">>)
    end.
