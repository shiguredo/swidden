-module(spam_user_handler2).

-export([create_user/1]).


create_user(#{<<"username">> := Username,
              <<"password">> := _Password,
              <<"group">> := _Group}) ->
    case Username of
        <<"error_code">> ->
            swidden:failure(<<"DuplicatedUser">>, #{error_code => 666});
        _ ->
            swidden:failure(<<"DuplicatedUser">>)
    end.
