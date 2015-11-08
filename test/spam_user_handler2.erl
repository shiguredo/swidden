-module(spam_user_handler2).

-export([create_user/2]).


create_user(JSON, _Opts) ->
    _Username = proplists:get_value(<<"username">>, JSON),
    _Password = proplists:get_value(<<"password">>, JSON),
    _Group = proplists:get_value(<<"group">>, JSON),
    swidden:failure(<<"DuplicatedUser">>).
