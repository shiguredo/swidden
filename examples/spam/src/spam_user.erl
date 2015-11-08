-module(spam_user).

-export([start/0]).
-export([get_user/2, create_user/2, update_user/2, delete_user/2]).

-define(TABLE, spam_user_table).


start() ->
    _Tid = ets:new(?TABLE, [set, public, named_table]),
    ok.


get_user(JSON, _Opts) ->
    Username = proplists:get_value(<<"username">>, JSON),
    case ets:lookup(?TABLE, Username) of
        [] ->
            swidden:failure(<<"MissingUserException">>);
        [{Username, Password}] ->
            %% proplists を戻せば JSON で返ります
            swidden:success([{password, Password}]);
        [{Username, Password, _Group}] ->
            %% spam_user_with_group 対応
            swidden:success([{password, Password}])
    end.


create_user(JSON, _Opts) ->
    Username = proplists:get_value(<<"username">>, JSON),
    Password = proplists:get_value(<<"password">>, JSON),
    case ets:insert_new(?TABLE, {Username, Password}) of
        true ->
            swidden:success();
        false ->
            swidden:failure(<<"DuplicateUserException">>)
    end.


update_user(JSON, _Opts) ->
    Username = proplists:get_value(<<"username">>, JSON),
    Password = proplists:get_value(<<"password">>, JSON),
    case ets:lookup(?TABLE, Username) of
        [] ->
            swidden:failure(<<"MissingUserException">>);
        [{Username, _OldPassword}] ->
            true = ets:insert(?TABLE, {Username, Password}),
            swidden:success();
        [{Username, _OldPassword, Group}] ->
            %% spam_user_with_group 対応
            true = ets:insert(?TABLE, {Username, Password, Group}),
            swidden:success()
    end.


delete_user(JSON, _Opts) ->
    Username = proplists:get_value(<<"username">>, JSON),
    case ets:lookup(?TABLE, Username) of
        [] ->
            swidden:failure(<<"MissingUserException">>);
        _ ->
            true = ets:delete(?TABLE, Username),
            swidden:success()
    end.
