-module(swidden_misc).

-export([pascal2snake/1]).

%% -spec is_pascalcase(binary()) -> ok | error.

-spec pascal2snake(binary()) -> no_pascalcase | binary().
pascal2snake(<<C:8, _/binary>> = Binary) when $A =< C andalso C =< $Z ->
    %% 大文字が２個以上 + 小文字 みたいなパターンの最後の大文字のまえに "_" を入れる
    Binary2 = re:replace(Binary, "([A-Z]+)([A-Z][a-z])", "\\1_\\2", [{return, binary}, global]),
    %% "小文字または数字"と大文字の間に "_" を入れる
    Binary3 = re:replace(Binary2, "([a-z\\d])([A-Z])", "\\1_\\2", [{return, binary}, global]),
    %% すべて小文字にする
    downcase(Binary3);
pascal2snake(_Binary) ->
    no_pascalcase.


%% Internal

downcase(Binary) ->
    downcase(Binary, <<>>).

downcase(<<>>, Acc) ->
    Acc;
downcase(<<C:8, Rest/binary>>, Acc) when $A =< C andalso C =< $Z ->
    downcase(Rest, <<Acc/binary, (C + 32):8>>);
downcase(<<C:8, Rest/binary>>, Acc) ->
    downcase(Rest, <<Acc/binary, C:8>>).
