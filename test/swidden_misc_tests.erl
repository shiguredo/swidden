-module(swidden_misc_tests).

-import(swidden_misc, [pascal2snake/1]).

-include_lib("eunit/include/eunit.hrl").


pascal2snake_test() ->
    ?assertEqual(<<"a">>, pascal2snake(<<"A">>)),
    ?assertEqual(<<"abc">>, pascal2snake(<<"Abc">>)),
    ?assertEqual(<<"abc_def">>, pascal2snake(<<"AbcDef">>)),
    ?assertEqual(<<"abc123_def">>, pascal2snake(<<"Abc123Def">>)),
    ?assertEqual(<<"abc123def">>, pascal2snake(<<"Abc123def">>)),
    ?assertEqual(<<"abc">>, pascal2snake(<<"ABC">>)),
    ?assertEqual(<<"abc_def">>, pascal2snake(<<"ABCDef">>)),
    ?assertEqual(<<"abc_def">>, pascal2snake(<<"AbcDEF">>)),
    ?assertEqual(<<"abc123">>, pascal2snake(<<"ABC123">>)),
    ?assertEqual(<<"abc123_def">>, pascal2snake(<<"ABC123Def">>)),
    ?assertEqual(<<"abc123def">>, pascal2snake(<<"ABC123def">>)),
    ?assertEqual(<<"abc123_def">>, pascal2snake(<<"Abc123DEF">>)),
    ?assertEqual(<<"abc123_def">>, pascal2snake(<<"ABC123DEF">>)),
    ?assertEqual(no_pascalcase, pascal2snake(<<"123Def">>)),
    ?assertEqual(no_pascalcase, pascal2snake(<<"a">>)),
    ok.
