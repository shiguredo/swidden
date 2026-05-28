-module(swidden_tests).

-include("swidden.hrl").

-include_lib("eunit/include/eunit.hrl").

%% 下記二つを使ったテスト
%% swidden/priv/swidden/dispatch.conf
%% swidden/priv/swidden/schemas

-define(APPS, [gun, ranch, cowlib, cowboy, swidden]).


start_apps() ->
    [ application:ensure_all_started(App) || App <- ?APPS ].


stop_apps() ->
    [ application:stop(App) || App <- lists:reverse(?APPS) ].


all_test_() ->
    {foreach,
     fun() ->
             start_apps(),
             ok
     end,
     fun(_) ->
             stop_apps(),
             ok
     end,
     [fun setup/0]}.


setup() ->
    {ok, _Pid} = swidden:start(swidden, [{port, 0}]),

    ?assertEqual({spam_user_handler, get_user},
                 swidden_dispatch:lookup(<<"Spam">>, <<"20141101">>, <<"GetUser">>)),
    ?assertEqual({spam_user_handler, create_user},
                 swidden_dispatch:lookup(<<"Spam">>, <<"20141101">>, <<"CreateUser">>)),
    ?assertEqual({spam_user_handler, update_user},
                 swidden_dispatch:lookup(<<"Spam">>, <<"20141101">>, <<"UpdateUser">>)),
    ?assertEqual({spam_user_handler, delete_user},
                 swidden_dispatch:lookup(<<"Spam">>, <<"20141101">>, <<"DeleteUser">>)),
    ?assertEqual({spam_user_handler2, create_user},
                 swidden_dispatch:lookup(<<"Spam">>, <<"20150701">>, <<"CreateUser">>)),
    ?assertEqual({spam_admin_handler, get_metrics},
                 swidden_dispatch:lookup(<<"SpamAdmin">>, <<"20141101">>, <<"GetMetrics">>)),

    ?assertEqual(ok, swidden:stop(swidden)),
    ok.
