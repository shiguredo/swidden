-include_lib("eunit/include/eunit.hrl").

-define(debugVal2(E),
    ((fun (__V) ->
          ?debugFmt(<<"~s = ~P">>, [(??E), __V, 255]),
          __V
      end)(E))).
