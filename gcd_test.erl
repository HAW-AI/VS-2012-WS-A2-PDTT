-module(gcd_test).
-include_lib("eunit/include/eunit.hrl").

process_name_test_() ->
  [ ?_assertEqual("1234", gcd:process_name(1, 2, 3, 4))
  ].