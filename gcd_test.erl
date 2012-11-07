-module(gcd_test).
-include_lib("eunit/include/eunit.hrl").

client_name_test_() ->
  [ ?_assertEqual("1234", gcd:client_name(1, 2, 3, 4))
  ].