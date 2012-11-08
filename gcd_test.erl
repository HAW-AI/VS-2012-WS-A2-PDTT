-module(gcd_test).
-include_lib("eunit/include/eunit.hrl").

client_name_test_() ->
  [ ?_assertEqual("1234", gcd:client_name(1, 2, 3, 4))
  ].

name_service_test_() ->
  nameservice:start(),
  Success = case gcd:name_service(node()) of
    {ok, NameService} ->
      NameService ! {self(), kill},
      receive
        ok -> true;
        _  -> false
      after timer:seconds(1) ->
        false
      end;

    error ->
      false
  end,

  [ ?_assert(Success)
  ].