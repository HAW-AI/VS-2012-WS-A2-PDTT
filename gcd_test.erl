-module(gcd_test).
-include_lib("eunit/include/eunit.hrl").
-define (COORDINATOR_NAME, chef).

% start_test() ->
%   nameservice:start(),
%   {ok, NameService} = gcd:name_service(node()),
%   coordinator:start(),
%   {ok, Coordinator} = gcd:coordinator(NameService, ?COORDINATOR_NAME),
%   PID = spawn(fun() -> gcd:start(0, 0, 0, 0, 0, 0, node(), ?COORDINATOR_NAME) end),
%   PID ! kill,
%   kill_nameservice(NameService).


client_name_test_() ->
  [ ?_assertEqual('1234', gcd:client_name(1, 2, 3, 4))
  ].

name_service_test_() ->
  nameservice:start(),
  Success = case gcd:name_service(node()) of
    {ok, NameService} ->
      kill_nameservice(NameService);

    error ->
      false
  end,

  [ ?_assert(Success)
  ].

% coordinator_test() ->
%   nameservice:start(),
%   {ok, NameService} = gcd:name_service(node()),
%   coordinator:start(),
%   {ok, Coordinator} = gcd:coordinator(NameService, ?COORDINATOR_NAME),
%   kill_nameservice(NameService).

kill_nameservice(NameService) ->
  NameService ! {self(), kill},
  receive
    ok -> true;
    _  -> false
  after timer:seconds(1) ->
    false
  end.