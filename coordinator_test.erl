-module(coordinator_test).
-include_lib("eunit/include/eunit.hrl").

-record(gcd_client, { name
                    , left_neighbor
                    , right_neighbor
                    }).
%client_name_test_() ->
  %[ ?_assertEqual("1234", gcd:client_name(1, 2, 3, 4))
  %].

%register_with_name_service_test_() ->
  %nameservice:start(),
  %coordinator:start(),
  %NameService = coordinator:ping_name_service('nameservice@localhost'),
  %NameService ! {lookup, chef},
  %receive
    %Coordinator -> Coordinator
  %end,
  %coordinator:stop(),
  %nameservice:stop(),

  %[ ?_assertEqual(self(), Coordinator)
  %].

build_ring_of_gcd_clients_test_() ->
  Clients = orddict:from_list([{1234, #gcd_client{name=1234}}, {3412, #gcd_client{name=3412}}, {4321, #gcd_client{name=4321}}]),
  ExpectedClientsWithRing = orddict:from_list([{1234, #gcd_client{name=1234,
                                                left_neighbor=4321,
                                                right_neighbor=3412}},
                                               {3412, #gcd_client{name=3412,
                                                left_neighbor=1234,
                                                right_neighbor=4321}},
                                               {4321, #gcd_client{name=4321,
                                                left_neighbor=3412,
                                                right_neighbor=1234}}
                                              ]),
  ClientsWithRing = coordinator:build_ring_of_gcd_clients(Clients),


  [ ?_assertEqual(ExpectedClientsWithRing, ClientsWithRing)
  ].
