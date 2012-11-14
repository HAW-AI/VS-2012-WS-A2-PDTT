-module(coordinator).

-compile([export_all]).

-record(state, { clients=orddict:new()
               , config
               }).
-record(gcd_client, { name
                    , left_neighbor
                    , right_neighbor
                    }).

start() ->
  spawn(fun initialize/0).

initialize() ->
  %%% read config from file into State
  State = read_config_into_state(#state{}),

  %%% register coordinator name locally
  register(get_coordinator_name(State), self()),

  %%% ping NameServiceNode
  case ping_name_service(get_nameservice_node_name(State)) of
    {ok, NameService} ->
      %%% first time bind of our service with the nameservice
      %%% use rebind just in case a previous coordinator did not shut down cleanly
      NameService ! {self(), {rebind, get_coordinator_name(State), node()}},

      receive
        ok ->
          log("Erfolgreich beim Namensdienst gebunden"),

          %%% done with the start phase. everything worked. go into initial state.
          initial(State);

        in_use ->
          %%% something went wrong. the coordinator is already bound at the Nameservice.
          log_error("Binden beim Namesdienst fehlgeschlagen. Der Name ist bereits vergeben."),
          terminating(State)
        end;

    error ->
      log_error("Namensdienst nicht verfuegbar"),
      terminating(State)

  end.

%%% initial state the coordinator goes into after the start phase
initial(State) ->
  log("Eintritt in Zustand: Initial"),
  receive
    % Die Anfrage nach den steuernden Werten durch den Starter Prozess.
    {getsteeringval, StarterPID} ->
      log("Starter fragt nach Steuerwerten."),
      StarterPID ! {steeringval,
                    get_processing_time(State),
                    get_termination_time(State),
                    get_gcd_process_number(State)},

      initial(State);

    % Ein ggT-Prozess meldet sich beim Koordinator mit Namen Clientname an (Name ist der lokal registrierte Name!).
    {hello, ClientName} ->
      log(format("Client hat sich registriert unter namen: ~p", [ClientName])),
      ClientNameToBeSaved = case werkzeug:type_is(ClientName) of
        list -> list_to_atom(ClientName);
        atom -> ClientName;
        _ ->
          log_error(format("Received Clientname that is neither string nor atom: ~p", [ClientName])),
          initial(State)
      end,
      initial(register_gcd_client(State, ClientNameToBeSaved));

    %%% once somebody triggers the calculation of the distributed gcd
    %%% the coordinator starts building "The Ring"
    get_ready ->
      get_ready(State);

    %%% the coordinator has to kill all gcd clients, unbind it's name and shutdown
    kill ->
      terminating(State);

    _Unknown ->
      %%% TODO handle unknown message. need to catch these otherwise they will
      %%% still be in the process' mailbox when we switch states and then
      %%% they might match which will result in unexpected behaviour
      initial(State)

  end,
  ok.

get_ready(State) ->
  log("Entering State: Get Ready"),
  %%% arrange the gcd clients in a ring and go into ready state
  log("Building ring of GCD Clients"),
  ClientsWithRing = case build_ring_of_gcd_clients(get_clients(State)) of
    error -> terminating(State);
    ClientsRing -> ClientsRing
  end,

  StateWithRing = State#state{clients = ClientsWithRing},

  %%% this sends the {setneighbors, LeftN, RightN} message to each client
  introduce_clients_to_their_neighbors(StateWithRing),

  log("Entering State: Ready"),
  ready(StateWithRing).

ready(State) ->
  receive
    start_distributed_gcd_calculation ->
      {GCD, _} = calculate_gcd_seed(),
      start_gcd_process(State, GCD),
      ready(State);

    {start_distributed_gcd_calculation, GCD} when
        is_integer(GCD) andalso GCD > 0 ->
      start_gcd_process(State, GCD),
      ready(State);

    {briefmi, {ClientName, CMi, CZeit}} ->
      log(format("Client ~p calculated new Mi ~p at ~p", [ClientName, CMi, CZeit])),
      ready(State);

    {briefterm, {ClientName, CMi, CZeit}} ->
      log(format("Client ~p finished calculation with Mi ~p at ~p", [ClientName, CMi, CZeit])),
      ready(State);

    reset ->
      log("Received reset command. Killing all gcd clients and going into initial state"),
      kill_all_gcd_clients(State),
      initial(State#state{clients=orddict:new()});

    %%% the coordinator has to kill all gcd clients, unbind it's name and shutdown
    kill ->
      terminating(State)
  end,
  ok.

terminating(State) ->
  log("Coordinator received kill command"),
  log("Start killing GCD clients"),
  %%% send the kill command to all registered clients
  kill_all_gcd_clients(State),

  log("Trying to unbind coordinator name at nameservice"),
  global:whereis_name(nameservice) ! {self(), {unbind, get_coordinator_name(State)}},
  log("Terminating coordinator process. Goodbye."),
  exit(self()).

%%% call this module function and it will start the coordiator that is
%%% registered with the name service, given that coordinator is in the
%%% initial state.
start_distributed_gcd_calculation() ->
  send_message_to_coordinator(start_distributed_gcd_calculation).

start_distributed_gcd_calculation(GCD) when is_integer(GCD) andalso GCD > 0 ->
  send_message_to_coordinator({start_distributed_gcd_calculation, GCD}).

get_ready() ->
  send_message_to_coordinator(get_ready).

reset() ->
  send_message_to_coordinator(reset).

stop() ->
  kill().

kill() ->
  send_message_to_coordinator(kill).

%%% read config from file into state and return new state
read_config_into_state(State) ->
  {ok, Config} = read_config_from_file(),
  State#state{config = Config}.

%%% useful getter functions

read_config_from_file() ->
  file:consult("coordinator.cfg").

%%% get config from state record
get_config(State) ->
  State#state.config.

%%% get config values from config within state
get_processing_time(State) ->
  Config = get_config(State),
  proplists:get_value(arbeitszeit, Config).

get_termination_time(State) ->
  Config = get_config(State),
  proplists:get_value(termzeit, Config).

get_gcd_process_number(State) ->
  Config = get_config(State),
  proplists:get_value(ggtprozessnummer, Config).

get_nameservice_node_name(State) ->
  Config = get_config(State),
  proplists:get_value(nameservicenode, Config).

get_coordinator_name(State) ->
  Config = get_config(State),
  proplists:get_value(koordinatorname, Config).

%%% get clients dictionary from state
get_clients(State) ->
  State#state.clients.

%%% update the clients dictionary with another client
update_clients_with_client(Clients, ClientName, UpdatedClient) ->
  orddict:store(ClientName, UpdatedClient, Clients).

%%% register gcd client and return new state
register_gcd_client(State, ClientName) ->
  Clients = get_clients(State),
  UpdatedClients = update_clients_with_client(Clients, ClientName, #gcd_client{name=ClientName}),
  State#state{clients=UpdatedClients}.

%%% build a ring of the registered gcd clients where each gcd client
%%% knows his left and right neighbor.
%%% Pivot: first ClientName from which we start building the ring
%%% Clients: State#state.clients Dictionary with ClientName -> gcd_client entries
%%%
%%% Returns: an updated Clients Dictionary
build_ring_of_gcd_clients(Clients) ->
  log("Baue Ring aus GGT Clients"),
  %%% it is not possible/ill-adviced to build a ring with only one client.
  %%% that client would have himself as his left and right neighbor and would
  %%% send himself 2 messages.
  %%% TODO decide if we should increment this to < 3 to have distinct neighbors
  case orddict:size(Clients) < 2 of
    true ->
      log_error("Building the GCD ring failed."),
      log_error("Building a ring of less than two clients is not possible"),
      error;
    false ->
      ClientsList = orddict:fetch_keys(Clients),
      [Pivot | Tail] = ClientsList,
      build_ring_of_gcd_clients(Clients,
                                orddict:fetch(Pivot, Clients),
                                Tail,
                                none)
  end.

build_ring_of_gcd_clients(Clients, Pivot, RemainingClientsList, none) ->
  %%% initial call:
  build_ring_of_gcd_clients(Clients,
                            Pivot,
                            RemainingClientsList,
                            Pivot);

build_ring_of_gcd_clients(Clients, Pivot, [], PreviousClient) ->
  %%% empty RemainingClientsList:
  %%% all clients have been updated. all that is missing is the left neighbor
  %%% of the Pivot element and the right_neighbor of the PreviousClient
  {ok, PivotFromClientsDictionary} = orddict:find(Pivot#gcd_client.name, Clients),
  FinishedPivot = PivotFromClientsDictionary#gcd_client{left_neighbor=PreviousClient#gcd_client.name},

  %%% 3. set FinishedPivot as the right_neighbor of the PreviousClient
  FinishedPreviousClient = PreviousClient#gcd_client{right_neighbor=FinishedPivot#gcd_client.name},

  %%% return the updated Clients Dictionary with the ring
  UpdatedClients = update_clients_with_client(Clients,
                                              FinishedPivot#gcd_client.name,
                                              FinishedPivot),

  log("Bauen des Rings aus GGT Clients erfolgreich"),
  update_clients_with_client(UpdatedClients,
                             FinishedPreviousClient#gcd_client.name,
                             FinishedPreviousClient);

build_ring_of_gcd_clients(Clients, Pivot, RemainingClientsList, PreviousClient) ->
  %%% there are remaining clients. recursively traverse the RemainingClientsList:
  %%% 1. get the CurrentClient from the Clients Dictionary
  %%% we dont match for error because if this fails we have a problem anyway
  %%% and want the process to throw an exception for now
  [Head | Tail] = RemainingClientsList,
  {ok, CurrentClient} = orddict:find(Head, Clients),

  %%% 1. set the PreviousClient as the left_neighbor of the current client
  %%% 2. set the head of the RemainingClientsList as the right_neighbor
  %%% of the current client
  UpdatedClient = CurrentClient#gcd_client{left_neighbor=PreviousClient#gcd_client.name},

  %%% 3. set UpdatedClient as the right_neighbor of the PreviousClient
  FinishedPreviousClient = PreviousClient#gcd_client{right_neighbor=UpdatedClient#gcd_client.name},

  %%% 4. update the Client Dictionary with the UpdatedClient and
  %%% FinishedPreviousClient
  UpdatedClients = update_clients_with_client(Clients,
                                              UpdatedClient#gcd_client.name,
                                              UpdatedClient),
  UpdatedClients2 = update_clients_with_client(UpdatedClients,
                                               FinishedPreviousClient#gcd_client.name,
                                               FinishedPreviousClient),
  %%% 3. set the RemainingClientsList to the tail of RemainingClientsList
  build_ring_of_gcd_clients(UpdatedClients2,
                            Pivot,
                            Tail,
                            UpdatedClient).

%%% this function iterates over all clients registered with the coordinator
%%% and sends them the message to set their neighbors
introduce_clients_to_their_neighbors(State) ->
  log("Introducing GCD clients to their neighbors"),
  orddict:map(
    fun(Key, Value) ->
        log(format("set GCD client ~p: left neighbor: ~p, right neighbor: ~p",
                   [Key,
                    Value#gcd_client.left_neighbor,
                    Value#gcd_client.right_neighbor])),
      send_message_to_service(State, Key, {setneighbors,
                                           Value#gcd_client.left_neighbor,
                                           Value#gcd_client.right_neighbor})
    end,
    get_clients(State)).

%%% sets a product of the gcd value in all the gcd_clients.
set_calculation_seed_in_gcd_clients(State, GCD) ->
  ClientsNamesList = orddict:fetch_keys(get_clients(State)),

  lists:map(
    fun(ClientName) ->
      {GCD, ProductOfGCD} = calculate_gcd_seed(GCD),
      log(format("Der ggT-Prozess ~p initiales Mi ~p", [ClientName, ProductOfGCD])),
      send_message_to_service(State, ClientName, {setpm, ProductOfGCD})
    end,
    ClientsNamesList),
  ok.

%%% sets the calculation seed value in 15% of the gcd_clients. these 15%
%%% are randomly chosen from all clients registered with the coordinator.
start_calculation_for_a_few_gcd_clients(State, GCD) ->
  ClientsNamesList = orddict:fetch_keys(get_clients(State)),
  %%% select 15% of the clients but at least 2 clients
  SelectedClientNames = select_percentage_of_elements_from_list(ClientsNamesList, 15),

  lists:map(
    fun(ClientName) ->
      %%% send the seed for the gcd calculation to each selected gcd_client
      {GCD, ProductOfGCD} = calculate_gcd_seed(GCD),

      send_message_to_service(State, ClientName, {sendy, ProductOfGCD})
    end,
    SelectedClientNames),
  ok.

kill_all_gcd_clients(State) ->
  ClientsNamesList = orddict:fetch_keys(get_clients(State)),

  lists:map(
    fun(ClientName) ->
      log(format("Sending the kill command to GCD-process ~p", [ClientName])),
      send_message_to_service(State, ClientName, kill)
    end,
    ClientsNamesList).


select_percentage_of_elements_from_list(List, Percentage) ->
  RemainingElementsToSelect = case round((length(List)/100) * Percentage) < 2 of
    true -> 2;

    _ -> round((length(List)/100) * Percentage)
  end,

  select_percentage_of_elements_from_list(List, RemainingElementsToSelect, []).

select_percentage_of_elements_from_list(_List, 0, Accu) ->
  Accu;

select_percentage_of_elements_from_list(List, RemainingElementsToSelect, Accu) ->
  [Head | Tail] = shuffle(List),
  select_percentage_of_elements_from_list(Tail,
                                          RemainingElementsToSelect - 1,
                                          [Head | Accu]).

start_gcd_process(State, GCD) ->
  set_calculation_seed_in_gcd_clients(State, GCD),
  start_calculation_for_a_few_gcd_clients(State, GCD).

%%% randomly shuffle a List with the Fisher-Yates Shuffle
%%% taken from: http://en.literateprograms.org/Fisher-Yates_shuffle_(Erlang)
shuffle(List) -> shuffle(List, []).
shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
    {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
    shuffle(Leading ++ T, [H | Acc]).

%%% returns {RequestedGCD, Product} where Product is the product of RequestedGCD
%%% and the set of primes {3, 5, 11, 13, 23, 37} randomly raised to the power
%%% of {0, 1, 2}.
%%% Example Return Value: {65, 65 * 3^2 * 5^1 * 11^0 * 13^2 * 23^1 * 37^1}
calculate_gcd_seed() ->
  %%% randomly pick a GCD between 1..100
  RequestedGCD = random:uniform(100),
  log(format("Der neue gesuchte GGT ist ~p", [RequestedGCD])),
  calculate_gcd_seed(RequestedGCD).

calculate_gcd_seed(RequestedGCD) ->
  {RequestedGCD,
   RequestedGCD * round(
      math:pow(3, random:uniform(3) - 1) *
      math:pow(5, random:uniform(3) - 1) *
      math:pow(11, random:uniform(3) - 1) *
      math:pow(13, random:uniform(3) - 1) *
      math:pow(23, random:uniform(3) - 1) *
      math:pow(37, random:uniform(3) - 1)
      )
  }.

%%% Log function for all coordinator logs
log(Message)->
  CoordinatorName= lists:concat(["Koordinator@",net_adm:localhost()]),
  LogMessage = lists:concat([CoordinatorName,
                             werkzeug:timeMilliSecond(),
                             " ",
                             Message,
                             io_lib:nl()]),
  werkzeug:logging(lists:concat([CoordinatorName,".log"]), LogMessage).

log_error(ErrorMessage) ->
  Message = lists:concat(["##### ","Error: ", ErrorMessage, " #####"]),
  log(Message).

format(String, ArgumentsList) ->
  io_lib:format(String, ArgumentsList).

%%%
%%% Helpers for message sending
%%%
nameservice_lookup(NameService, ServiceName) ->
  log(format("Suche beim Namensdienst nach Dienst: ~p", [ServiceName])),

  NameService ! {self(), {lookup, ServiceName}},

  receive
    not_found ->
      log_error(format("Fehlgeschlagene Suche beim Namensdienst nach Dienst: ~p", [ServiceName])),
      not_found;
    ServiceAddress = {ServiceName, ServiceNode} when
      is_atom(ServiceName) and is_atom(ServiceNode) ->
      {ok, ServiceAddress};

    %%% we do not want to get stuck due to an unexpected message
    _Unknown -> error
  end.

%%% ping the nameservice in order to introduce our nodes to each other
ping_name_service(NameServiceNode) ->
  case net_adm:ping(NameServiceNode) of
    pong ->
      global:sync(),
      {ok, global:whereis_name(nameservice)};

    _ ->
      log_error("Cannot find NameService"),
      error
  end.

send_message_to_coordinator(Message) ->
  State = read_config_into_state(#state{}),
  CoordinatorName = get_coordinator_name(State),
  send_message_to_service(State, CoordinatorName, Message).

send_message_to_service(State, ServiceName, Message) ->
  NameServiceNode = get_nameservice_node_name(State),

  %%% ping NameServiceNode
  case ping_name_service(NameServiceNode) of
    {ok, NameService} ->
      %%% lookup the name and node of the current coordinator in charge
      case nameservice_lookup(NameService, ServiceName) of
        not_found ->
          log_error(format("Service: ~p not found at nameservice", [ServiceName])),
          not_found;

        %%% everything is good. send message.
        {ok, ServicePid} -> ServicePid ! Message
      end;

    _ ->
      log_error("NameService was not found in send_message_to_service function"),
      error
  end.

