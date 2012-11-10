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
  %%% read config from file into State
  State = read_config_into_state(#state{}),

  %%% register coordinator name locally
  register(get_coordinator_name(State), self()),

  %%% ping NameServiceNode
  case ping_name_service(get_nameservice_node_name(State)) of
    {ok, NameService} ->
      %%% first time bind of our service with the nameservice
      NameService ! {self(), {bind, get_coordinator_name(State), node()}},

      receive
        ok ->
          io:format("..bind.done.\n"),

          %%% done with the start phase. everything worked. go into initial state.
          initial(State);

        in_use ->
          %%% something went wrong. the coordinator is already bound at the Nameservice.
          io:format("..schon gebunden.\n")
      end;

    error ->
      %%% TODO log the error
      error

  end.


stop(State) ->
  %%% get Nameservice PID
  NameService = global:whereis_name(nameservice),

  %%% unbind name with Nameservice
  NameService ! {self(), {unbind, get_coordinator_name(State)}},

  receive 
    ok -> io:format("..unbind..done.\n")
  end,

  ok.


%%% initial state the coordinator goes into after the start phase
initial(State) ->
  receive
    % Die Anfrage nach den steuernden Werten durch den Starter Prozess.
    {getsteeringval, StarterPID} ->
      StarterPID ! {steeringval,
                    get_processing_time(State),
                    get_termination_time(State),
                    get_gcd_process_number(State)},

      initial(State);

    % Ein ggT-Prozess meldet sich beim Koordinator mit Namen Clientname an (Name ist der lokal registrierte Name!).
    {hello, ClientName} ->
      initial(register_gcd_client(State, ClientName));

    %%% once somebody triggers the calculation of the distributed gcd
    %%% the coordinator starts building "The Ring"
    start_distributed_gcd_calculation ->
      build_ring(State)

  end,

  ok.

build_ring(State) ->
  %%% TODO log that the ring is being built
  %%% arrange the gcd clients in a ring and go into ready state
  StateWithRing = State#state{clients = build_ring_of_gcd_clients(get_clients(State))},

  %%% this sends the {setneighbors, LeftN, RightN} message to each client
  introduce_clients_to_their_neighbors(StateWithRing),

  ready(StateWithRing).

ready(State) ->
  inform_gcd_clients_,
  ok.

%%% call this module function and it will start the coordiator that is
%%% registered with the name service, given that coordinator is in the
%%% initial state.
start_distributed_gcd_calculation() ->
  {ok, Config} = file:consult("server.cfg"),
  CoordinatorName = proplists:get_value(koordinatorname, Config),
  NameServiceNode = proplists:get_value(nameservicenode, Config),


  %%% ping NameServiceNode
  case ping_name_service(NameServiceNode) of
    {ok, NameService} ->
      %%% lookup the name and node of the current coordinator in charge
      NameService ! {self(), {lookup, CoordinatorName}},

      receive
        not_found -> io:format("..meindienst..not_found.\n");

        {Name, Node} ->
          io:format("...ok: {~p,~p}.\n",[Name,Node]),

          %%% the message should only be received by a coordiator in the
          %%% initial state
          {Name, Node} ! start_distributed_gcd_calculation
      end;

    _ ->
      %%% TODO log the error
      error
  end.

terminating() ->
  ok.

%%% read config from file into state and return new state
read_config_into_state(State) ->
  {ok, Config} = file:consult("server.cfg"),
  State#state{config = Config}.

%%% useful getter functions

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

%%% ping the nameservice in order to introduce our nodes to each other
ping_name_service(NameServiceNode) ->
  case net_adm:ping(NameServiceNode) of
    pong ->
      global:sync(),
      {ok, global:whereis_name(nameservice)};

    _ ->
      error
  end.

%%% build a ring of the registered gcd clients where each gcd client
%%% knows his left and right neighbor.
%%% Pivot: first ClientName from which we start building the ring
%%% Clients: State#state.clients Dictionary with ClientName -> gcd_client entries
%%%
%%% Returns: an updated Clients Dictionary
build_ring_of_gcd_clients(Clients) ->
  %%% it is not possible/ill-adviced to build a ring with only one client.
  %%% that client would have himself as his left and right neighbor and would
  %%% send himself 2 messages.
  %%% TODO decide if we should increment this to < 3 to have distinct neighbors
  case length(Clients) < 2 of
    true -> error;
    false ->
      ClientsList = orddict:fetch_keys(Clients),
      Pivot = lists:head(Clients),
      build_ring_of_gcd_clients(Clients,
                                Pivot,
                                lists:tail(ClientsList),
                                none)
  end.

build_ring_of_gcd_clients(Clients, Pivot, RemainingClientsList, none) ->
  %%% initial call:
  build_ring_of_gcd_clients(Clients,
                            Pivot,
                            lists:tail(RemainingClientsList),
                            Pivot);

build_ring_of_gcd_clients(Clients, Pivot, [], PreviousClient) ->
  %%% empty RemainingClientsList:
  %%% all clients have been updated. all that is missing is the left neighbor
  %%% of the Pivot element and the right_neighbor of the PreviousClient
  FinishedPivot = Pivot#gcd_client{left_neighbor=PreviousClient#gcd_client.name},

  %%% 3. set FinishedPivot as the right_neighbor of the PreviousClient
  FinishedPreviousClient = PreviousClient#gcd_client{right_neighbor=FinishedPivot#gcd_client.name},

  %%% return the updated Clients Dictionary with the ring
  UpdatedClients = update_clients_with_client(Clients,
                                              FinishedPivot#gcd_client.name,
                                              FinishedPivot),
  update_clients_with_client(UpdatedClients,
                             FinishedPreviousClient#gcd_client.name,
                             FinishedPreviousClient);

build_ring_of_gcd_clients(Clients, Pivot, RemainingClientsList, PreviousClient) ->
  %%% there are remaining clients. recursively traverse the RemainingClientsList:
  %%% 1. get the CurrentClient from the Clients Dictionary
  %%% we dont match for error because if this fails we have a problem anyway
  %%% and want the process to throw an exception for now
  {ok, CurrentClient} = orddict:find(lists:head(RemainingClientsList), Clients),
 
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
                            lists:tail(RemainingClientsList),
                            UpdatedClient).

nameservice_lookup(ServiceName) ->
  %%% get Nameservice PID
  NameService = global:whereis_name(nameservice),

  NameService ! {self(), {lookup, ServiceName}},

  receive
    not_found -> not_found;
    ServicePID -> ServicePID
  end.

introduce_clients_to_their_neighbors(State) ->
  orddict:map(
    fun(Key, Value) ->
      case nameservice_lookup(Key) of
        %%% TODO throw some exception if not_found
        not_found -> error;

        ServicePID ->
          ServicePID ! {setneighbors,
                        Value#gcd_client.left_neighbor,
                        Value#gcd_client.right_neighbor},
          ok
      end
    end,
    get_clients(State)).





