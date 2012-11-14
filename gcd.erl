-module(gcd).
-compile([export_all]).
% DelayTime, TerminationTime, ClientName, NameService, Coordinator, LeftNeighbor, RightNeighbor, Number
-record (state, { delay_time
                , termination_time
                , client_name
                , name_service
                , coordinator
                , left_neighbor
                , right_neighbor
                , number
                }).

% die Verzögerungszeit
% die Terminierungszeit
% der Startnummer dieses Prozesses (also der wievielte gestartete ggT-Prozess er ist)
% seine eindeutige Starternummer
% die Praktikumsgruppennummer
% die Teamnummer 
% die benötigten Kontaktdaten für den Namensdienst
%                             und den Koordinator
% Der ggT-Prozess meldet sich beim Koordinator mit seinem Namen an (hello) und beim Namensdienst (rebind). Er registriert sich ebenfalls lokal auf der Erlang-Node mit seinem Namen (register). Der ggT-Prozess erwartet dann vom Koordinator die Informationen über seine Nachbarn (setneighbors).
start(DelayTime, TerminationTime, ProcessNumber, StarterNumber, GroupNumber, TeamNumber, NameServiceNode, CoordinatorName) ->
  ClientName = client_name(GroupNumber, TeamNumber, ProcessNumber, StarterNumber),
  Log = fun (Msg) -> log(ClientName, Msg) end,

  Log(format("~s Start GCD client ~s with PID ~p on node ~p.", [werkzeug:timeMilliSecond(), ClientName, self(), node()])),

  register(list_to_atom(ClientName), self()),

  case name_service(NameServiceNode) of
    {ok, NameService} ->
      NameService ! {self(), {rebind, ClientName, node()}},
      Log(format("Registered with name service on node ~p.", [NameServiceNode])),

      receive
        ok ->
          Log(format("Resolve coordinator from name ~s.", [CoordinatorName])),

          case coordinator(NameService, CoordinatorName) of
            {ok, Coordinator} ->
              Log(format("Resolved coordinator to ~p.", [Coordinator])),

              Coordinator ! {hello, ClientName},
              Log("Contacted coordinator."),
              wait_for_neighbors(DelayTime, TerminationTime, ClientName, NameService, Coordinator);

            _ ->
              Log("Could not resolve coordinator"),
              error
          end;

        Unknown ->
          Log(format("start: Unknown message ~p.", [Unknown])),
          error
      end;

    error -> 
      Log(format("Could not find name service on node ~p.", [NameServiceNode])),
      error
  end.

% {setneighbors,LeftN,RightN}: die (lokal auf deren Node registrieten und im Namensdienst registrierten) Namen des linken und rechten Nachbarn werden gesetzt.
wait_for_neighbors(DelayTime, TerminationTime, ClientName, NameService, Coordinator) ->
  log(ClientName, "Waiting for neighbors..."),

  receive
    {setneighbors, LeftNeighbor, RightNeighbor} ->
      log(ClientName, format("Set neighbors to ~p (left) and ~p (right).", [LeftNeighbor, RightNeighbor])),
      wait_for_number(DelayTime, TerminationTime, ClientName, NameService, Coordinator, LeftNeighbor, RightNeighbor);

    kill ->
      log(ClientName, "wait_for_neighbors: kill."),
      ok;

    Unknown ->
      log(ClientName, format("wait_for_neighbors: Unknown message ~p.", [Unknown])),
      wait_for_neighbors(DelayTime, TerminationTime, ClientName, NameService, Coordinator)
  end.

% {setpm,MiNeu}: die von diesem Prozess zu berabeitenden Zahl für eine neue Berechnung wird gesetzt.
wait_for_number(DelayTime, TerminationTime, ClientName, NameService, Coordinator, LeftNeighbor, RightNeighbor) ->
  log(ClientName, "Waiting for number..."),
  
  receive
    {setpm, Number} ->
      log(ClientName, format("wait_for_number: Set Number to ~B.", [Number])),
      with_number(#state{ delay_time = DelayTime
                        , termination_time = TerminationTime
                        , client_name = ClientName
                        , name_service = NameService
                        , coordinator = Coordinator
                        , left_neighbor = LeftNeighbor
                        , right_neighbor = RightNeighbor
                        , number = Number
                        });

    kill ->
      log(ClientName, "wait_for_number: kill."),
      ok;

    Unknown ->
      log(ClientName, format("wait_for_number: Unknown message ~p.", [Unknown])),
      wait_for_number(DelayTime, TerminationTime, ClientName, NameService, Coordinator, LeftNeighbor, RightNeighbor)
  end.

with_number(State = #state{client_name = ClientName, number = Number}) ->
  log(ClientName, "Waiting for incomming messages..."),
  
  receive
    {sendy, Y} ->
      log(ClientName, format("New Y: ~B.", [Y])),
      with_number(send_y(State, Y));

    % {abstimmung,Initiator}: Wahlnachricht für die Terminierung der aktuellen Berechnung; Initiator ist der Initiator dieser Wahl (z.B. Name des ggT-Prozesses).
    {abstimmung, Initiator} ->
      log(ClientName, format("Vote from ~p.", [Initiator])),
      % TODO
      with_number(State);

    {tellmi, From} ->
      log(ClientName, format("Tell ~p the number (~B).", [From, Number])),
      with_number(tell_mi(State, From));

    % kill: der ggT-Prozess wird beendet.
    kill ->
      log(ClientName, "with_number: kill."),
      ok;

    Unknown ->
      log(ClientName, format("with_number: Unknown message ~p.", [Unknown])),
      with_number(State)
  end.


% {sendy,Y}: der rekursive Aufruf der ggT Berechnung.
send_y(State = #state{number = Number, client_name = ClientName, coordinator = Coordinator, left_neighbor = LeftNeighbor, right_neighbor = RightNeighbor}, Y) ->
  NewNumberSafe = case gcd(Number, Y) of
    NewNumber when NewNumber =/= Number ->
      Coordinator ! {briefmi, {ClientName, NewNumber, werkzeug:timeMilliSecond()}},
      send_number_to_neighbors(NewNumber, LeftNeighbor, RightNeighbor),
      NewNumber;

    _ ->
      Number
  end,

  State#state{number = NewNumberSafe}.

send_number_to_neighbors(Number, LeftNeighbor, RightNeighbor) ->
  lists:foreach(fun(Neighbor) -> Neighbor ! {sendy, Number} end, [LeftNeighbor, RightNeighbor]).

% {tellmi,From}: Sendet das aktuelle Mi an From. Kann z.B. vom Koordinator genutzt werden, um bei einem Berechnungsstillstand die Mi-Situation im Ring anzuzeigen.
tell_mi(State = #state{number = Number}, From) ->
  From ! Number,
  State.


gcd(Number, Y) ->
  case Y < Number of
    true -> ((Number-1) rem Y) + 1;
    _    -> Number
  end.



% Ein ggT-Prozess hat den Namen ?????, wobei ????? eine Zahl ist, die sich wie folgt zusammensetzt: 
%    <PraktikumsgruppenID><TeamID><Nummer des ggT-Prozess><Nummer des Starters>, 
% also z.B. ist in der Praktikumsgruppe 4 von dem Team 03 ein zweiter ggT-Prozess von ihrem ersten Starter gestartet worden, so erhält dieser ggT-Prozess den Namen 4321. In der Kommunikation wird nur dieser Name verwendet!  
client_name(GroupNumber, TeamNumber, ProcessNumber, StarterNumber) ->
  format("~B~B~B~B", [GroupNumber, TeamNumber, ProcessNumber, StarterNumber]).

name_service(NameServiceNode) ->
  case net_adm:ping(NameServiceNode) of
    pong ->
      global:sync(),
      {ok, global:whereis_name(nameservice)};

    _ ->
      error
  end.

coordinator(NameService, CoordinatorName) ->
  NameService ! {self(), {lookup, CoordinatorName}},
  receive
    not_found ->
      error;

    Coordinator -> 
      {ok, Coordinator}
  end.

log(ClientName, Msg) ->
  {ok, HostName} = inet:gethostname(),
  werkzeug:logging(format("GGTP_~s@~s.log", [ClientName, HostName]), format("~s~n", [Msg])).

format(String, Arguments) ->
  lists:flatten(io_lib:format(String, Arguments)).
