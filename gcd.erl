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
                , vote_timer
                , has_started_vote
                , number_retrieval_time
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

start(DelayTimeInS, TerminationTimeInS, ProcessNumber, GroupNumber, TeamNumber, NameServiceNode, CoordinatorName, StarterNumber) ->
  spawn(fun()-> init(DelayTimeInS, TerminationTimeInS, ProcessNumber, GroupNumber, TeamNumber, NameServiceNode, CoordinatorName, StarterNumber) end).

init(DelayTimeInS, TerminationTimeInS, ProcessNumber, GroupNumber, TeamNumber, NameServiceNode, CoordinatorName, StarterNumber) ->
  DelayTime = DelayTimeInS * 1000,
  TerminationTime = TerminationTimeInS * 1000,
  ClientName = client_name(GroupNumber, TeamNumber, ProcessNumber, StarterNumber),
  Log = fun (Msg) -> log(ClientName, Msg) end,

  Log(format("~s Start GCD client ~s with PID ~p on node ~p.", [werkzeug:timeMilliSecond(), ClientName, self(), node()])),

  register(ClientName, self()),

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
      VoteTimer = start_vote_timer(TerminationTime, ClientName),
      with_number(#state{ delay_time = DelayTime
                        , termination_time = TerminationTime
                        , client_name = ClientName
                        , name_service = NameService
                        , coordinator = Coordinator
                        , left_neighbor = LeftNeighbor
                        , right_neighbor = RightNeighbor
                        , number = Number
                        , vote_timer = VoteTimer
                        , has_started_vote = false
                        , number_retrieval_time = current_time_milliseconds()
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
      log(ClientName, format("Vote request from ~p.", [Initiator])),
      with_number(abstimmung(State, Initiator));

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
send_y(State = #state{number = Number, client_name = ClientName, coordinator = Coordinator, left_neighbor = LeftNeighbor, right_neighbor = RightNeighbor, delay_time = DelayTime}, Y) ->
  NewNumberSafe = case gcd(Number, Y, DelayTime) of
    NewNumber when NewNumber =/= Number ->
      Coordinator ! {briefmi, {ClientName, NewNumber, werkzeug:timeMilliSecond()}},
      send_number_to_neighbors(NewNumber, LeftNeighbor, RightNeighbor),
      NewNumber;

    _ ->
      Number
  end,

  restart_vote_timer(State#state{number = NewNumberSafe, number_retrieval_time = current_time_milliseconds(), has_started_vote = false}).

send_number_to_neighbors(Number, LeftNeighbor, RightNeighbor) ->
  lists:foreach(fun(Neighbor) -> Neighbor ! {sendy, Number} end, [LeftNeighbor, RightNeighbor]).

abstimmung(State = #state{termination_time = TerminationTime, number_retrieval_time = NumberRetrievalTime, client_name = ClientName, left_neighbor = LeftNeighbor, right_neighbor = RightNeighbor, coordinator = Coordinator, number = Number}, Initiator) ->
  case Initiator of
    _Initiator = ClientName ->
      log(ClientName, format("I started the vote (~p = ~p).", [Initiator, ClientName])),
      RightNeighbor ! {abstimmung, ClientName};

    _ ->
      log(ClientName, format("He started the vote (he ~p != me ~p).", [Initiator, ClientName])),

      % ist seit dem letzten Empfang einer Zahl mehr als **/2 (** halbe) Sekunden vergangen, dann leitet er die Anfrage an seinen rechten Nachbarn weiter (implizites Zustimmen)
      case current_time_milliseconds() - NumberRetrievalTime > TerminationTime / 2 of
        true ->
          log(ClientName, "Accept vote request"),

          case Initiator of
            % Erhält ein initiierende Prozess von seinem linken Nachbarn die Anfrage nach der Terminierung (abstimmung), meldet er die Terminierung dem Koordinator.
            _Initiator = LeftNeighbor ->
              log(ClientName, format("Vote request from left neighbor (~p) --> Send termination request to coordinator.", [LeftNeighbor])),
              Coordinator ! {briefterm, {ClientName, Number, werkzeug:timeMilliSecond()}};

            _ ->
              log(ClientName, format("Vote request from someone else. Pass request to right neighbor (~p).", [RightNeighbor])),
              RightNeighbor ! {abstimmung, ClientName}
          end;

        _ ->
          log(ClientName, "Ignore vote request")
      end
  end,

  State#state{has_started_vote = Initiator =:= ClientName}.

% {tellmi,From}: Sendet das aktuelle Mi an From. Kann z.B. vom Koordinator genutzt werden, um bei einem Berechnungsstillstand die Mi-Situation im Ring anzuzeigen.
tell_mi(State = #state{number = Number}, From) ->
  From ! Number,
  State.


gcd(Number, Y, DelayTime) ->
  timer:sleep(DelayTime),

  case Y < Number of
    true -> ((Number-1) rem Y) + 1;
    _    -> Number
  end.


start_vote_timer(TerminationTime, ClientName) ->
  log(ClientName, format("start timer with msg ~p. will fire in ~Bms.", [ClientName, TerminationTime])),
  {ok, Timer} = timer:send_after(TerminationTime, {abstimmung, ClientName}),
  Timer.

restart_vote_timer(State = #state{termination_time = TerminationTime, client_name = ClientName}) ->
  State#state{vote_timer = start_vote_timer(TerminationTime, ClientName)}.


current_time_milliseconds() ->
  {Mega, Sec, Micro} = now(),
  Mega*1000000000 + Sec*1000 + Micro/1000.


% Ein ggT-Prozess hat den Namen ?????, wobei ????? eine Zahl ist, die sich wie folgt zusammensetzt: 
%    <PraktikumsgruppenID><TeamID><Nummer des ggT-Prozess><Nummer des Starters>, 
% also z.B. ist in der Praktikumsgruppe 4 von dem Team 03 ein zweiter ggT-Prozess von ihrem ersten Starter gestartet worden, so erhält dieser ggT-Prozess den Namen 4321. In der Kommunikation wird nur dieser Name verwendet!  
client_name(GroupNumber, TeamNumber, ProcessNumber, StarterNumber) ->
  list_to_atom(format("~B~B~B~B", [GroupNumber, TeamNumber, ProcessNumber, StarterNumber])).

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
  werkzeug:logging(format("GGTP_~s@~s.log", [atom_to_list(ClientName), net_adm:localhost()]), format("~s~n", [Msg])).

format(String, Arguments) ->
  lists:flatten(io_lib:format(String, Arguments)).
