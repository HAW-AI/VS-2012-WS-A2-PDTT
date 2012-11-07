-module(gcd).
-compile([export_all]).

% Der ggT-Prozess meldet sich beim Koordinator mit seinem Namen an (hello) und beim Namensdienst (rebind). Er registriert sich ebenfalls lokal auf der Erlang-Node mit seinem Namen (register). Der ggT-Prozess erwartet dann vom Koordinator die Informationen über seine Nachbarn (setneighbors).
start(DelayTime, TerminationTime, ProcessNumber, StarterNumber, GroupNumber, TeamNumber, NameService, Coordinator) ->
  ClientName = client_name(GroupNumber, TeamNumber, ProcessNumber, StarterNumber),
  register(ClientName, self()).
  % Coordinator ! {hello, }

% {setneighbors,LeftN,RightN}: die (lokal auf deren Node registrieten und im Namensdienst registrierten) Namen des linken und rechten Nachbarn werden gesetzt.
% {setpm,MiNeu}: die von diesem Prozess zu berabeitenden Zahl für eine neue Berechnung wird gesetzt.
% {sendy,Y}: der rekursive Aufruf der ggT Berechnung.
% {abstimmung,Initiator}: Wahlnachricht für die Terminierung der aktuellen Berechnung; Initiator ist der Initiator dieser Wahl (z.B. Name des ggT-Prozesses).
% {tellmi,From}: Sendet das aktuelle Mi an From. Kann z.B. vom Koordinator genutzt werden, um bei einem Berechnungsstillstand die Mi-Situation im Ring anzuzeigen.
% kill: der ggT-Prozess wird beendet.

% Ein ggT-Prozess hat den Namen ?????, wobei ????? eine Zahl ist, die sich wie folgt zusammensetzt: 
%    <PraktikumsgruppenID><TeamID><Nummer des ggT-Prozess><Nummer des Starters>, 
% also z.B. ist in der Praktikumsgruppe 4 von dem Team 03 ein zweiter ggT-Prozess von ihrem ersten Starter gestartet worden, so erhält dieser ggT-Prozess den Namen 4321. In der Kommunikation wird nur dieser Name verwendet!  
client_name(GroupNumber, TeamNumber, ProcessNumber, StarterNumber) ->
  lists:flatten(io_lib:format("~B~B~B~B", [GroupNumber, TeamNumber, ProcessNumber, StarterNumber])).
