-module(starter).

-compile([export_all]).
-record(state, {
    lab_group,
    team_number,
    nameservicenode,
    coordinatorname
    }).

start() ->
  start(1).

start(NumberOfStarters) when is_integer(NumberOfStarters) andalso NumberOfStarters > 0 ->
  lists:map(
    fun(Num) ->
        spawn(fun() -> initialize(Num) end)
    end,
    lists:seq(1, NumberOfStarters)).

initialize(StarterNumber) ->
  State = read_config_into_state(),
  %%% create one log per gcd client started
  file:open(format("gcd-~B@~s.log", [StarterNumber, net_adm:localhost()]), [append]),
  case ping_name_service(get_nameservicenode(State), StarterNumber) of
    {ok, NameService} ->
      NameService ! {self(), {lookup, get_coordinator_name(State)}};

    _ ->
      error
  end,
  receive
    not_found ->
      log_error("Could not find coordinator", StarterNumber);

    Coordinator = {ServiceName, ServiceNode} when
      is_atom(ServiceName) andalso is_atom(ServiceNode) ->
      Coordinator ! {getsteeringval, self()},
      handle_steeringval(State, StarterNumber)
  end.

handle_steeringval(State, StarterNumber) ->
  receive
    {steeringval, ProcessingTime, TerminationTime, GCDProcessNumber} ->
      log("Received Steering Values from Coordinator", StarterNumber),
      log(format("Starting ~p GCD processes with a working delay of ~p and a lifetime of ~p",
                 [GCDProcessNumber, ProcessingTime, TerminationTime]), StarterNumber),
      lists:map(
        fun(Num) ->
          gcd:start(ProcessingTime,
                    TerminationTime,
                    Num,
                    get_lab_group(State),
                    get_team_number(State),
                    get_nameservicenode(State),
                    get_coordinator_name(State),
                    StarterNumber)
        end,
        lists:seq(1, GCDProcessNumber)),
      log("Starter finished starting GCD client and will terminate now.", StarterNumber),
      exit(self());

    _Unknown ->
      %%% got an unknwon command
      error
  end.

%%% read config from file into state and return new state
read_config_into_state() ->
  {ok, Config} = read_config_from_file(),
  #state{
    lab_group = proplists:get_value(praktikumsgruppe, Config),
    team_number = proplists:get_value(teamnummer, Config),
    nameservicenode = proplists:get_value(nameservicenode, Config),
    coordinatorname = proplists:get_value(koordinatorname, Config)
  }.

read_config_from_file() ->
  file:consult("gcd.cfg").

%%% get values from config within state
get_lab_group(State) ->
  State#state.lab_group.
get_team_number(State) ->
  State#state.team_number.
get_nameservicenode(State) ->
  State#state.nameservicenode.
get_coordinator_name(State) ->
  State#state.coordinatorname.

format(String, ArgumentsList) ->
  io_lib:format(String, ArgumentsList).

%%% Log function for all coordinator logs
log(Message, StarterNumber)->
  StarterName= lists:concat(["Starter@",net_adm:localhost()]),
  LogMessage = lists:concat([StarterName,
                             werkzeug:timeMilliSecond(),
                             " ",
                             Message,
                             io_lib:nl()]),

  werkzeug:logging(format("gcd-~B@~s.log", [StarterNumber, net_adm:localhost()]), LogMessage).

log_error(ErrorMessage, StarterNumber) ->
  Message = lists:concat(["##### ","Error: ", ErrorMessage, " #####"]),
  log(Message, StarterNumber).

%%% ping the nameservice in order to introduce our nodes to each other
ping_name_service(NameServiceNode, StarterNumber) ->
  case net_adm:ping(NameServiceNode) of
    pong ->
      global:sync(),
      {ok, global:whereis_name(nameservice)};

    _ ->
      log_error("Cannot find NameService", StarterNumber),
      error
  end.
