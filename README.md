VS-2012-WS-A2-PDTT
==================
Verteilte Systeme, Wintersemester 2012, Aufgabe 2

Set nameservice nodes in coordinator.cfg and gcd.cfg.

Compile the source files:

    c(gcd).
    c(coordinator).
    c(starter).
    c(werkzeug).
    c(nameservice).

Start the system:

    nameservice:start().
    timer:sleep(100).
    Coord = coordinator:start().
    timer:sleep(100).
    starter:start(20).
    timer:sleep(100).
    Coord ! get_ready.
    timer:sleep(100).
    Coord ! start_distributed_gcd_calculation.

Note that `starter:start(N)` starts N starter which then take the amount of GCD processes to start from the steeringvalues the coordinator sends to the starter. So you end up with `N * ggtprozessnummer` GCD Clients.

start_distributed_gcd_calculation will pick a random start value
between 1-100 but you can also specify a gcd to search for with:

    Coord ! {start_distributed_gcd_calculation, 34}.

You can reset the coordinator and re-open the registration for gcd
clients with:

    Coord ! reset.

You can kill the coordinator which will kill all connected gcd clients
and then unbinds the coordinator name at the nameservice.
