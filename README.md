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