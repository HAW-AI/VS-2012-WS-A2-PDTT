VS-2012-WS-A2-PDTT
==================
Verteilte Systeme, Wintersemester 2012, Aufgabe 2

Start the system by pasting the following code into your REPL:

    c(gcd).
    nameservice:start().
    timer:sleep(100).
    Coord = coordinator:start().
    timer:sleep(100).
    starter:start(5).
    timer:sleep(100).
    Coord ! get_ready.
    timer:sleep(100).
    Coord ! start_distributed_gcd_calculation.