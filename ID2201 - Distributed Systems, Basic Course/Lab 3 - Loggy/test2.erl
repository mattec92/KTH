-module(test2).
-export([run/2]).

run(Sleep, Jitter) ->
    Log = logger2:start([john, paul, ringo, george]),
    A = worker2:start(john, Log, 13, Sleep, Jitter),
    B = worker2:start(paul, Log, 23, Sleep, Jitter),
    C = worker2:start(ringo, Log, 36, Sleep, Jitter),
    D = worker2:start(george, Log, 49, Sleep, Jitter),
    worker2:peers(A, [B, C, D]),
    worker2:peers(B, [A, C, D]),
    worker2:peers(C, [A, B, D]),
    worker2:peers(D, [A, B, C]),
    timer:sleep(5000),
    logger2:stop(Log),
    worker2:stop(A),
    worker2:stop(B),
    worker2:stop(C),
    worker2:stop(D).
