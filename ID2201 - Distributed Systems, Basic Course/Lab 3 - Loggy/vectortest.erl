-module(vectortest).
-export([run/2]).

run(Sleep, Jitter) ->
    Log = vectorlogger:start([john, paul, ringo, george]),
    A = vectorworker:start(john, Log, 13, Sleep, Jitter, [john, paul, ringo, george]),
    B = vectorworker:start(paul, Log, 23, Sleep, Jitter, [john, paul, ringo, george]),
    C = vectorworker:start(ringo, Log, 36, Sleep, Jitter, [john, paul, ringo, george]),
    D = vectorworker:start(george, Log, 49, Sleep, Jitter, [john, paul, ringo, george]),
    vectorworker:peers(A, [B, C, D]),
    vectorworker:peers(B, [A, C, D]),
    vectorworker:peers(C, [A, B, D]),
    vectorworker:peers(D, [A, B, C]),
    timer:sleep(5000),
    vectorlogger:stop(Log),
    vectorworker:stop(A),
    vectorworker:stop(B),
    vectorworker:stop(C),
    vectorworker:stop(D).
