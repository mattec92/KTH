-module(dinner).
-export([start/0]).

start() ->
    spawn(fun() -> init() end).
init() ->
    C1 = chopstick:start(),
    C2 = chopstick:start(),
    C3 = chopstick:start(),
    C4 = chopstick:start(),
    C5 = chopstick:start(),
    Ctrl = self(),
    philosopher:start(5, C1, C2, "Confucios", Ctrl),
    philosopher:start(5, C2, C3, "Avicenna", Ctrl),
    philosopher:start(5, C3, C4, "Plato", Ctrl),
    philosopher:start(5, C4, C5, "Kant", Ctrl),
    philosopher:start(5, C5, C1, "Descartes", Ctrl),
    wait(5, [C1, C2, C3, C4, C5]).

wait(0, Chopsticks) ->
    lists:foreach(fun(C) -> C ! quit end, Chopsticks);
wait(N, Chopsticks) ->
    receive
	done ->
	    wait(N-1, Chopsticks);
	abort ->
	    erlang:exit(abort)
    end.
