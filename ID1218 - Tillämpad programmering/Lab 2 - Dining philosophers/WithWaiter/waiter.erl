-module(waiter).
-export([start/0]).

start() ->
    spawn_link(fun() -> wait(free) end).

wait(free) ->
    receive 
	{mayieat, From} ->
	    From ! yes,
	    wait(1)
    end;
wait(1) ->
    receive
	{mayieat, From} ->
	    From ! yes,
	    wait(2);
	done ->
	    wait(free)
    end;
wait(2) ->
    receive
	{mayieat, From} ->
	    From ! yes,
	    wait(3);
	done ->
	    wait(1)
    end;
wait(3) ->
    receive
	{mayieat, From} ->
	    From ! yes,
	    wait(4);
	done ->
	    wait(2)
    end;
wait(4) ->
    receive
	done ->
	    wait(1)
    end.
