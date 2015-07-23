-module(philosopher).
-export([start/6]).

start(Hungry, Right, Left, Name, Ctrl, Waiter) ->
    spawn_link(fun() -> dreaming(Hungry, Right, Left, Name, Ctrl, Waiter) end).

dreaming(Hungry, Right, Left, Name, Ctrl, Waiter) ->
    Time = 500+random:uniform(500),
    timer:sleep(Time),
    Waiter ! {mayieat, self()},
    receive 
	yes ->
	    Left ! {request, self()},
	    timer:sleep(Time),
	    Right ! {request, self()},
	    waiting(Hungry, Right, Left, Name, Ctrl, Waiter)
    end.
    

waiting(Hungry, Right, Left, Name, Ctrl, Waiter) ->
    receive
	{granted, _} ->
	    io:format("~s received first chopstick~n", [Name]),
	    receive
		{granted, _} ->
		    io:format("~s received second chopstick~n", [Name]),
		    eating(Hungry, Right, Left, Name, Ctrl, Waiter)
	    end
    end.


eating(Hungry, Right, Left, Name, Ctrl, Waiter) ->
    io:format("~s started eating~n", [Name]),
    Time = 500+random:uniform(500),
    timer:sleep(Time),
    Left ! returned,
    Right ! returned,
    Waiter ! done,
    io:format("~s put back two chopsticks~n", [Name]),
    if 
	Hungry > 1 ->
	    dreaming(Hungry-1, Right, Left, Name, Ctrl, Waiter);
	true ->
	    Ctrl ! done
    end.
