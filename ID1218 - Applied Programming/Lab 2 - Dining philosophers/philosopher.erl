-module(philosopher).
-export([start/5]).

start(Hungry, Right, Left, Name, Ctrl) ->
    spawn_link(fun() -> init(Hungry, Right, Left, Name, Ctrl) end).

init(Hungry, Right, Left, Name, Ctrl) ->
    random:seed(now()),
    dreaming(Hungry, Right, Left, Name, Ctrl).

dreaming(Hungry, Right, Left, Name, Ctrl) ->
    Time = 500 + random:uniform(500),
    receive
	{granted, From} ->
	    From ! returned
    after Time ->
	    Left ! {request, self()},
	    timer:sleep(Time),
	    Right ! {request, self()},
	    io:format("~s requested  chopsticks~n", [Name]),
	    waiting(Hungry, Right, Left, Name, Ctrl)
    end.

waiting(Hungry, Right, Left, Name, Ctrl) ->
    receive
	{granted, First} ->
	    io:format("~s received first chopstick~n", [Name]),
	    receive
		{granted, _} ->
		    io:format("~s received second chopstick~n", [Name]),
		    eating(Hungry, Right, Left, Name, Ctrl)
	    after (1000+ random:uniform(2000)) ->
		    io:format("~s giving up~n", [Name]),
		    io:format("~p~n", [First]),
		    if First == Right ->
			    Left ! abort;
		       true ->
			    Right ! abort
		    end,
		    First ! returned,
		    dreaming(Hungry, Right, Left, Name, Ctrl)
	    end
    end.


eating(Hungry, Right, Left, Name, Ctrl) ->
    io:format("~s started eating~n", [Name]),
    Time = 500+random:uniform(500),
    timer:sleep(Time),
    Left ! returned,
    Right ! returned,
    io:format("~s put back two chopsticks~n", [Name]),
    if 
	Hungry > 1 ->
	    dreaming(Hungry-1, Right, Left, Name, Ctrl);
	true ->
	    io:format("***~s are not hungry any more***~n", [Name]),
	    Ctrl ! done
    end.
