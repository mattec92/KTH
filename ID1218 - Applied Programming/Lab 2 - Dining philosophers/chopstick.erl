-module(chopstick).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).
init() ->
    available().

available() ->
    receive
	{request, From} ->
	    send(From),
%	    From ! {granted, self()},
	    gone();
	quit ->
	    ok
    end.

send(To) ->
    receive
	{request, To} ->
	    send(To)
    after 0 ->
%	    io:format("Ger bort pinne~n"),
	    To ! {granted, self()}

    end.

gone() ->
    receive
	returned ->
%	    io:format("Får tillbaka pinne~n"),
	    available();
	abort ->
	    abort();
	quit ->
	    ok
    end.

abort() ->
    receive
	{request, _} ->
	    abort()
    after 0 ->
	    gone()
    end.
