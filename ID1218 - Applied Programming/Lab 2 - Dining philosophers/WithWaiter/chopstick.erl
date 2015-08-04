-module(chopstick).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).
init() ->
    available().

available() ->
    receive
	{request, From} ->
	    From ! {granted, self()},
	    gone();
	quit ->
	    ok
    end.

gone() ->
    receive
	returned ->
	    available();
	quit ->
	    ok
    end.
