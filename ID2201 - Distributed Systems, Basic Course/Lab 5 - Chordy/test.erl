-module(test).
-export([test/0, test/1]).
-define(Stabilize, 100).
-define(Timeout, 10000).

test() ->
    Pid = node2:start(1),
    %%io:format("Pid ~w~n", [Pid]),
    %%node2:start(250000000, Pid),
    %%node2:start(500000000, Pid),
    %%node2:start(750000000, Pid),
    %%register(node, Pid),
    test(Pid).

test(Node) ->    
    timer:sleep(1000),
    Keys = add(Node, [], 10000),
    Start = now(),
    lookup(Node, Keys),
    End = now(),
    io:format("Lookup time: ~w~n", [timer:now_diff(End, Start)]).

add(_Pid, Keys, 0) ->
    Keys;
add(Pid, Keys, N) ->
    Key = key:generate(),
    Value = key:generate(),
    Qref = make_ref(),
    Pid ! {add, Key, Value, Qref, self()},
    receive
	{Qref, ok} ->
	    add(Pid, [Key|Keys], N - 1)
    end.

lookup(_Pid, []) ->
    ok;
lookup(Pid, Keys) ->
    [Key|Rest] = Keys,
    Qref = make_ref(),
    Pid ! {lookup, Key, Qref, self()},
    receive
	{Qref, Result} ->
	    %%io:format("Lookup key ~w: ~w~n", [Key, Result]),
	    lookup(Pid, Rest)
    end.

 
