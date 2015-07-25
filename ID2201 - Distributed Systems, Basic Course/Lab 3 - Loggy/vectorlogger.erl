-module(vectorlogger).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Vector = lists:map(fun(N) -> {N, 0} end, Nodes),
    loop(Vector, []).

loop(Vector, Messages) ->
    receive
	{log, From, RemoteVector, Msg} ->
	    %% Add the message to the list of messages.
	    UpdatedMessages = [{log, From, RemoteVector, Msg}|Messages],

	    {From, CurrentCount} = lists:keyfind(From, 1, RemoteVector),

	    %%io:format("log: ~w - Found vector entry from ~w? ~n", [lists:keyfind(From, 1, Vector), From]),
	    UpdatedVector = lists:keyreplace(From, 1, Vector, {From, CurrentCount}),

	    %%Traverse the list to find what messages are safe to print
	    %%IsOk = check(UpdatedVector, RemoteVector), 

	    %%io:format("log: ~w - Local vector ~n", [UpdatedVector]),
	    %%io:format("log: ~w - Remote vector~n", [RemoteVector]),
	    %%io:format("log: ~w - IsOk? ~n", [IsOk]),

	    %% Find the safe messages, those with counter value less than the currently lowest.
	    SafeMessages = lists:filter(fun({log, _Node, V, _Msg}) -> check(UpdatedVector, V) == true end, UpdatedMessages),

	    %% Find the unsafe messages (all other).
	    UnsafeMessages = lists:filter(fun({log, _Node, V, _Msg}) -> check(UpdatedVector, V) == false end, UpdatedMessages),

	    %% Sort the messages by counter value, to make sure they are in the right order.
	    %%	    SortedMessages = lists:keysort(3, SafeMessages),

	    %% Print all safe messages.
	    lists:foreach(fun(M) -> log(M) end, SafeMessages),

	    %%log({log, From, RemoteVector, Msg}),
	    loop(UpdatedVector, UnsafeMessages);
	stop ->
	    ok
    end.

log({log, From, Time, Msg}) ->
    io:format("log: ~w ~w ~p~n", [From, Time, Msg]).

check(LocalVector, RemoteVector) ->
    Merged = lists:zipwith(fun({N, LocalValue}, {N, RemoteValue}) -> 
				   if LocalValue >= RemoteValue -> true; true -> false end end,
			   LocalVector, 
			   RemoteVector),
    false == lists:member(false, Merged).
