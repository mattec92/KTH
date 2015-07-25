-module(logger).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    %% Initialize node list with 1 as the counter value (whats defined in the workers).
    NodeCounters = lists:foldl(fun(Node, List) -> [{Node, 1}|List] end, [], Nodes),
    loop(NodeCounters, [], 0).

loop(NodeCounters, Messages, MaxLength) ->
    receive
	{log, From, Time, Msg} ->
	    %% Add the message to the list of messages.
	    UpdatedMessages = [{log, From, Time, Msg}|Messages],

	    %% Update the node list.
	    UpdatedNodeCounters = lists:keyreplace(From, 1, NodeCounters, {From, Time}),

	    %% Find the lowest value of all the counters.
	    LowestCounter = lists:foldl(fun({_Node, T}, Acc) -> erlang:min(T, Acc) end, inf, UpdatedNodeCounters),

	    %% Find the safe messages, those with counter value less than the currently lowest.
	    SafeMessages = lists:filter(fun({log, _Node, T, _Msg}) -> T < LowestCounter end, UpdatedMessages),

	    %% Find the unsafe messages (all other).
	    UnsafeMessages = lists:filter(fun({log, _Node, T, _Msg}) -> T >= LowestCounter end, UpdatedMessages),

	    %% Sort the messages by counter value, to make sure they are in the right order.
	    SortedMessages = lists:keysort(3, SafeMessages),

	    %% Print all safe messages.
	    lists:foreach(fun(M) -> log(M) end, SortedMessages),

	    %%io:format("log: Max length ~w~n", [MaxLength]),

	    loop(UpdatedNodeCounters, UnsafeMessages, erlang:max(erlang:length(UnsafeMessages), MaxLength));
	stop ->
	    ok
    end.

log({log, From, Time, Msg}) ->
    io:format("log: ~w ~w ~p~n", [From, Time, Msg]).
