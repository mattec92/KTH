-module(lamp).
-compile(export_all).

clock(Nodes) ->
	lists:map(fun(Node) -> {Node, 0} end, Nodes).

update(Node, T, Clock) ->
	lists:keyreplace(Node, 1, Clock, {Node, T}).

safe(_, inf) ->
	true;
safe(T, Clock) ->
	Lowest = lists:foldl(fun({_Node, Count}, Acc) -> erlang:min(Count, Acc) end, inf, Clock),
	if 
		T > Lowest ->
			false;
		true ->
			true
	end.

inf() ->
	inf.

leq(Ti, Tj) ->
	if
		Ti > Tj ->
			false;
		true ->
			true
	end.

new() ->
	0.

merge(Ti, Tj) ->
	erlang:max(Ti, Tj).

inc(T) ->
	T + 1.