-module(dijkstra).
-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).

entry(Node, Sorted) ->
    Entry = lists:keysearch(Node, 1, Sorted),
    case Entry of
	{value, {Node, Length, _Gateway}} ->
	    Length;
	false ->
	    0
    end.

replace(Node, N, Gateway, Sorted) ->
    Entry = lists:keysearch(Node, 1, Sorted),
    case Entry of
	{value, {Node, _OldLength, _OldGateway}} ->
	    NewList = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway}),
	    lists:keysort(2, NewList);
	false ->
	    Sorted
    end .

update(Node, N, Gateway, Sorted) ->
    Length = entry(Node, Sorted),
    if N < Length ->
	    replace(Node, N, Gateway, Sorted);
       true ->
	    Sorted
    end.
	
iterate([], _Map, Table) ->
    Table;
iterate([{_Node, inf, _Gateway}|_T], _Map, Table) ->
    Table;
iterate([{Node, Length, Gateway}|T], Map, Table) ->
    Reachable = map:reachable(Node, Map),
    NewList = lists:foldl(fun(N, Sorted) -> update(N, Length + 1, Gateway, Sorted) end, T, Reachable),
    iterate(NewList, Map, [{Node, Gateway}|Table]).

table(Gateways, Map) ->
    AllNodes = map:all_nodes(Map),
    InfList = lists:map(fun(Node) -> {Node, inf, unknown} end, AllNodes),
    SortedList = lists:foldl(fun(Node, L) -> update(Node, 0, Node, L) end, InfList, Gateways),
    iterate(SortedList, Map, []).

route(Node, Table) ->
    Entry = lists:keysearch(Node, 1, Table),
    case Entry of
	{value, {Node, Gateway}} ->
	    {ok, Gateway};
	false ->
	    notfound
    end.
