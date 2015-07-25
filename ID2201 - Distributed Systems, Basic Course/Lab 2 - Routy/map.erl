-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    [].

update(Node, Links, Map) ->
    Result = lists:keysearch(Node, 1, Map),
    case Result of
	{value, {Node, _List}} ->
	    lists:keyreplace(Node, 1, Map, {Node, Links});
	false ->
	    [{Node, Links}|Map]
    end.
    
reachable(Node, Map) ->
    Result = lists:keysearch(Node, 1, Map),
    case Result of
	{value, {_Node, List}} ->
	    List;
	false ->
	    []
    end.

all_nodes(Map) ->
    List = lists:foldl(fun({Node, Links}, Output) -> [Node|Links] ++ Output end, [], Map),
    Set = sets:from_list(List),
    sets:to_list(Set).
