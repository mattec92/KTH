-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
    [].

add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid}|Intf].

remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
    Entry = lists:keyfind(Name, 1, Intf),
    case Entry of
	{Name, _Ref, Pid} ->
	    {ok, Pid};
	false  ->
	    notfound
    end.

ref(Name, Intf) ->
    Entry = lists:keyfind(Name, 1, Intf),
    case Entry of
	{Name, Ref, _Pid} ->
	    {ok, Ref};
	false  ->
	    notfound
    end.

name(Ref, Intf) ->
    Entry = lists:keyfind(Ref, 2, Intf),
    case Entry of
	{Name, Ref, _Pid} ->
	    {ok, Name};
	false  ->
	    notfound
    end.

list(Intf) ->
    lists:map(fun({Name, _Ref, _Pid}) -> Name end, Intf).

broadcast(Message, Intf) ->
    lists:foreach(fun({_Name, _Ref, Pid}) -> Pid ! Message end, Intf).
