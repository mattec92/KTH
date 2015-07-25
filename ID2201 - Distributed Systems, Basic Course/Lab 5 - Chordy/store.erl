-module(store).
-export([create/0, add/3, lookup/2, split/2, merge/2,size/1]).

create() ->
    [].

add(Key, Value, Store) ->
    lists:keystore(Key, 1, Store, {Key, Value}).

lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

split(Key, Store) ->
    Sorted = lists:keysort(1, Store),
    lists:splitwith(fun(E) -> E =< Key end, Sorted).

merge(ToMerge, Store) ->
    Store ++ ToMerge.

size(Store) ->
	length(Store).