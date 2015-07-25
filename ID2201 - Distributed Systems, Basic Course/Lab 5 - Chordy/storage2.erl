-module(storage2).

-export([add/3, lookup/2, new/0, split/2, merge/2, size/1]).

new() ->
    ets:new(storage, [private]).

add(Key, Value, Storage) ->
    ets:insert(Storage, {Key, Value}),
    Storage.

lookup(Key, Storage) ->
    case ets:lookup(Storage, Key) of
	[] ->
	    false;
	Found ->
	    hd(Found)
    end.

split(Id, Storage) ->
    Selected = ets:select(Storage, [{{'$1', '$2'},[{'=<','$1', Id}],[{{'$1', '$2'}}]}]),
    lists:foreach( fun({Key,_}) -> ets:delete(Storage, Key) end, Selected),
    {Storage, Selected}.
			    
merge(Storage, More) ->
    ets:insert(Storage, More),
    Storage.

size(Storage) ->
    ets:select_count(Storage, [{{'$1', '$2'},[true],[true]}]).
