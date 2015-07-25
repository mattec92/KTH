-module(node2).
-export([start/1, start/2]).
-define(Stabilize, 100).
-define(Timeout, 10000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, store:create()).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(_Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
	{Qref, Skey} ->
	    {ok, {Skey, Peer}}
    after ?Timeout ->
	    io:format("Time out: no response~n",[])
    end.

node(Id, Predecessor, Successor, Store) ->
    receive
	{key, Qref, Peer} ->
	    Peer ! {Qref, Id},
	    node(Id, Predecessor, Successor, Store);
	{notify, New} ->
	    {Pred, UpdatedStore} = notify(New, Id, Predecessor, Store),
	    node(Id, Pred, Successor, UpdatedStore);
	{request, Peer} ->
	    request(Peer, Predecessor),
	    node(Id, Predecessor, Successor, Store);
	{status, Pred} ->
	    Succ = stabilize(Pred, Id, Successor),
	    node(Id, Predecessor, Succ, Store);
	stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor, Store);

	{add, Key, Value, Qref, Client} ->
	    Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
	    node(Id, Predecessor, Successor, Added);
	{lookup, Key, Qref, Client} ->
	    lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
	    node(Id, Predecessor, Successor, Store);
	{handover, Elements} ->
	    Merged = store:merge(Store, Elements),
	    node(Id, Predecessor, Successor, Merged);

	probe ->
	    create_probe(Id, Successor, Store),
	    node(Id, Predecessor, Successor, Store);
	{probe, Id, Size, Nodes, T} ->
	    remove_probe(T, Nodes, Size),
	    node(Id, Predecessor, Successor, Store);
	{probe, Ref, Size, Nodes, T} ->
	    forward_probe(Ref, T, Size, Nodes, Id, Successor, Store),
	    node(Id, Predecessor, Successor, Store);

	status ->
	    io:format("node ~w: Predecessor ~w, Successor ~w Store size ~w~n", [Id, Predecessor, Successor, store:size(Store)]),
	    node(Id, Predecessor, Successor, Store);
	stop ->
	    ok
    end.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
	nil ->
	    Spid ! {notify, {Id, self()}},
	    Successor;
	{Id, _} ->
	    Successor;
	{Skey, _} ->
	    Spid ! {notify, {Id, self()}},
	    Successor;
	{Xkey, Xpid} ->
	    case key:between(Xkey, Id, Skey) of
		true ->
		    Xpid ! {request, self()},
		    Pred;
		false ->
		    Spid ! {notify, {Id, self()}},
		    Successor
	    end
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor) ->
    case Predecessor of
	nil ->
	    Peer ! {status, nil};
	{Pkey, Ppid} ->
	    Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
	nil ->
	    Keep = handover(Store, Nkey, Npid),
	    {{Nkey, Npid}, Keep};
	{Pkey, _} ->
	    case key:between(Nkey, Pkey, Id) of
		true ->
		    Keep = handover(Store, Nkey, Npid),
		    {{Nkey, Npid}, Keep};
		false ->
		    {Predecessor, Store}
	    end
    end.

create_probe(Id, Successor, Store) ->
    {_, Pid} = Successor,
    S = store:size(Store),
    Pid ! {probe, Id, S, [Id], erlang:now()}.

remove_probe(Time, Nodes, Size) ->
    T = timer:now_diff(erlang:now(), Time),
    io:format("Probe time ~w, list ~w, Size ~w~n", [T, Nodes, Size]).

forward_probe(Ref, Time, Size, Nodes, Id, Successor, Store) ->
    {_, Pid} = Successor,
    S = store:size(Store),
    Pid ! {probe, Ref, Size + S, [Id|Nodes], Time}.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {Skey, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
	true ->
	    Client ! {Qref, ok},
	    %%io:format("node ~w: added successfully key ~w, value ~w~n", [Id, Key, Value]),
	    store:add(Key, Value, Store);
	false ->
	    Spid ! {add, Key, Value, Qref, Client},
	    %%io:format("forwarding key ~w add to node ~w~n", [Key, Skey]),
	    Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
	true ->
	    Result = store:lookup(Key, Store),
	    %%io:format("node ~w: lookup successful: ~w~n", [Id, Result]),
	    Client ! {Qref, Result};
	false ->
	    {_, Spid} = Successor,
	    Spid ! {lookup, Key, Qref, Client}
    end.

handover(Store, Nkey, Npid) ->
    {Keep, Leave} = store:split(Nkey, Store),
    Npid ! {handover, Leave},
    Keep.
