-module(node4).
-export([start/1, start/2]).
-define(Stabilize, 100).
-define(Timeout, 10000).
-compile({no_auto_import,[demonitor/1]}).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    register(Id, spawn(fun() -> init(Id, Peer) end)).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, nil, store:create(), store:create()).

connect(Id, nil) ->
    {ok, {Id, nil, self()}};
connect(_Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
	{Qref, Skey} ->
	    Sref = monitor(Peer),
	    {ok, {Skey, Sref, Peer}}
    after ?Timeout ->
	    io:format("Time out: no response~n",[])
    end.

node(Id, Predecessor, Successor, Next, Store, Replica) ->
    receive
	{key, Qref, Peer} ->
	    Peer ! {Qref, Id},
	    node(Id, Predecessor, Successor, Next, Store, Replica);
	{notify, New} ->
	    {Pred, UpdatedStore} = notify(New, Id, Predecessor, Store, Replica),
	    node(Id, Pred, Successor, Next, UpdatedStore, Replica);
	{request, Peer} ->
	    request(Peer, Predecessor, Next),
	    node(Id, Predecessor, Successor, Next, Store, Replica);
	{status, Pred, Nx} ->
	    {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
	    node(Id, Predecessor, Succ, Nxt, Store, Replica);
	stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor, Next, Store, Replica);

	{add, Key, Value, Qref, Client} ->
	    Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
	    node(Id, Predecessor, Successor, Next, Added, Replica);
	{lookup, Key, Qref, Client} ->
	    lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
	    node(Id, Predecessor, Successor, Next, Store, Replica);
	{handover, Elements, ReplicaElements} ->
	    Merged = store:merge(Store, Elements),
	    MergedReplica = store:merge(Replica, ReplicaElements),
	    Successor ! {replica, Merged},
	    node(Id, Predecessor, Successor, Next, Merged, MergedReplica);
	{'DOWN', Ref, process, _, _} ->
	    {Pred, Succ, Nxt, NewStore} = down(Ref, Predecessor, Successor, Next, Store, Replica),
	    node(Id, Pred, Succ, Nxt, NewStore, store:create());

	{replicate, Client, Qref, Key, Value} ->
	    NewReplica = store:add(Key, Value, Replica),
	    Client ! {Qref, ok},
	    node(Id, Predecessor, Successor, Next, Store, NewReplica);
	{replica, NewReplica} ->
	    node(Id, Predecessor, Successor, Next, Store, NewReplica);

	probe ->
	    create_probe(Id, Successor),
	    node(Id, Predecessor, Successor, Next, Store, Replica);
	{probe, Id, Nodes, T} ->
	    remove_probe(T, Nodes),
	    node(Id, Predecessor, Successor, Next, Store, Replica);
	{probe, Ref, Nodes, T} ->
	    forward_probe(Ref, T, Nodes, Id, Successor),
	    node(Id, Predecessor, Successor, Next, Store, Replica);

	status ->
	    io:format("node ~w: Predecessor ~w, Successor ~w~n", [Id, Predecessor, Successor]),
	    node(Id, Predecessor, Successor, Next, Store, Replica);
	stop ->
	    ok
    end.

stabilize(Pred, Next, Id, Successor) ->
    {Skey, Sref, Spid} = Successor,
    case Pred of
	nil ->
	    Spid ! {notify, {Id, self()}},
	    {Successor, Next};
	{Id, _} ->
	    {Successor, Next};
	{Skey, _} ->
	    Spid ! {notify, {Id, self()}},
	    {Successor, Next};
	{Xkey, Xpid} ->
	    case key:between(Xkey, Id, Skey) of
		true ->
		    Xpid ! {request, self()},
		    Xref = monitor(Xpid),
		    demonitor(Sref),
		    {{Xkey, Xref, Xpid}, Successor};
		false ->
		    Spid ! {notify, {Id, self()}},
		    {Successor, Next}
	    end
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, _, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor, Next) ->
    case Predecessor of
	nil ->
	    Peer ! {status, nil, Next};
	{Pkey, _, Ppid} ->
	    Peer ! {status, {Pkey, Ppid}, Next}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store, Replica) ->
    case Predecessor of
	nil ->
	    Keep = handover(Store, Replica, Nkey, Npid),
	    Nref = monitor(Npid),
	    {{Nkey, Nref, Npid}, Keep};
	{Pkey, Pref,  _} ->
	    case key:between(Nkey, Pkey, Id) of
		true ->
		    Keep = handover(Store, Replica, Nkey, Npid),
		    Nref = monitor(Npid),
		    demonitor(Pref),
		    {{Nkey, Nref, Npid}, Keep};
		false ->
		    {Predecessor, Store}
	    end
    end.

create_probe(Id, Successor) ->
    {_, _, Pid} = Successor,
    Pid ! {probe, Id, [Id], erlang:now()}.

remove_probe(Time, Nodes) ->
    T = timer:now_diff(erlang:now(), Time),
    io:format("node ~w: Probe time ~w, list ~w~n", [whoissuedprobe, T, Nodes]).

forward_probe(Ref, Time, Nodes, Id, Successor) ->
    {_, _, Pid} = Successor,
    Pid ! {probe, Ref, Nodes ++ [Id], Time}.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
	true ->
	    Spid ! {replicate, Client, Qref, Key, Value},
	    %%io:format("node ~w: added successfully key ~w, value ~w~n", [Id, Key, Value]),
	    store:add(Key, Value, Store);
	false ->
	    Spid ! {add, Key, Value, Qref, Client},
	    Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
	true ->
	    Result = store:lookup(Key, Store),
	    %%io:format("node ~w: lookup successful: ~w~n", [Id, Result]),
	    Client ! {Qref, Result};
	false ->
	    {_, _, Spid} = Successor,
	    Spid ! {lookup, Key, Qref, Client}
    end.

handover(Store, Replica, Nkey, Npid) ->
    {Keep, Leave} = store:split(Nkey, Store),
    {ReplicaKeep, ReplicaLeave} = store:split(Nkey, Replica),
    Npid ! {handover, Leave, ReplicaLeave},
    {Keep, ReplicaKeep}.

monitor(Pid) ->
    erlang:monitor(process, Pid).

demonitor(nil) ->
    ok;
demonitor(Pid) ->
    erlang:demonitor(Pid, [flush]).

down(Ref, {_, Ref, _}, Successor, Next, Store, Replica) ->
    NewStore = store:merge(Store, Replica),
    {nil, Successor, Next, NewStore};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}, Store, _Replica) ->
    Nref = monitor(Npid),
    %%And make sure to run the stabilizing procedure
    Npid ! {replica, Store},
    {Predecessor, {Nkey, Nref, Npid}, nil, Store}.
