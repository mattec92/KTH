-module(node1).
-export([start/1, start/2, test/0]).
-define(Stabilize, 100).
-define(Timeout, 10000).

test() ->
    Pid = start(1),
    register(first, Pid),
    start(2, Pid),
    start(3, Pid),
    start(4, Pid),
    start(5, Pid).
    

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

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

node(Id, Predecessor, Successor) ->
    receive
	{key, Qref, Peer} ->
	    Peer ! {Qref, Id},
	    node(Id, Predecessor, Successor);
	{notify, New} ->
	    Pred = notify(New, Id, Predecessor),
	    node(Id, Pred, Successor);
	{request, Peer} ->
	    request(Peer, Predecessor),
	    node(Id, Predecessor, Successor);
	{status, Pred} ->
	    Succ = stabilize(Pred, Id, Successor),
	    node(Id, Predecessor, Succ);
	stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor);

	probe ->
	    create_probe(Id, Successor),
	    node(Id, Predecessor, Successor);
	{probe, Id, Nodes, T} ->
	    remove_probe(T, Nodes),
	    node(Id, Predecessor, Successor);
	{probe, Ref, Nodes, T} ->
	    forward_probe(Ref, T, Nodes, Id, Successor),
	    node(Id, Predecessor, Successor);

	status ->
	    io:format("node ~w: Predecessor ~w, Successor ~w~n", [Id, Predecessor, Successor]),
	    node(Id, Predecessor, Successor);
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

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
	nil ->
	    {Nkey, Npid};
	{Pkey, _} ->
	    case key:between(Nkey, Pkey, Id) of
		true ->
		    {Nkey, Npid};
		false ->
		    Predecessor 
	    end
    end.


create_probe(Id, Successor) ->
    {_, Pid} = Successor,
    Pid ! {probe, Id, [Id], erlang:now()}.

remove_probe(Time, Nodes) ->
    T = timer:now_diff(erlang:now(), Time),
    io:format("node ~w: Probe time ~w, list ~w~n", [whoissuedprobe, T, Nodes]).

forward_probe(Ref, Time, Nodes, Id, Successor) ->
    {_, Pid} = Successor,
    Pid ! {probe, Ref, Nodes ++ [Id], Time}.
