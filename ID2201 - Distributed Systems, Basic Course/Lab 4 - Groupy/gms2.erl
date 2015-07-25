-module(gms2).
-export([start/1, start/2]).

-define(timeout, 1000).
-define(arghh, 1000).

start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> leaderinit(Id, Rnd, Self) end)}.

leaderinit(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, [], [Master]).

start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
    Self = self(),
    %%io:format("gms ~w: Sending slave request~n", [Id]), Added by me
    Grp ! {join, Master, Self},
    receive
	{view, [Leader|Slaves], Group} ->
	    %%io:format("gms ~w: Received slave response~n", [Id]), Added by me
	    Master ! {view, Group},
	    erlang:monitor(process, Leader),
	    slave(Id, Master, Leader, Slaves, Group)
    after ?timeout ->
	    Master ! {error, "no reply from leader"}
    end.

slave(Id, Master, Leader, Slaves, Group) ->
    receive
	{mcast, Msg} ->
	    %%io:format("gms ~w: received {mcast, ~w} in state ~w~n", [Id, Msg, N]),
	    Leader ! {mcast, Msg},
	    slave(Id, Master, Leader, Slaves, Group);
	{join, Wrk, Peer} ->
	    %%io:format("gms ~w: forward join from ~w to leader~n", [Id, Peer]),
	    Leader ! {join, Wrk, Peer},
	    slave(Id, Master, Leader, Slaves, Group);
	{msg, Msg} ->
	    %%io:format("gms ~w: deliver msg ~w in state ~w~n", [Id, Msg, N]),
	    Master ! Msg,
	    slave(Id, Master, Leader, Slaves, Group);
	{view, [Leader|Slaves2], Group2} ->
	    %%io:format("gms ~w: received view ~w ~w~n", [Id, N, View]),
	    Master ! {view, Group2},
	    slave(Id, Master, Leader, Slaves2, Group2);
	{'DOWN', _Ref, process, Leader, _Reason} ->
	    election(Id, Master, Slaves, Group);
	stop -> 
	    ok;
	Error ->
	    io:format("gms ~w: slave, strange message ~w~n", [Id, Error])
    end.

leader(Id, Master, Slaves, Group) ->
    receive
	{mcast, Msg} ->
	    %%io:format("gms ~w: received {mcast, ~w} in state ~w~n", [Id, Msg, N]),
	    bcast(Id, {msg, Msg}, Slaves),
	    Master ! Msg,
	    leader(Id, Master, Slaves, Group);
	{join, Wrk, Peer} ->
	    %%io:format("gms ~w: forward join from ~w to master~n", [Id, Peer]),
	    Slaves2 = lists:append(Slaves, [Peer]),
	    Group2 = lists:append(Group, [Wrk]),
	    bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
	    Master ! {view, Group2},
	    leader(Id, Master, Slaves2, Group2);
	stop -> 
	    ok;
	Error ->
	    io:format("gms ~w: leader, strange message ~w~n", [Id, Error])
    end.

election(Id, Master, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
	[Self|Rest] ->
	    bcast(Id, {view, Slaves, Group}, Rest),
	    Master ! {view, Group},
	    leader(Id, Master, Rest, Group);
	[Leader|Rest] ->
	    erlang:monitor(process, Leader),
	    slave(Id, Master, Leader, Rest, Group)
    end.

bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
    case random:uniform(?arghh) of
	?arghh ->
	    io:format("leader ~w: crash~n",
		      [Id]),
	    exit(no_luck);
	_ ->
	    ok
    end.
