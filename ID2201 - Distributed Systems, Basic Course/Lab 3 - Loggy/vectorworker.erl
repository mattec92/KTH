-module(vectorworker).
-export([start/6, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter, Nodes) ->
    Vector = lists:map(fun(N) -> {N, 0} end, Nodes),
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter, Vector) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter, Vector) ->
    random:seed(Seed, Seed, Seed),
    receive
	{peers, Peers} ->
	    loop(Name, Log, Peers, Sleep, Jitter, Vector);
	stop ->
	    ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Vector) ->
    Wait = random:uniform(Sleep),
    receive
	{msg, RemoteVector, Msg} ->
	    UpdatedVector = update_vector(Name, Vector, RemoteVector),
	    Log ! {log, Name, UpdatedVector, {received, Msg}},
	    loop(Name, Log, Peers, Sleep, Jitter, UpdatedVector);
	stop ->
	    ok;
	Error ->
	    Log ! {log, Name, time, {error, Error}, Vector}
    after Wait ->
	    Selected = select(Peers),
	    UpdatedVector = increment_vector(Name, Vector),
	    Message = {hello, random:uniform(100)},
	    Selected ! {msg, UpdatedVector, Message},
	    jitter(Jitter),
	    Log ! {log, Name, UpdatedVector, {sending, Message}},
	    loop(Name, Log, Peers, Sleep, Jitter, UpdatedVector)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) ->
    ok;
jitter(Jitter) -> 
    timer:sleep(random:uniform(Jitter)).

increment_vector(Node, Vector) ->
    {Node, Count} = lists:keyfind(Node, 1, Vector),
    lists:keyreplace(Node, 1, Vector, {Node, Count + 1}).

update_vector(Node, Vector, RemoteVector) ->
    IncrementedVector = increment_vector(Node, Vector),
    Merged = lists:zipwith(fun({N, IncrementedValue}, {N, RemoteValue}) -> 
				   {N, erlang:max(IncrementedValue, RemoteValue)} end,
			   IncrementedVector, 
			   RemoteVector).
			
