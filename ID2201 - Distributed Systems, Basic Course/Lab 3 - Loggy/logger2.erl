-module(logger2).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    %% Initialize node list with 1 as the counter value (whats defined in the workers).
    Clock = lamp:clock(Nodes),
    loop(Clock, []).

loop(Clock, Messages) ->
    receive
	{log, From, Time, Msg} ->
	    %% Add the message to the list of messages.
	    Updated = lamp:update(From, Time, Clock),
	    Inserted = insert(From, Time, Msg, Messages),
	    Rest = log(Updated, Inserted),

	    loop(Updated, Rest);
	stop ->
		log(lamp:inf(), Messages),
	    ok
    end.

log(_, []) ->
	ok;
log(Clock, Messages) ->
	[{Fi, Ti, Mi}|Rest] = Messages,
	case lamp:safe(Ti, Clock) of
		true ->
			io:format("log: ~w ~w ~p~n", [Ti, Fi, Mi]),
			log(Clock, Rest);
		false -> 
			Messages
    end.

insert(Fj, Tj, Mj, []) ->
	[{Fj, Tj, Mj}];
insert(Fj, Tj, Mj, Messages) ->
	[Entry|Rest] = Messages,
	{_, Ti, _} = Entry,
	case lamp:leq(Ti, Tj) of
		true ->
			[Entry|insert(Fj, Tj, Mj, Rest)];
		false ->
			[{Fj, Tj, Mj}|Messages]
	end.