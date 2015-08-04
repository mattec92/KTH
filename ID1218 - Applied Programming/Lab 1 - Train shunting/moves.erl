-module(moves).
-export([single/2,move/2]).


single(Move,State) ->
    {Main, One, Two} = State,
    case Move of
	{one, N} when N > 0  -> %From main to one
	    {list:take(Main,length(Main)-N), list:append(list:drop(Main,length(Main)-N),One), Two};
	{one, N} when N < 0  -> %From one to main
	    {list:append(Main,list:take(One,-N)),list: drop(One,-N), Two};
	{two, N} when N > 0 -> %From main to two
	    {list:take(Main,length(Main)-N),One, list:append(list:drop(Main,length(Main)-N),Two)};
	{two, N} when N < 0 -> %From one to main
	    {list:append(Main,list:take(Two,-N)),One,list: drop(Two,-N)};
	{_, 0} ->
	    State
     end.

move(Moves,State) ->
    case Moves of
	[] ->
	    State;
	[H|T] ->
	    list:append([State], move(T,single(H,State)))
    end.

%move1([],State) ->
%    State;
    
%move1([H|T],State) ->
%    list:append([State], move1(T,single(H,State))).
