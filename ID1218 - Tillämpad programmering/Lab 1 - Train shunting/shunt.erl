-module(shunt).
-export([split/2,find/2,few/2,compress/1,rules/1]).

split([],_) ->
    {[],[]};
split(Xs,Y) ->
    {list:take(Xs,list:position(Xs,Y)-1),list:drop(Xs,list:position(Xs,Y))}.

find(_,{[],[],[]}) ->
    [];
find({[],[],[]},_) ->
    [];
find({Main,_,_},{MainY,_,_}) ->
    [H|_] = MainY,
    {Hs,Ts} = split(Main,H),
    list:append([{one,1+length(Ts)},{two,length(Hs)},{one,-1-length(Ts)},{two,-length(Hs)}],
		find({list:append(Ts,Hs),[],[]},{list:drop(MainY,1),[],[]})).

few(_,{[],[],[]}) ->
    [];
few({[],[],[]},_) ->
    [];
few({[H|T],[],[]},{[H|Y],[],[]}) ->
    few({T,[],[]},{Y,[],[]});
few({Main,_,_},{MainY,_,_}) ->
    [H|_] = MainY,
    {Hs,Ts} = split(Main,H),
    list:append([{one,1+length(Ts)},{two,length(Hs)},{one,-1-length(Ts)},{two,-length(Hs)}],
		few({list:append(Ts,Hs),[],[]},{list:drop(MainY,1),[],[]})).

compress(Ms) ->
    Ns = rules(Ms),
    if Ns == Ms ->
	    Ms;
        true ->
	    compress(Ns)
    end.

rules([]) ->
    [];
rules([{_,0}|T]) ->
    rules(T);
rules([{Tr,N},{Tr,M}|T]) ->
    [{Tr,N+M}|rules(T)];
rules([H|T]) ->
    [H|rules(T)].
