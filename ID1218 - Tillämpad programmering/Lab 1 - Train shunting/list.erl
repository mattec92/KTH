-module(list).
-export([take/2,drop/2,append/2,member/2,position/2]).

take([],_)->
    [];
take(_,0)->
    [];
take([H|T],N)->
    [H|take(T,N-1)].

drop([],_)->
    [];
drop(Xs,0)->
    Xs;
drop([_|T],N)-> 
    drop(T,N-1).

append([],Ys)->
    Ys;
append(Xs,[])->
    Xs;
append([H|T],Ys) ->
    [H|append(T, Ys)].

member([],_)->
    no;
member([Y|_],Y)->
    yes;
member([_|T],Y)->
    member(T,Y).

position([],_)->
    0;
position([Y|_],Y)->
    1;
position([_|T],Y) ->
    1+ position(T,Y).
