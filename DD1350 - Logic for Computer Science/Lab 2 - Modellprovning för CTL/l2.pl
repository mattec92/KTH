% For SICStus, uncomment line below: (needed for member/2)
:- use_module(library(lists)).

% Load model, initial state and formula from file.
verify(Input) :-
see(Input), read(T), read(L), read(S), read(F), seen,
check(T, L, S, [], F).

% check(T, L, S, U, F)
% T - The transitions in form of adjacency lists
% L - The labeling
% S - Current state
% U - Currently recorded states
% F - CTL Formula to check.
%
% Should evaluate to true iff the sequent below is valid.
%
% (T,L), S |- F
% U
% To execute: consult('your_file.pl'). verify('input.txt').

% Literals
check(_, L, S, [], F) :-
	member([S, Labels], L),
	member(F , Labels).

% Negation
check(T, L, S, [], neg(F)) :-
	\+ check(T, L, S, [], F).
		 
% And
check(T, L, S, [], and(F,G)) :-
	check(T, L, S, [], F),
	check(T, L, S, [], G).

% Or
check(T, L, S, [], or(F,_)) :-
	check(T, L, S, [], F).
check(T, L, S, [], or(_,G)) :-
	check(T, L, S, [], G).

% AX
check(T, L, S, [], ax(F)) :-
	check(T, L, S, [], neg(ex(neg(F)))).


% EX
check(T, L, S, [], ex(F)) :-
	member([S, Trans], T),
	member(NextState, Trans),
	check(T, L, NextState, [], F).

% AG 
check(T, L, S, U, ag(F)) :-
	check(T, L, S, U, neg(ef(neg(F)))).

% EG
check(_, _, S, U, eg(_)) :-
	member(S,U).
check(T, L, S, U, eg(F)) :-
	\+ member(S, U),
	check(T, L, S, [], F),
	member([S, Trans], T),
	member(NextState, Trans),
	check(T, L, NextState, [S|U], eg(F)).

% EF
check(T, L, S, U, ef(F)) :-
	\+ member(S, U),
	check(T, L, S, [], F).
check(T, L, S, U, ef(F)) :-
	\+ member(S, U),
	member([S, Trans], T),
	member(NextState, Trans),
	check(T, L, NextState, [S|U], ef(F)).

% AF
check(T, L, S, U, af(F)) :-
	check(T, L, S, U, neg(eg(neg(F)))).
