verify(InputFileName) :-
	see(InputFileName),
	read(Prems), read(Goal), read(Proof),
	seen,
	valid_proof(Prems, Goal, Proof).

valid_proof(Prems, Goal, Proof) :-
	valid_proof(Prems, Goal, Proof, []).

%Om inga ovaliderade rader och senast validerade raden är slutsatsen.
valid_proof(_Prems, Goal, [], [[_Row, Goal, _Rule]|_Validate]).

%Första raden är en box som (måste) börjar med ett antagande.
valid_proof(Prems, Goal, [[[Row, Result, assumption]|Boxtail]|Prooftail], Validated) :-
	valid_box(Prems, Goal, Boxtail, [[Row, Result, assumption]|Validated]),
	valid_proof(Prems, Goal, Prooftail, [[[Row, Result, assumption]|Boxtail]|Validated]).

%Första raden är resultatet av en regel applicerad på tidigare validerad bevisrad.
valid_proof(Prems, Goal, [Proofhead|Prooftail], Validated) :-
	valid_rule(Prems, Proofhead, Validated),
	valid_proof(Prems, Goal, Prooftail, [Proofhead|Validated]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% RULES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%premise
valid_rule(Prems, [_Row, Result, premise], _Validated) :-
	member(Result, Prems).

%negnegel(X)
valid_rule(_Prems, [_Row, Result, negnegel(X)], Validated) :-
	member([X, neg(neg(Result)), _], Validated).

%impel(X,Y)
valid_rule(_Prems, [_Row, Result, impel(X,Y)], Validated) :-
	member([X, Impprem, _], Validated),
	member([Y, imp(Impprem, Result), _], Validated). 

%copy(X)
valid_rule(_Prems, [_Row, Result, copy(X)], Validated) :-
	member([X, Result, _], Validated).

%andint(X,Y)
valid_rule(_Prems, [_Row, and(And1, And2), andint(X,Y)], Validated) :-
	member([X, And1, _], Validated),
	member([Y, And2, _], Validated).

%andel1(X)
valid_rule(_Prems, [_Row, Result, andel1(X)], Validated) :-
	member([X, and(Result, _), _], Validated).

%andel2(X)
valid_rule(_Prems, [_Row, Result, andel2(X)], Validated) :-
	member([X, and(_, Result), _], Validated).

%contel(X)
valid_rule(_Prems, [_Row, _Result, contel(X)], Validated) :-
	member([X, cont, _], Validated).

%negnegint(X)
valid_rule(_Prems, [_Row, neg(neg(Result)), negnegint(X)], Validated) :-
	member([X, Result, _], Validated).

%orint1(X)
valid_rule(_Prems, [_Row, or(Result, _Other), orint1(X)], Validated) :-
	member([X, Result, _], Validated).

%orint2(X)
valid_rule(_Prems, [_Row, or(_Other, Result), orint2(X)], Validated) :-
	member([X, Result, _], Validated).

%lem
valid_rule(_Prems, [_Row, or(X, neg(X)), lem], _Validated).

%negel(X,Y)
valid_rule(_Prems, [_Row, cont, negel(X,Y)], Validated) :-
	member([X, Cont, _], Validated),
	member([Y, neg(Cont), _], Validated).

%mt(X,Y)
valid_rule(_Prems, [_Row, neg(Result), mt(X,Y)], Validated) :-
	member([X, imp(Result, Second), _], Validated),
	member([Y, neg(Second), _], Validated).

%negint(X,Y)
valid_rule(_Prems, [_Row, neg(Result), negint(X,Y)], Validated) :-
	find_box(X, Validated, Prevbox),
	member([X, Result, assumption], Prevbox),
	member([Y, cont, _], Prevbox).

%pbc(X,Y)
valid_rule(_Prems, [_Row, Result, pbc(X,Y)], Validated) :-
	find_box(X, Validated, Prevbox),
	member([X, neg(Result), assumption], Prevbox),
	member([Y, cont, _], Prevbox).

%impint(X,Y)
valid_rule(_Prems, [_Row, imp(Impprem, Result), impint(X,Y)], Validated) :-
	find_box(X, Validated, Prevbox),
	member([X, Impprem, assumption], Prevbox),
	member([Y, Result, _], Prevbox).

%orel(X,Y,U,V,W)
valid_rule(_Prems, [_Row, Result, orel(X,Y,U,V,W)], Validated) :-
	find_box(Y, Validated, FirstBox),
	find_box(V, Validated, SecondBox),
	member([X, or(First, Second), _], Validated),
	member([Y, First, _], FirstBox),
	member([U, Result, _], FirstBox),
	member([V, Second, _], SecondBox),
	member([W, Result, _], SecondBox).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% BOXHANTERING %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Alla rader i lådan är validerade.
valid_box(_Prems, _Goal, [], _Validated).

%Första raden är en box som (måste) börjar med ett antagande.
valid_box(Prems, Goal, [[[Row, Result, assumption]|Boxtail]|Prooftail], Validated) :-
	valid_box(Prems, Goal, Boxtail, [[Row, Result, assumption]|Validated]),
	valid_box(Prems, Goal, Prooftail, [[[Row, Result, assumption]|Boxtail]|Validated]).

%Första raden är resultatet av en regel applicerad på tidigare validerad bevisrad.
valid_box(Prems, Goal, [Proofhead|Prooftail], Validated) :-
	valid_rule(Prems, Proofhead, Validated),
	valid_box(Prems, Goal, Prooftail, [Proofhead|Validated]).

%Första raden är en box som innehåller eftersökt rad.
find_box(Searchfor, [Boxhead|_Validated], Boxhead) :-
	member([Searchfor, _, _], Boxhead).

%Om inte leta efter raden i svansen.
find_box(Searchfor, [_|Validated], _Box) :-
	find_box(Searchfor, Validated, _Box).
