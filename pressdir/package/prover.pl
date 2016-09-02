/*		PROVER 		Updated: 14 January 83
*/
/********************************
 THEOREM PROVERS
********************************/

/*FIND MAXIMUM OF SET*/

maximum(IneqC,AnsC) :-
	andtodot(IneqC,IneqL),
	maximum1(IneqL,AnsL),
	dottoand(AnsL,AnsC),
	!.

maximum1([],[]) :- !.

maximum1([Ineq],[Ineq]) :- !.

maximum1([Ineq|Rest],Ans) :-
	some(smaller(Ineq),Rest), !,
	maximum1(Rest,Ans).

maximum1([Ineq|Rest],[Ineq]) :-
	checklist(bigger(Ineq),Rest), !.

maximum1([Ineq|Rest],[Ineq|Ans]) :-
	maximum1(Rest,Ans), !.

/*FIND MINIMUM OF SET*/

minimum(IneqC,AnsC) :-
	andtodot(IneqC,IneqL),
	minimum1(IneqL,AnsL),
	dottoand(AnsL,AnsC),
	!.

minimum1([],[]) :- !.

minimum1([Ineq],[Ineq]) :- !.

minimum1([Ineq|Rest],Ans) :-
	some(bigger(Ineq),Rest), !,
	minimum1(Rest,Ans).

minimum1([Ineq|Rest],[Ineq]) :-
	checklist(smaller(Ineq),Rest), !.

minimum1([Ineq|Rest],[Ineq|Ans]) :-
	minimum1(Rest,Ans), !.


/*INEQ1 DOMINATES INEQ2*/    
smaller(Ineq2,Ineq1) :- bigger(Ineq1,Ineq2), !.

			%  Greater thans

bigger(X>=Y,X>=Z) :- prove(Y>=Z), !.
bigger(X>Y,X>Z) :- prove(Y>=Z), !.
bigger(X>Y,X>=Z) :- prove(Y>=Z), !.
bigger(X>=Y,X>Z) :- prove(Y>Z), !.

			%  Less thans

bigger(X=<Y,X=<Z) :- prove(Y>=Z), !.
bigger(X<Y,X<Z) :- prove(Y>=Z), !.
bigger(X<Y,X=<Z) :- prove(Y>=Z), !.
bigger(X=<Y,X<Z) :- prove(Y>Z), !.

/* Prove simple inequalities etc*/

prove(X>=Y) :- simplify(X+(-1*Y) , E), non_neg(E), !.

prove(X>Y) :- simplify(X+(-1*Y), E), positive(E), !.

prove(X=\=Y) :- simplify(X+(-1*Y), E), non_zero(E), !.

prove(X=Y) :- simplify(X+(-1*Y), 0), !.

/* Simplify formulae into true or false if possible*/

verify(F,true) :- prove(F), !.

verify(F,false) :- negation(F,NF), prove(NF), !.

verify(F,F) :- !.

/* Negation of formula */
negation(F,NF) :- negation1(F,NF), !.
negation(F,NF) :- negation1(NF,F), !.

negation1(A=B,A=\=B).
negation1(A>=B,B>A).

