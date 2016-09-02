%   File   : LOOP
%   Author : Bernard Silver
%   Updated: 23 February 1984
%   Purpose: Loop checking for LP

:- dynamic seen_eqn/1.

seen_eqn(_) :- !,fail.


looping(Eqn,X) :- flag(loop,Old,Old),looping_check(Eqn,X,Old),!.

looping_check(_,_,no) :- !.
looping_check(Eqn,X,Old) :-
	normstore(Eqn,X,Eqn1),
	((seen_eqn(Eqn1) -> looping_action(Old));
	asserta(seen_eqn(Eqn1))).

looping_action(X) :- 
	(X=warn;X=warn1),
	!,
	writef('\n[**Warning. Equation has been seen before**]\n').

looping_action(yes) :- !,
	writef('\n**Looping**.  This equation has been seen before.\n'),
	process_reply([a,b,c,n,w],loop_action(X),X,'Action:',l_text).
	
l_text :-
	writef('\nType

	a  : to abort,

	b  : to break,
	
	c  : to continue,
	
	n  : to switch off loop test,
	
	w  : to switch loop test to warn only.\n').

loop_action(a) :- !,writef('\n[Aborting]\n'),abort.
loop_action(b) :- !,
	writef('\n[Entering break]\n'),
	break,
	writef('\n[Leaving break and continuing]\n').
loop_action(c) :- !,writef('\n[Continuing]\n').
loop_action(n) :- !,
	flag(loop,_,no),
	writef('\n[Disabling loop checker]\n').
loop_action(w) :- !,
	flag(loop,_,warn),
	writef('\n[Setting loop checker to warn only]').
loop_action(_) :- writef('\nNot a valid option! Please try again.\n'),fail.
	
normstore(Eqn,X,Eq) :- 
	subst(X  = unk,Eqn,Eqn1),
	!,
	remove_arbs(Eqn1,Eq),
	!.



remove_arbs(Eqn1,Eqn2) :- 
	wordsin(Eqn1,Words),
	subintegral(Words,Word),
	remove_arbs1(Eqn1,Word,Eqn3),
	tidy(Eqn3,Eqn2),
	!.

remove_arbs1(X,[],X) :- !.
remove_arbs1(X,H,Y) :- make_arblist(H,Z),make_subl(H,Z,Y1),subs1(X,Y1,Y),!.

make_arblist(H,Z) :- make_arblist1(H,Z,1),!.

make_arblist1([],[],_) :- !.
make_arblist1([_|T],[arb(N)|T1],N) :- M is N+1,make_arblist1(T,T1,M),!.
