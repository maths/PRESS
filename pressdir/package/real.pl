%   File   : REAL.PL
%   Author : Bernard Silver
%   Updated: 31 May 1985
%   Purpose: Real number function evaluation for PRESS

% Very badly written!!


% Called by process_answer/2 in SOLVE

process_answer_23(A#B,A1#B1) :- !,
	process_answer_23(A,A1),
	process_answer_23(B,B1).

process_answer_23(A&B,A1&B1) :- !,
	process_answer_23(A,A1),
	process_answer_23(B,B1).

process_answer_23(A=B,Ans) :- !,
	handle(B,New),
	check_for_validity(A,B,New,Ans).

process_answer_23(false,false) :- !.
process_answer_23(true,true) :- !.

check_for_validity(A,B,New, X-(A=B)) :-
	member(X,[invalidlog,invalidasin,invalidacos,invalidexp]),
	contains(X,New),
	!.

check_for_validity(A,_,F,A=F).


handle(Ans,NewAns) :-
	const_parse(Ans,Set),
	mathevaluate(Set,SetAns),
	substitute_back(Set,SetAns,Ans,New),
	tidy(New,New1),
	evalmath(New1,NewAns,_).

substitute_back([],[],X,X) :- !.
substitute_back([H|T],[H1|T1],Old,New) :-
	subst(H=H1,Old,Mid),
	subst_lhs(T,NewT,H=H1),
	!,
	substitute_back(NewT,T1,Mid,New).

subst_lhs([],[],_).
subst_lhs([Old|T],[New|T1],Subs) :-
	subst(Subs,Old,New),
	!,
	subst_lhs(T,T1,Subs).

mathevaluate([],[]) :- !.
mathevaluate([H|T],[H1|T1]) :-
	matheval(H,H1),
	!,
	mathevaluate(T,T1).

pi(Pi) :- atan(1,X), Pi is 4*X.
convert_angle(Degree,Radian) :- pi(Pi),Radian is Degree*180/Pi.
inv_convert_angle(Radian,Degree) :- pi(Pi),Degree is Radian*Pi/180.

matheval(e,Ans) :- !,exp(1,Ans).
matheval(Atom,Atom) :- atomic(Atom),!.
matheval(A,B) :- top_matheval(A,B,win),!.
matheval(X,X) :- ok_number(X),!.   % Must be a long integer or procedure above would have got it.
matheval(F,Ans) :-
	F=..[Func|Args],
	Args \= [],
	mathevaluate(Args,Args1),
	New =.. [Func|Args1],
	top_matheval(New,Ans1,_),
	evalmath(Ans1,Ans,_).

long_eval_and_check(X,Ans) :-
	eval(X<100000),
	eval(X> -100000),  % Not long integer
	!,
	eval(numer(X),Y),
	eval(denom(X),Z),
	eval(sign(X),S),
	Ans is S*Y/Z.

top_matheval(A*B,Ans,win) :- number(A),number(B),!,Ans is A*B.
top_matheval(A+B,Ans,win) :- number(A),number(B),!,Ans is A+B.
top_matheval(X,Ans,win) :- 
	ok_number(X),
	!,
	long_eval_and_check(X,Ans).

top_matheval(e^X,Ans,win) :- number(X),!,exp(X,Ans).
top_matheval(A^ -1,Ans,win) :- number(A),!,Ans is 1/A.
top_matheval(A^B,Ans,win) :- number(A),integer(B),B>0,!, real_power(A,A,B,Ans).

top_matheval(A^B,Ans,win) :- number(A),number(B),exp_valid_check(A,B,Ans).
top_matheval(e,Ans,win) :- !,exp(1,Ans).
top_matheval(log(e,A),Ans,win) :- number(A),!,log(A,Ans).
top_matheval(log(10,A),Ans,win):- number(A),!,log10(A,Ans).
top_matheval(log(A,B),Ans,win) :- number(A),number(B),!,log_check_valid(B,A,Ans).
top_matheval(sin(X),Ans,win) :- number(X),!,convert_angle(X,Y),sin(Y,Ans).
top_matheval(cos(X),Ans,win) :- number(X),!,convert_angle(X,Y),cos(Y,Ans).
top_matheval(tan(X),Ans,win) :- number(X),!,convert_angle(X,Y),tan(Y,Ans).
top_matheval(arcsin(X),Ans,win) :- number(X),!,asin_check_valid(X,Ans).
top_matheval(arccos(X),Ans,win) :- number(X),!,acos_check_valid(X,Ans).
top_matheval(arctan(X),Ans,win) :- number(X),!,atan(X,Y),inv_convert_angle(Y,Ans).
top_matheval(X,X,lose).

real_power(_,Acc,1,Acc) :- !.
real_power(A,Acc,M,Ans) :-
	N is M - 1,
	New is A*Acc,
	!,
	real_power(A,New,N,Ans).
asin_check_valid(X,Ans) :-
	X >= -1,
	X =< 1,
	!,
	asin(X,Y),
	inv_convert_angle(Y,Ans).
asin_check_valid(_,invalidasin) :- !.

exp_valid_check(A,B,Ans) :- A > 0,!,log(A,Log),Exp is B*Log,exp(Exp,Ans).
exp_valid_check(_,_,invalidexp).

acos_check_valid(X,Ans) :-
	X >= -1,
	X =< 1,
	!,
	acos(X,Y),
	inv_convert_angle(Y,Ans).
acos_check_valid(_,invalidacos) :- !.

log_check_valid(B,A,Ans) :-
	B > 0,
	!,
	log(B,X),
	log(A,Y),
	Ans is X/Y.
log_check_valid(_,_,invalidlog) :- !.

const_parse(Eqn,Set) :- dl_parse_const(Eqn,Set1-[]),listtoset(Set1,Set).

dl_parse_const(e^X,[e^X|L]-L) :- !.
dl_parse_const(e,[e|L]-L) :- !.
dl_parse_const(X,L-L) :- atomic(X),!.
dl_parse_const(X,L-L) :- number(X),!.
dl_parse_const(A+B,L-L1) :- !,dl_parse_const(A,L-L2),dl_parse_const(B,L2-L1).
dl_parse_const(A*B,L-L1) :- !,dl_parse_const(A,L-L2),dl_parse_const(B,L2-L1).
dl_parse_const(A^B,L) :-number(B), !,dl_parse_const(A,L).
dl_parse_const(A,[A|L]-L) :- !.


evalmath(A+B,Ans,win) :- 
	evalmath(A,A1,win),
	evalmath(B,Sum,win),
	!,
	Ans is A1 + Sum.

evalmath(A*B,Ans,win) :- 
	evalmath(A,A1,win),
	evalmath(B,Prod,win),
	!,
	Ans is A1* Prod.
evalmath(A^B,Ans,win) :-
	evalmath(A,A1,win),
	evalmath(B,B1,win),
	!,
	evalmath1(A1,B1,Ans).

evalmath(X,X,win) :- number(X),!.
evalmath(X,Y,win) :- top_matheval(X,Y,win),!.
evalmath(X,X,Flag) :-
	ok_number(X), % Must be long int by now.
	!,
	Flag = lose.
evalmath(X,X,Flag) :- 
	atom(X),
	X \= e,
	!,
	Flag = lose.

evalmath(A,A,lose).


evalmath1(A,-1,Ans) :- !, Ans is 1/A.
evalmath1(A,B,Ans) :-
	integer(B),
	B > 0,
	!,
	real_power(A,A,B,Ans).

evalmath1(A,B,Ans) :-
	exp_valid_check(A,B,Ans).
