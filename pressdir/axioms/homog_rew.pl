/*	HOMOG.REW	*/
/* Written by Bernard Silver  Jan 1981  */
% Updated: 26 February 83

% :- public
% 		rew/5,
% 		rew1/5.

/* Try to rewrite each of the terms in the offending set as a 
    function of the reduced term */
rew(X,L,Subs,Unk,Type) :-  newtype(Type,New),
	maplist(rew1(New,X,Unk),L,L1),
	make_subl(L,L1,Subs),
	!.

			%  Kludge for stopping recursive calls of rew-rule
			%  in mixed case, and for getting the log case right
newtype(mixed,_) :- !.
newtype(log(X),log) :- X \== 10.
newtype(C,C) :- !.

rew1(_,X,_,X,X) :- !.
rew1(Type,A^B,Unk,Old,New) :- !,rew_rule(Type,A^B,Old,New,Unk).
rew1(Type,X,Unk,A^B,C^D) :- rew1(Type,X,Unk,A,C),rew1(Type,X,Unk,B,D),!.
rew1(Type,X,Unk,Old,New) :- rew_rule(Type,X,Old,New,Unk),!.

/* rew_rule(Type,Term1,Term2,Exp,Unk) gives Exp as a rewrite of Term2 in terms */
/* of Term1,where Unk is the  unknown, and the rule is for type Type */

/* Special cases   */
rew_rule(_,X,Y,X,_) :- match(X,Y),!.

rew_rule(_,_,Y,Y,Unk) :- freeof(Unk,Y),!.

/* Generalized Polynomial Rewrite rules  */
rew_rule(genpol,X^M,X,(X^M)^K,X) :- ok_number(M),!,eval(1/M,K).

rew_rule(genpol,X^N,X^M,(X^N)^K,X) :- ok_number(N),ok_number(M),!,eval(M/N,K).

/* Hyperbolic Rewrite rules  */
rew_rule(T,e^X,sinh(Z),((e^X)^K-(e^X)^(-K))/2,_) :- (T = hyper;T = hyper_exp),
	break(X,Z,P,Q),
	eval(Q/P,K),
	!.

rew_rule(T,e^X,cosh(Z),((e^X)^K+(e^X)^(-K))/2,_) :- (T = hyper;T = hyper_exp),
	break(X,Z,P,Q),
	eval(Q/P,K),
	!.

rew_rule(T,e^X,tanh(Z),((e^X)^K-(e^X)^(-K))*((e^X)^K+(e^X)^(-K))^ -1,_) :- 
	(T = hyper;T = hyper_exp),
	break(X,Z,P,Q),
	eval(Q/P,K),
	!.

rew_rule(T,e^X,sech(Z),((e^X)^K+(e^X)^(-K))^ -1*2,_) :- 
	(T = hyper;T = hyper_exp),
	break(X,Z,P,Q),
	eval(Q/P,K),!.

rew_rule(T,e^X,cosech(Z),((e^X)^K-(e^X)^(-K))^ -1*2,_) :- 
	(T = hyper;T = hyper_exp),
	break(X,Z,P,Q),
	eval(Q/P,K),!.

rew_rule(T,e^X,coth(Z),((e^X)^K+(e^X)^(-K))*((e^X)^K-(e^X)^(-K))^ -1,_) :- 
	(T = hyper;T = hyper_exp),
	break(X,Z,P,Q),
	eval(Q/P,K),
	!.

rew_rule(T,sinh(X),cosh(X),(1 + sinh(X)^2)^(1/2),_) :- 
	(T = hyper;T = hyper_exp),!.

rew_rule(T,cosh(X),sinh(X),(cosh(X)^2 - 1)^(1/2),_) :- 
	(T = hyper;T = hyper_exp),!.

rew_rule(T,sech(X),tanh(X),(1 - sech(X)^2)^(1/2),_) :- 
	(T = hyper;T = hyper_exp),!.

rew_rule(T,tanh(X),sech(X),(1 - tanh(X)^2)^(1/2),_) :- 
	(T = hyper;T = hyper_exp),!.

rew_rule(T,coth(X),cosech(X),(coth(X)^2 - 1)^(1/2),_) :- 
	(T = hyper;T = hyper_exp),!.

rew_rule(T,cosech(X),coth(X),(1 + cosech(X)^2)^(1/2),_) :- 
	(T = hyper;T = hyper_exp),!.

	
/* Exponential Rewrite rules  */
rew_rule(T,A^B,A^C,A^B,_) :- (T = exp;T = hyper_exp),match(B,C),!.

rew_rule(T,A^B,V^Z,X*Y,Unk) :- 	(T == exp;T == hyper_exp),
	match(Z,C*D+E),
	!,
	rew_rule(T,A^B,V^(C*D),X,Unk),
	rew_rule(T,A^B,V^E,Y,Unk).

rew_rule(T,A^B,A^Z,A^C*A^B,X) :- (T = exp;T = hyper_exp),
	match(Z,B+C),
	freeof(X,C),!.

rew_rule(T,A^B,A^Z,(A^B)^C,X) :- (T = exp;T = hyper_exp),
	match(Z,B*C),
	freeof(X,C),!.

rew_rule(Type,A^B,C^D,C^E*Z,X) :- Type == exp,
	ok_number(A),
	ok_number(C),
	match(D,B+E),
	freeof(X,E),				
	rew_rule(exp,A^B,C^B,Z,X),
	!.

rew_rule(exp,A^B,C^B,(A^B)^N,_) :- powered(A,N,C),!.

rew_rule(T,A^B,A^C,(A^B)^D,_) :- 
	(T = exp;T = hyper_exp),
	match(B,E*F),
	ok_number(E),
	match(C,G*F),
	ok_number(G),
	eval(G/E,D),
	!.


rew_rule(T,A^B,C^D,Term^N,X) :- 
	T == exp,
	powered(A,N,C),
	eval(N\=1),
	!,
	rew_rule(T,A^B,A^D,Term,X).

/* Trignometric Rewrite rules */
rew_rule(T,sin(X),sin(Z),V*cos(C) + V1*sin(C),U) :- T == trig,
	match(Z,B + C),
	contains(U,B),
	freeof(U,C),
	rew_rule(trig,sin(X),sin(B),V,U),
	rew_rule(trig,sin(X),cos(B),V1,U),
	!.

rew_rule(T,sin(X),cos(Z),V*cos(C) - V1*sin(C),U) :- T == trig,
	match(Z,B + C),
	contains(U,B),
	freeof(U,C),
	rew_rule(trig,sin(X),sin(B),V1,U),
	rew_rule(trig,sin(X),cos(B),V,U),
	!.

rew_rule(T,cos(X),sin(Z),V*cos(C) + V1*sin(C),U) :- T == trig,
	match(Z,B + C),
	contains(U,B),
	freeof(U,C),
	rew_rule(trig,cos(X),sin(B),V,U),
	rew_rule(trig,cos(X),cos(B),V1,U),
	!.

rew_rule(T,cos(X),cos(Z),V*cos(C) - V1*sin(C),U) :- T == trig,
	match(Z,B + C),
	contains(U,B),
	freeof(U,C),
	rew_rule(trig,cos(X),cos(B),V,U),
	rew_rule(trig,cos(X),sin(B),V1,U),
	!.


rew_rule(trig,sin(X),cos(Z),V,_) :- break(X,Z,P,Q),
	absol(Q,Q1),
	expcs(P,Q1,X,V),
	!.

rew_rule(trig,sin(X),sin(Z),I*(V),_) :- break(X,Z,P,Q),
	absol(Q,Q1),
	eval(sign(Q),I),
	expss(P,Q1,X,V),
	!.

rew_rule(trig,cos(X),sin(Z),I*(V),_) :- break(X,Z,P,Q),
	absol(Q,Q1),
	eval(sign(Q),I),
	expsc(P,Q1,X,V),
	!.

rew_rule(trig,cos(X),cos(Z),V,_) :- break(X,Z,P,Q),
	absol(Q,Q1),
	expcc(P,Q1,X,V),
	!.

rew_rule(trig,tan(X),sec(X),(1+tan(X)^2)^(1/2),_) :- !.

rew_rule(trig,sec(X),tan(X),(sec(X)^2-1)^(1/2),_) :- !.

rew_rule(trig,cot(X),cosec(X),(1+cot(X)^2)^(1/2),_) :- !.

rew_rule(trig,cosec(X),cot(X),(cosec(X)^2-1)^(1/2),_) :- !.

rew_rule(T,tan(X),tan(Z),(V + tan(C))/(1 - tan(C)*V),U) :- T == trig,
	match(Z,B + C),
	contains(U,B),
	freeof(U,C),
	rew_rule(trig,tan(X),tan(B),V,U),
	!.

rew_rule(trig,tan(X),tan(Z),I*(V),_) :- break(X,Z,P,Q),
	absol(Q,Q1),
	eval(sign(Q),I),
	exptt(P,Q1,X,V),
	!.

rew_rule(trig,tan(X),cosec(X),(1+tan(X)^2)^(1/2)/tan(X),_) :- !.

rew_rule(trig,tan(X),sin(X),tan(X)/(1+tan(X)^2)^(1/2),_) :- !.

rew_rule(trig,tan(X),cos(X),1/(1+tan(X)^2)^(1/2),_) :- !.

/* Tan half-angle Rewrite rules */
rew_rule(trig,tan(X),sin(Z),2*tan(X)*(1+tan(X)^2)^(-1),_) :-break(X,Z,P,Q),
	eval(Q/P=:=2),!.

rew_rule(trig,tan(X),cos(Z),(1-tan(X)^2)*(1+tan(X)^2)^(-1),_) :-break(X,Z,P,Q),
	eval(Q/P=:=2),!.

/* Reciprocal function Rewrite rules  */
rew_rule(T,X,tan(Z),A*B^ -1,Unk) :- T == trig,
	rew_rule(trig,X,sin(Z),A,Unk),
	rew_rule(trig,X,cos(Z),B,Unk),
	!.

rew_rule(T,A,sec(Z),(B)^ -1,Unk) :- T == trig,
	rew_rule(trig,A,cos(Z),B,Unk),
	!.

rew_rule(T,A,cosec(Z),(B)^ -1,Unk) :-  T == trig,
	rew_rule(trig,A,sin(Z),B,Unk),
	!.

rew_rule(T,A,cot(Z),(B)^ -1,Unk) :- T == trig,
	rew_rule(trig,A,tan(Z),B,Unk),
	!.


/* Logarithmic Rewrite rules  */
rew_rule(log,log(X,Y),log(Y,X),log(X,Y)^ -1,_) :- !.
 
rew_rule(log,log(X,Y),log(Z,Y),N1*log(X,Y),_) :- 
	powered(X,N,Z),
	!,
	eval(1/N,N1).

rew_rule(log,log(X,Y),log(Y,Z),N*log(X,Y)^ -1,_) :- powered(X,N,Z),!.

rew_rule(log,log(X,Y),log(X,Z),N1*log(X,Y),_) :- 
	powered(Y,N,Z),
	!,
	eval(1/N,N1).

rew_rule(log,log(X,Y),log(Z,X),N*log(X,Y)^ -1,_) :- powered(Y,N,Z),!.

			%  Reduced term is log base 10
rew_rule(log(10),log(10,X),log(X,10),log(10,X)^ -1,_) :- !.

rew_rule(log(10),log(10,X),log(A,X),Term,_Unk) :-
	ok_number(A),
	tidy(log(10,X)/log(10,A),Term),
	!.

rew_rule(log(10),log(10,X),log(X,A),Term,_Unk) :-
	ok_number(A),
	tidy(log(10,A)*(log(10,X)^ -1),Term),
	!.

/* Failure  */
rew_rule(_,X,Y,_,_) :- !,
	trace_press('\nFailed to find a rewrite for %t\n in terms of %t\n',[Y,X],2),
	fail.


