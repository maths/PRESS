%   Press:Diff.				Updated: 12 Sept 81

%======================================================================%
%			Differential Calculus		19.2.81	       %
%======================================================================%

% :- public diffwrt/3.
% :- mode
%     diffwrt(+, -, +),
% 	dx(+, -, +),
% 	    exactly_one_arg(+, +, -),
% 		exactly_one_arg(+, +, +, ?).


diffwrt(Exp, Ans, Var) :-
	trace_press('Differentiating %c with respect to %t\n', [Exp, Var], 1),
	dx(Exp, Der, Var),
	tidy(Der, Ans),
	trace_press('   gives : %c\n', [Ans], 1), !.


dx(Exp, 0, X) :-
	freeof(X, Exp), !.

dx(X, 1, X) :- !.

dx(X^N, N*X^M, X) :-
	freeof(X, N),
	tidy(N-1, M), !.

dx(Exp^X, Exp^X*log(e,Exp)^(-1), X) :-
	freeof(X, Exp), !.

dx(log(e,X), X^(-1), X) :- !.

dx(tan(X), sec(X)^2, X) :- !.

dx(cot(X), -1*cosec(X)^2, X) :- !.

dx(sec(X), sec(X)*tan(X), X) :- !.	%  is this a good way to say it?

dx(arcsin(X), (1 + -1*X^2)^(-2 ^ -1), X) :- !.

dx(cosec(X), -1*cos(X)*cosec(X)^2, X) :- !.

dx(arcsin(X), (1 + -1*X^2)^(-2 ^ -1), X):- !.

dx(cosec(X), -1*cos(X)*cosec(X)^2, X):- !.

dx(arctan(X), (1+X^2)^(-1), X):- !.

dx(sin(X), cos(X), X) :- !.

dx(cos(X), -1*sin(X), X) :- !.

dx(A+B, DA+DB, X) :- !, 
	dx(A, DA, X), !,
	dx(B, DB, X).

dx(C*A, C*DA, X) :-
	freeof(X, C),  !,  dx(A, DA, X).

dx(A*C, DA*C, X) :-
	freeof(X, C),  !,  dx(A, DA, X).

dx(A/C, DA/C, X) :-
	freeof(X, C),  !,  dx(A, DA, X).

dx(C/A, -1*C*DA/A^2, X) :-
	freeof(X, C), !, dx(A, DA, X).

dx(A*B, A*DB + B*DA, X) :- !, 
	dx(A, DA, X), !, dx(B, DB, X).

dx(A/B, (B*DA + -1*A*DB)/B^2, X) :- !, 
	dx(A, DA, X), !, dx(B, DB, X).

dx(Exp, Exp1*Arg1, X) :-
	exactly_one_arg(X, Exp, Arg),
	Arg \== X, !,
	gensym(var, T),
	subst(Arg=T, Exp, Mid),		dx(Mid, Mid1, T),
	subst(T=Arg, Mid1, Exp1), !,	dx(Arg, Arg1, X).

%   check that there is exactly one argument of Exp containing Term,
%   and return that argument as Arg.

exactly_one_arg(Term, Exp, Arg) :-
	functor(Exp, _, N),
	exactly_one_arg(N, Term, Exp, Arg).

	exactly_one_arg(0, _Term, _Exp, Ans) :- !, nonvar(Ans).
	exactly_one_arg(N, Term, Exp, Ans) :-
		arg(N, Exp, Arg),
		contains(Term, Arg), !,
		M is N-1, Arg = Ans, !,
		exactly_one_arg(M, Term, Exp, Ans).
	exactly_one_arg(N, Term, Exp, Ans) :-
		M is N-1,
		exactly_one_arg(M, Term, Exp, Ans).

