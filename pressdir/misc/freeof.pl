%   File: FreeOf	Author: R.A.O'Keefe	Updated: 18 August 82

%   This file provides an alternative implementation of freeof/2 and
%   contains/2 which should be faster and use less stack.

:- public freeof/2, contains/2.

:- mode
    contains(+, +),
    freeof(+, +),
	freeof(+, +, -).


contains(Kernel, Expression) :-
	\+ freeof(Kernel, Expression).


freeof(Kernel, Kernel) :- !,
	fail.
freeof(Kernel, Expression) :-
	atomic(Expression), !.	/*
freeof(Kernel, Expression) :-
	number(Expression), !.	*/
freeof(Kernel, Expression) :-
	functor(Expression, _, Arity),
	freeof(Arity, Kernel, Expression).

	freeof(0, Kernel, Expression) :- !.
	freeof(N, Kernel, Expression) :-
		arg(N, Expression, Argument),
		freeof(Kernel, Argument),
		M is N-1, !,
		freeof(M, Kernel, Expression).


