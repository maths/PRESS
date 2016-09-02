%   File   : PRESS:MATCH
%   Author : Press Group
%   Updated: 14 March 82
%   Purpose: Pattern Matcher for associative commutative functions.

:- public
	corresponding_arguments/4,	%   (replaces any1)
	decomp/2,
	match/2,
	recomp/2,
	ac_op/5.

:- mode
	corresponding_arguments(+, -, -, -),
	decomp(+, ?),
	    ac_decomp(+, +, ?, ?),
	    ac_op(+, ?, ?, ?, -),
	recomp(?, +),
	    ac_recomp(+, +, ?),
	match(+, ?),
	    match_arguments(+, +, +),
	    split_two_ways(+, ?, ?).


%   replace OldA by NewA in one element of Old, giving New.

corresponding_arguments([OldA|Tail], OldA, [NewA|Tail], NewA).
corresponding_arguments([Head|Tail], OldA, [Head|Rest], NewA) :-
	corresponding_arguments(Tail, OldA, Rest, NewA).


%------------------------------------------------------------------------%

%   decomp(Term, List) and recomp(Term, List) are generalisations of univ,
%   i.e. Term =.. List, treating the four known associative commutative
%   operators as function symbols having any number of arguments.

%   They are called in the patterns
%	decomp(Old, [Op|Olds]),		%   var(Op)
%	any1(<foo>, Olds, News),
%	recomp(New, [Op|News]),
%   in collect and attract, and elsewhere in the form
%   	decomp(Old, [+|_])	trig_fac,multiply_through,weaknf
%	recomp(New, [+|_])	make_poly.

%   ac_op(Op, X, Y, X Op Y, Idn) means that Op is known to be a commutative
%   associative operator, that X Op Y =.. [Op,X,Y], and that Idn Op X = X
%   i.e. Idn is the identity of Op.  All four operators have an identity.
%   The fifth clause is a hack for 1/(X*Y), but is still true.

ac_op(+, X, Y, X+Y, 0)     :- !.
ac_op(*, X, Y, Y*X, 1)     :- !.	%   note reversal!
ac_op(&, X, Y, X&Y, true)  :- !.	%   conjunction
ac_op(#, X, Y, X#Y, false) :- !.	%   disjunction
%%ac_op(*, X^N, Y^N, (Y*X)^N, 1) :- !.


decomp(Term, [Op|Args]) :-
	functor(Term, Op, 2),
	ac_op(Op, _, _, _, _), !,
	ac_decomp(Term, Op, Args, []).
%%decomp((X*Y)^(-1), [*|Args]) :-		%   special hack
%%	ac_decomp((X*Y)^(-1), *, Args, []).
decomp(Term, List) :-
	Term =.. List.


	ac_decomp(Term, Op, [Term|R], R) :-
		var(Term), !.
	ac_decomp(Term, Op, L, R) :-
		ac_op(Op, X, Y, Term, _), !,
		ac_decomp(X, Op, L, M), !,
		ac_decomp(Y, Op, M, R).
	ac_decomp(Term, Op, [Term|R], R).



recomp(Term, [Op|Args]) :-
	ac_op(Op, _, _, _, _), !,
	ac_recomp(Args, Op, Term).
recomp(Term, List) :-
	Term =.. List.

	ac_recomp([[]|Args], Op, Term) :- !,
		ac_recomp(Args, Op, Term).
	ac_recomp([Exp], Op, Term) :- !,
		Term = Exp.
	ac_recomp([Exp|Args], Op, Term) :-
		ac_op(Op, Exp, Mid, Term, _), !,
		ac_recomp(Args, Op, Mid).
	ac_recomp([], Op, Term) :-
		ac_op(Op, _, _, _, Term).


%------------------------------------------------------------------------%

%   match two terms, using the associativity and commutativity of + and *.

match(Lhs, Rhs) :-
	functor(Lhs, Op, 2),
	ac_op(Op, Arg1, Arg2, Rhs, _), !,
	decomp(Lhs, [Op|Olds]), !,
	split_two_ways(Olds, [C1|Cs1], [C2|Cs2]),
	recomp(D1, [Op,C1|Cs1]),
	recomp(D2, [Op,C2|Cs2]),
	match(D1, Arg1),
	match(D2, Arg2).

match(Lhs, Lhs) :-		%   atoms match themselves
	atomic(Lhs), !.

match(Neg, -1*Pos) :-		%   hack round the representation of
	ok_number(Neg),		%   negative numbers
	eval(Neg < 0),		%   rationals are around now!
	eval(-Neg, Pos), !.
match(-1*Pos, Neg) :-		%  can't happen if Lhs is tidied first
	ok_number(Neg),
	eval(Neg < 0),
	eval(-Neg, Pos), !.
match(Lhs, Rhs) :-
	functor(Lhs, Functor, Arity),
	functor(Rhs, Functor, Arity), !,
	match_arguments(Arity, Lhs, Rhs).


	match_arguments(0, Lhs, Rhs) :- !.
	match_arguments(N, Lhs, Rhs) :-
		arg(N, Lhs, LhsNth),
		arg(N, Rhs, RhsNth),
		match(LhsNth, RhsNth),
		M is N-1,
		match_arguments(M, Lhs, Rhs).


	split_two_ways([Head|Tail], A, B) :-
		split_two_ways(Tail, A1, B1),
		(   A = [Head|A1], B = B1
		;   B = [Head|B1], A = A1
		).
	split_two_ways([], [], []).

%   Given a Term, discover all the constants, atoms, and functors occuring in
%   it.  The Term is known to be ground.  Special code for matching for Leon.
/*
functors_in(Term, List) :-
	functors_in(Term, L, []),
	sort(L, List).

	functors_in(Term, [Term|R], R) :-
		atom(Term), !.
	functors_in(Term, [Abso|R], R) :-
		number(Term), !,
		eval(abs(Term), Abso).
	functors_in(Term, [Head|L], R) :-
		functor(Term, Functor, Arity),
		functor(Head, Functor, Arity), !,
		functors_in(Arity, Term, L, R).

		functors_in(0, Term, R, R) :- !.
		functors_in(N, Term, L, R) :-
			arg(N, Term, Argument),
			functors_in(Argument, L, M),
			K is N-1, !,
			functors_in(K, Term, M, R).
*/
