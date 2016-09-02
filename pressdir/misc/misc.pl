%   Press:Misc.				Updated: 24 August 82
%   Basic utilities for Press.  Written by Alan Bundy 31.8.80.
%   additional routines by Leon Sterling, Richard O'Keefe, and Bernard Silver

%   flag(tflag,_,1)  has been moved to  Press:Filin.

% :- public
% 	andtodot/2,		%  X&Y -> [X|Y']
% 	arbint/1,		%  -> GenSym {flagged as integer}
% 	cond_print/3,		%  if Old and New differ, print New.
% %	contains/2,		%  SubTerm in Term ?
% %	correspond/4,		%  X,Xlist,Ylist,Y  X,Y at similar places
% %	delete/3,		%  delete term from list to get new list
% 	dottoand/2,		%  [X|Y] -> X&Y'
% 	dottoor/2,		%  [X|Y] -> X#Y'
% 	extreme_term/3,		%  from List pick smallest(<)|biggest(>) Term
% 	fixvar/2,		%  Exp -> Vbl where Vbl is ok for isolation
% %	freeof/2,		%  SubTerm not in Term?
% 	identifier/1,		%  -> GenSym {any old intermediate}
% 	least_dom/2,		%  SubTerm has Term as least dominating term?
% 	mult_occ/2,		%  SubTerm in Term more than once
% 	ok/1,			%  does it make sense to solve for X ?
% 	ortodot/2,		%  X#Y -> [X|Y']
% %	position/3,		%  Term,Exp -> Path
% 	single_occ/2,		%  SubTerm in Term exactly once?
% 	subst_mesg/3.		%  substitution with trace_press
% 
% %   Predicates to convert between conjunctions/disjunctions and lists.
% %   The routines binary_to_list and list_to_binary might be useful elsewhere.
% 
% :- mode
% 	andtodot(+,-),			%   Conjunction -> List
% 	binary_to_list(+,+,+,?,?),	%   Term,Operator,Unit -> DiffList
% 	dottoand(+,-),			%   List -> Conjunction
% 	dottoor(+,-),			%   List -> Disjunction
% 	list_to_binary(+,+,-),		%   List,Operator -> Term
% 	ortodot(+,-).			%   Disjunction -> List


:- dynamic integral/1.

andtodot(Term, List) :-
	binary_to_list(Term, &, true, List, []).

ortodot(Term, List) :-
	binary_to_list(Term, #, false, List, []).

	binary_to_list(Nil, _, Nil, List, List) :- !.
	binary_to_list(Term, Op, Nil, Head, Tail) :-
		Term =.. [Op, Arg1, Arg2],
		binary_to_list(Arg1, Op, Nil, Head, Middle), !,
		binary_to_list(Arg2, Op, Nil, Middle, Tail).
	binary_to_list(Term, _, _, [Term|Tail], Tail).


dottoand([], true) :- !.
dottoand(List, Term) :-
	list_to_binary(List, &, Term).

dottoor([], false) :- !.
dottoor(List, Term) :-
	list_to_binary(List, #, Term).

	list_to_binary([Term], _, Term) :- !.
	list_to_binary([Head|Tail], Op, Answer) :-
		Answer =.. [Op,Head,Rest], !,
		list_to_binary(Tail, Op, Rest).



%   Occurrence clauses.  The routine occ/3 is defined in STRUCT.PL.
%   freeof(K,E) :- occ(K,E,0) is certainly a good definition of the
%   meaning of freeof, but the definition here is faster and uses
%   less stack.  A similar improvement is possible for single_occ
%   and mult_occ, but there is less to be gained from them.

% :- mode
% %	contains(+,+),			%  Kernel in Expression ?
% %	freeof(+,+),			%  Kernel not in Expression ?
% %	freeof(+,+,+),			%  Arity,Kernel,Expression ?
% 	mult_occ(+,+),			%  mult_occ with args swapped
% 	single_occ(+,+).		%  Kernel in Expression once ?


single_occ(Kernel, Expression) :-
	occ(Kernel, Expression, 1).

mult_occ(Kernel, Expression) :-		%  arguments right way round
	occ(Kernel, Expression, N), !, N > 1.

/*  Defined elsewhere
contains(Kernel, Expression) :-
	\+ freeof(Kernel, Expression).


freeof(Kernel, Kernel) :- !,
	fail.
freeof(Kernel, Expression) :-
	simple(Expression), !.
freeof(Kernel, Expression) :-
	functor(Expression, _, Arity), !,
	freeof(Arity, Kernel, Expression).

	freeof(0, Kernel, Expression) :- !.
	freeof(N, Kernel, Expression) :-
		arg(N, Expression, Argument),
		freeof(Kernel, Argument),
		M is N-1, !,
		freeof(M, Kernel, Expression).

*/


%   test whether Exp is a least dominating expression of Term, i.e.
%   whether Exp contains at least two occurrences of Term directly.

% :- mode
% 	at_least_occ(+,+,+),		%  List has Term >= Limit times?
% 	com_ass_idn(+,-),		%  Operator -> Identity
% 	least_dom(+,+),			%  Kernel,Expression ?
% 	least_dom(+,+,+,+).


%   at_least_occ(List, Term, Limit) is true when List contains at least
%   Limit (>= 0) elements which contain Term.  This is NOT the same as
%   occ(List,Term,N) & N >= Limit, as several instances can be in 1 element.

at_least_occ(_, _, 0) :- !.
at_least_occ([Head|Tail], Term, Limit) :-
	contains(Term, Head),
	Mimit is Limit-1, !,
	at_least_occ(Tail, Term, Mimit).
at_least_occ([_|Tail], Term, Limit) :-
	at_least_occ(Tail, Term, Limit).


%   com_ass_idn(Op,Id) -> Op is a commutative associative operator
%   with identity element Id.  This is a makeshift for keeping the
%   arguments of such operators as bags.

	com_ass_idn((+), 0).		com_ass_idn(*, 1).
	com_ass_idn(&, true).		com_ass_idn(#, false).


least_dom(Term, Exp) :-
	functor(Exp, Op, 2),
	com_ass_idn(Op, Unit),
	binary_to_list(Exp, Op, Unit, List, []), !,
	at_least_occ(List, Term, 2).
least_dom(Term, Exp) :-
	functor(Exp, _, N),
	least_dom(N, 0, Term, Exp).

	least_dom(_N, 2, _Term, _Exp) :- !.
	least_dom(0, _K, _Term, _Exp) :- !, fail.
	least_dom(N, K, Term, Exp) :-
		arg(N, Exp, Arg),
		contains(Term, Arg),
		M is N-1, L is K+1, !,
		least_dom(M, L, Term, Exp).
	least_dom(N, K, Term, Exp) :-
		M is N-1, !,
		least_dom(M, K, Term, Exp).


/*
%   position(Term, Exp, Path) is true when Term occurs in Exp at the
%   position defined by Path.  It may be at other places too, so the
%   predicate is prepared to generate them all.

% :- mode
% 	position(?,+,?),		%  Term,Exp -> Path
% 	position(+,?,+,?).		%  ArgNo,Term,Exp -> Path


position(Term, Term, []).
position(Term, Exp, Path) :-
	(   var(Exp) ; atomic(Exp) ; ok_number(Exp)   ), !, fail.
position(Term, Exp, Path) :-
	functor(Exp, _, N),
	position(N, Term, Exp, Path).

	position(0, Term, Exp, Path) :- !, fail.
	position(N, Term, Exp, [N|Path]) :-
		arg(N, Exp, Arg),
		position(Term, Arg, Path).
	position(N, Term, Exp, Path) :-
		M is N-1, !,
		position(M, Term, Exp, Path).
*/
 
%   Find the smallest (if C = <) or greatest (if C = >) term in a list of
%   terms, where comparison is by the size of a term.

% :- mode
% 	extreme_term(+,+,-),
% 	extreme_term(+,+,+,+,-),
% 	term_size(+,-),
% 	term_size(+,+,+,-).


extreme_term([Head|Tail], C, Term) :-
	term_size(Head, Size), !,
	extreme_term(Tail, Head, Size, C, Term).

	extreme_term([Head|Tail], _Hold, Sold, C, Term) :-
		term_size(Head, Size),
		compare(C, Size, Sold), !,
		extreme_term(Tail, Head, Size, C, Term).
	extreme_term([_Head|Tail], Hold, Sold, C, Term) :- !,
		extreme_term(Tail, Hold, Sold, C, Term).
	extreme_term([],	  Term, _,    _, Term).

	term_size(Term, 1) :-
		(   var(Term) ; atomic(Term) ; ok_number(Term)   ), !.
	term_size(Term, Size) :-
		functor(Term, _, N), !,
		term_size(N, Term, 1, Size).

		term_size(0, _Exp, Ans, Ans) :- !.
		term_size(N, Exp, Acc, Ans) :-
			arg(N, Exp, Arg),
			term_size(Arg, Size),
			Nxt is Acc+Size+1, M is N-1, !,
			term_size(M, Exp, Nxt, Ans).


%   generate intermediate variables, or arbitray integer tokens.

% :- mode
% 	arbint(-),
% 	identifier(-).

arbint(Var) :-
	gensym(n, Var),
	assert(integral(Var)),
	trace_press('\n\tLetting %t denote an arbitrary integer', [Var], 1), !.

identifier(Var) :-
	gensym(x, Var), !.
%	assert(intermediate(Var)) for MECHO info.

/*  Code needed for running MECHO output (goes in process_input)
%   fix the variable to be isolated if it has not already been fixed.

fixvar(Exp, Var) :-
	var(Var),		%  why not drop this test?
	wordsin(Exp, Words),
	member(Var, Words),
	ok(Var),
	checkand(contains(Var), Exp), !.
fixvar(Exp, Var) :-
	nonvar(Var).


ok(Var) :-
	\+ call(const(Var)),
	(  call(sought(Var))
	;  call(given(Var))
	), !.

*/

/*
%   correspond(X, Xlist, Ylist, Y) is true when the position of X and Xlist
%   and the position of Y in Ylist (which is as long as Xlist) are the same.

:-  mode  correspond(?, +, +, ?).	%  the lists must be given

correspond(X, [X|_], [Y|_], Y) :- !.
correspond(X, [_|T], [_|U], Y) :-
	correspond(X, T, U, Y).

*/
%   cond_print(Old,New,Infer) prints New unless it matches Old.

% :-  mode  cond_print(+, +, -).

cond_print(Old, New, nil) :-
	match(Old, New), !.
cond_print(_Old, New, tidy(New)) :-
	trace_press('\nTidying to %t\n', [New], 1).


%   apply a substitution, tidy the result, and print a message.

% :-  mode  subst_mesg(+, +, -).

subst_mesg(Substitution, Old, New) :-
	subst(Substitution, Old, Mid),
	tidy(Mid, New),
	trace_press('\nApplying substitution %c\n   to    : %c\n   gives : %c\n',
		[Substitution, Old, New], 1), !.

/*

:- mode
	delete(+,+,-).			%  List,Drop -> Depleted

delete([], _, []) :- !.
delete([Kill|Tail], Kill, Rest) :- !,
	delete(Tail, Kill, Rest).
delete([Head|Tail], Kill, [Head|Rest]) :- !,
	delete(Tail, Kill, Rest).



*/

