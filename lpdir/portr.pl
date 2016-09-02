%   File   :  /usr/bs/lpdir/portr.pl
%   Author : Richard+Lawrence
%   Updated: Tue Oct 15 11:51:39 1985
%   Purpose: First stab at a general all level portray handler.

/*
	This was Richard's code for his rational stuff.
	Eventually I must fix these problems by having the 'print'
	routine in the interpreter actually descend level by level
	taking operators into account and calling portray at each
	level to see whether the users wants to handle it.
	NB: this has now been done.  Why is gportr still around?
	(To get prettier effects)

	The following magic numbers appear in put(N) calls:
	32 = space, 40 = "(", 41 = ")", 44 = ",", 91 = "[", 93 = "]".
	The magic number 1000 also appears; this is the priority of ','.

*/


/* EXPORT */

:- public
    portray/1.


/* MODES */

:- mode
    portray(?),
	prin(+, +),
	    prin(+, +, +),
	    prnf(+, +, +),
	    prna(+, +, +),
	    prnp(+, +, +, +),
	    printail(+),
	    oper(+, ?, ?),
		oper(+, +, ?, ?).



			% Top level

portray(Term) :-
	prin(1000, Term).



			% Print a term taking account of surrounding
			%  operator priorities.

	prin(Prio, Term) :-
		(   var(Term)		%  _N style of variables
		|   atom(Term)		%  ordinary atoms
		|   number(Term)	% numbers
		|   Term = '$VAR'(N)	%  A1 style of variables from numbervars
		),  !,
		writeq(Term).		%  quotes around e.g. 'foo baz'
	prin(Prio, Term) :- /*Q'*/
		portray_number(Term),	%  if a number
		!.


	/*  Other user-provided portrayal methods should be called here  */
	prin(Prio, [Head|Tail]) :- !,	%  list
		put(91),		%  "["
		prin(1000, Head),
		printail(Tail).
	prin(Prio, Term) :-		%  postfix operator
		functor(Term, Functor, 1),
		oper(Functor, Lp, 0), !,
		prnp(Prio, Lp, 0, 40),
  		prna(Lp, Term, 1),
		prnf(Functor, 0, 1),
		prnp(Prio, Lp, 0, 41).
	prin(Prio, Term) :-		%  prefix operator
		functor(Term, Functor, 1),
		oper(Functor, 0, Rp), !,
		prnp(Prio, 0, Rp, 40),
		prnf(Functor, 1, 0),
		prna(Rp, Term, 1),
		prnp(Prio, 0, Rp, 41).
	prin(Prio, Term) :-		%  infix operator
		functor(Term, Functor, 2),
		oper(Functor, Lp, Rp),
		Lp > 0, Rp > 0, !,
		prnp(Prio, Lp, Rp, 40),
		prna(Lp, Term, 1),
		prnf(Functor, 0, 0),
		prna(Rp, Term, 2),
		prnp(Prio, Lp, Rp, 41).
	prin(Prio, Term) :-
		functor(Term, Functor, N),
		writeq(Functor),
		prin(0, N, Term).


					% print one argument of a term

		prna(Prio, Term, ArgNo) :-
			arg(ArgNo, Term, Arg),
			prin(Prio, Arg).

					% print a functor with spaces

		prnf(',', _, _) :- !,
			write(', ').
		prnf(';', _, _) :- !,
			write('; ').
		prnf('#',L,R) :- !, % For LP disjunction
			prnp(L, 1, 1, 32),
			write(' v '),
			prnp(R, 1, 1, 32).
		prnf(Functor, L, R) :-
			prnp(L, 1, 1, 32),
			write(Functor),
			prnp(R, 1, 1, 32).

					% print the arguments of a term

		prin(0, N, Term) :-
			put(40),		%  "("
			prna(1000, Term, 1),
			prin(1, N, Term).
		prin(N, N, Term) :- !,
			put(41).		%  ")"
		prin(L, N, Term) :-
			M is L+1,
			write(', '),
			prna(1000, Term, M), !,
			prin(M, N, Term).
		

					% Print a parenthesis if the priorities
					%  around the operator require it.

		prnp(Prio, Lp, Rp, Char) :-
			Prio >= Lp, Prio >= Rp, !.
		prnp(Prio, Lp, Rp, Char) :-
			put(Char).


					% Print the tail of a list, being
					%  careful about partial instantiation
					%  at the end.

		printail(List) :-
			nonvar(List), List = [Head|Tail], !,
			write(', '),
			prin(1000, Head), !,
			printail(Tail).
		printail(Tail) :-
			Tail \== [],
			put(124),		%  "|"
			prin(1000, Tail), !,
			printail([]).
		printail([]) :-
			put(93).		%  "]"



			% Check for operators.  Return left and right
			%  precedences. These are Richard's conventions.
			%  Note that prefix/postfix ops have 0 for their
			%  other precedence.

oper(Op, Left,Right) :-
	current_op(Prec, Type, Op),
	oper(Type, Prec, Left, Right).


	oper( fx, Prec, 0, Prec).
	oper( fy, Prec, 0, Prec).
	oper(xf , Prec, Prec, 0).
	oper(yf , Prec, Prec, 0).
	oper(xfx, Prec, Prec, Prec).
	oper(xfy, Prec, Prec, More) :- More is Prec+1.
	oper(yfx, Prec, More, Prec) :- More is Prec+1.


