%   File   : WEAKNF
%   Author : Bernard Silver
%   Updated: 23 February 1984
%   Purpose: Weak normal forms for LP

:- public
	weak_normal_form/3,
	mod_weak_normal_form/3,
	mod_weak_normal_form1/4,
	mod_weak_normal_form2/3.

:- mode

	filter(+, +, -, -),
	mod_weak_normal_form(+,+,?),
	mod_weak_normal_form1(+,+,+,?),
	mod_weak_normal_form2(+,+,?),
	weak_normal_form(+, +, -),
	zero_rhs(+, -).

 % Put equation(s) into weak normal form
weak_normal_form(Eqn, Var, New) :-
	tidy_expr(Eqn,Tidy),
	zero_rhs(Tidy, Mid),
	decomp(Mid, [+|Bag]),
	filter(Bag, Var, Lhs, Rhs),
	tidy_expr(Lhs=Rhs, New), !.

weak_normal_form(Eqn, _Var, New=0)  :- zero_rhs(Eqn,New),!.

%   put an equation Lhs=Rhs into the form New=0.

	zero_rhs(Lhs=0, Lhs) :- !.
	zero_rhs(Lhs=Rhs, New) :- tidy(Lhs-Rhs, New).

%   split a sum bag into Lhs, holding all elements containing Var,
%   and Rhs, holding all the elements not containing Var.  We are
%   free to use '-' in Rhs, as it will be tidied before use.

	filter([Head|Tail], Var, Head+More, Rest) :-
		contains(Var, Head), !,
		filter(Tail, Var, More, Rest).
	filter([Head|Tail], Var, More, Rest-Head) :- !,
		filter(Tail, Var, More, Rest).
	filter([],	  _Var, 0,    0).


mod_weak_normal_form(Exp,X,Ans) :-
	mod_weak_normal_form1(Exp,normal,X,Ans),
	(match_check(Exp,Ans);
	writef('\nPutting in weak normal form to obtain\n\n%t\n',[Ans])),
	!.

mod_weak_normal_form(Old,_,Old).

 % m_w_n_f1 can be called on its own if we don't want the message output


mod_weak_normal_form1(A#B,Type,X,New) :- !,
	mod_weak_normal_form1(A,Type,X,A1),
	mod_weak_normal_form1(B,Type,X,B1),
	type_tidy(Type,A1#B1,New).
	
mod_weak_normal_form1(Exp,Type,X,Ans) :-
	type_tidy(Type,Exp,Exp1),
	weak_normal_form(Exp1,X,Ans1),
	type_tidy(Type,Ans1,Ans).


 % Normal case
mod_weak_normal_form2(A=B,X,H1) :-
	contains(X,A),
	!,
	mod_weak_normal_form1(A=B,expr,X,H1).

 % A is an atom and so is probably a new variable.  Call m_w_n_f1 with A
 % as unknown
mod_weak_normal_form2(A=B,_,H1) :-
	atom(A),
	!,
	mod_weak_normal_form1(A=B,expr,A,H1).

 % Whole expression is an atom so return it
mod_weak_normal_form2(A,_,A) :- atomic(A),!.

 % Exp does not contain the old unknown so guess it!
mod_weak_normal_form2(Expr,X,H1) :-
	freeof(X,Expr),
	!,
	wordsin(Expr,Words),
	member(Y,Words),
	!,
	mod_weak_normal_form1(Expr,expr,Y,H1).

 % All other cases, e.g Term is a disjunction
mod_weak_normal_form2(Term,X,New) :-
	mod_weak_normal_form1(Term,expr,X,New).
