%   Press:Weaknf.			Updated: 10 September 82
%   					Author:  Bernard Silver 28.4.81
%   Put expression into weak normal form for collection, attraction, &c.

% :- public weak_normal_form/3,
% 	zero_rhs/2,
% 	filter/4.
% :- mode  
%     weak_normal_form(+, +, -),
% 	zero_rhs(+, -),
% 	filter(+, +, -, -).

weak_normal_form(Eqn, Var, New) :-
	zero_rhs(Eqn, Mid),
	decomp(Mid, [+|Bag]),
	filter(Bag, Var, Lhs, Rhs),
	tidy(Lhs=Rhs, New), !.

weak_normal_form(Eqn, _Var, New=0)  :- zero_rhs(Eqn,New),!.

weak_normal_form(Exp,_X,Exp).  % Hack default for inequalities

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


