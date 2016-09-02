%   File   : COND
%   Author : Bernard Silver
%   Updated: Tue Oct 15 11:38:52 1985
%   Purpose: Conditions for LP

/*
:- public
	closer/3,
	common_subterms/3,
	contains/2,
	contains_nasties/2,
	dis_solution/2,
	dominated/3,
	find_type/3,
	findtype/2,
	freeof/2,
	good_subterm/4,
	identical_subterms/3,
	is_disjunct/2,
	is_mod_poly/3,
	is_product/2,
	is_sum/2,
	less_nasty/3,
	less_occ/3,
	mult_occ/2,
	multiple_offenders_set/3,
	parse/3,
	prod_exp_terms_eqn/3,
	rhs_zero/1,
	same_occ/3,
	single_occ/2,
	trigf/1.

:- mode 
		check_expp(+),
		check_logf(+),
		check_trigf(+),
		closer(+,+,+),
		common_subterms(+,+,?),
		common_subterms_list(+,+,?),
		contains(+, +),
		contains_nasties(+,+),
		contains_nasties(+,+,-),
		dis_solution(+,+),
		dl_parse(+,?,+),
		dominated(+,+,?),
		dominatable_function(+),
		dominated1(+,+,+,+,?),
		exp_term(+,+),
		find_type(+,+,?),
		findtype(-,+),
		freeof(+, +),
		freeof(+, +, -),
		good_subterm(+, +, +, -),
		good_subterm(+, -),
		good_subterm(+, +, -),
		identical_subterms(+, +, -),
		is_disjunct(+,+),
		is_mod_poly(+,+,?),
		is_product(+,+),
		is_sum(+,+),
		less_nasty(+,+,+),
		less_nasty1(+,+,+),
		less_occ(+,+,+),
		mult_occ(+,+),
		multiple_offenders_set(+,?,+),
		parse(+,-,+),
		prod_exp_terms(+,+),
		prod_exp_terms_eqn(+,+,-),
		rhs_zero(+),
		same_occ(+,+,+),
		single_occ(+,+),
		trigf(+).

*/
%   Occurrence clauses.
mult_occ(Term,Exp)	:- occ(Term, Exp, N), N > 1, !.

single_occ(X,Eqn) :- occ(X,Eqn,1).

%   This provides an alternative implementation of freeof/2 and
%   contains/2 which should be faster and use less stack.

contains(Kernel, Expression) :-
	\+ freeof(Kernel, Expression).

freeof(Kernel, Kernel) :- !,
	fail.
freeof(_Kernel, Expression) :-
	simple(Expression), !.
freeof(Kernel, Expression) :-
	functor(Expression, _, Arity), !,
	freeof(Arity, Kernel, Expression).

	freeof(0, _Kernel, _Expression) :- !.
	freeof(N, Kernel, Expression) :-
		arg(N, Expression, Argument),
		freeof(Kernel, Argument),
		M is N-1, !,
		freeof(M, Kernel, Expression).

same_occ(X,Old,New) :-
	occ(X,Old,M),
	occ(X,New,N),
	!,
	M = N.

less_occ(X,Old,New) :-
	occ(X,Old,M),
	occ(X,New,N),
	!,
	M > N.

 % Pattern Matching
rhs_zero(_=0).

is_product(X,A*B=_) :-
	contains(X,A),
	contains(X,B).

is_disjunct(X,A#B) :-
	contains(X,A),
	contains(X,B),
	!.
	
is_sum(X,A+B=_) :-
	contains(X,A),
	contains(X,B),
	!.

dis_solution(X=A,X) :- !,
	atomic(X),
	freeof(X,A).

dis_solution(A#B,X) :- !,
	dis_solution(A,X),
	dis_solution(B,X).
dis_solution(true,_) :- !.
dis_solution(false,_) :- !.

 % For F.P.
common_subterms(X,Term=_,Subterm) :- !,
	common_subterms(X,Term,Subterm).
common_subterms(X,Term,Subterm) :-
	decomp(Term,[+|List]),
	common_subterms_list(X,List,Subterm),
	!.

common_subterms_list(_,[],_).
common_subterms_list(X,[H|T],Subterm) :-
	subterms2(H,X,Subterm),
	common_subterms_list(X,T,Subterm).

 % For Attraction
closer(X,A,B) :- 
	closeness(X,A,C1),
	closeness(X,B,C2),
	!,
	C2<C1.


 % Function Stripping
dominated(X,L=_R,[1|Posn]) :-
	mult_occ(X,L),
	functor(L,F,N),
	L =..[F|Args],
	dominated1(N,F,Args,X,Posn),
	!.


dominated1(2,F,[A,B],X,[P]) :- 
	dominatable_function(F),
	!,
	((contains(X,A),freeof(X,B),P=1);
	(contains(X,B),freeof(X,A),P=2)),
	!.
dominated1(1,_,_,_,[1]) :- !.

dominatable_function(*) :- !.
dominatable_function(log) :- !.
dominatable_function(^) :- !.

 % Defn for Nasty Function Preconditions

contains_nasties(X,Eqn) :-
	contains_nasties(X,Eqn,_),
	!.

less_nasty(X,Old,New) :-
	contains_nasties(X,Old,Nasty),
	less_nasty1(X,New,Nasty),
	!.


contains_nasties(X,Eqn,V) :-
	call(parse4(Eqn,X,U,other)),
	call(subnasty(X,U,V)),
	V \== [],
	!.

contains_nasties(X,Eqn,V) :-
	call(parse4(Eqn,X,U,neg)),
	call(exp_nasty_list(X,U,V)),
	V \== [],
	!.

 % New is less nasty than Old, given Old contains Nasties, if either
 % New contain nasties, but less than Old (first clause), or New doesn't
 % contain nasties (second clause).

less_nasty1(X,Eqn,N) :-
	contains_nasties(X,Eqn,Set),
	!,
	length(Set,L1),
	length(N,L2),
	L2 > L1.

less_nasty1(_,_,_).

 % Recognize poly if equation is not in weak normal form
is_mod_poly(X,Pol,A=B) :- 
	weak_normal_form(Pol,X,Eqn),
	!,
	Eqn = (A=B),
	is_poly(X,A).

% For Change of Unknown
hard_identical_subterms(Eqn,X,Term) :-
	hard_identical_subterms(Eqn,X,Term,_New).
hard_identical_subterms(Eqn,X,Term,Eqn) :-
	identical_subterms(Eqn,X,Term,_),
	!.
hard_identical_subterms(Exp=A,X,Subst,New) :-
	occ(X,Exp,N),
	N > 1,
	bagof(Term,good_subterm(Exp,Term),Set),
	setof(List,match_subterm_set(Set,X,N,List),GoodList),
	find_best_term(GoodList,Subst,Exp=A,New),
	!.

% Last arg only for compatibility with test above
identical_subterms(Exp,X,Term,Exp) :-
	identical_subterms(Exp,X,Term).
identical_subterms(Lhs=_Rhs, Var, Term) :-
	occ(Var, Lhs, N), N > 1,
	setof(Term, good_subterm(Lhs, Var, N, Term), TermSet),
	extreme_term(TermSet, >, Term),!.




good_subterm(Exp, Var, N, Term) :-
	good_subterm(Exp, Term),
	occ(Var, Term, M), M > 0,
	occ(Term, Exp, L), L > 1,
	N is L*M.
	
	%   good_subterm(Term, Exp) is true when Term is a non-atomic subterm
	%   of Exp.  This enables us to drop the "Term \= Var" requirement in 
	%   good_subterm/4.

	good_subterm(Exp, _Term) :-
		(   atomic(Exp) ; ok_number(Exp)   ), !, fail.
	good_subterm(Exp, Term) :-
		functor(Exp, _, N),
		good_subterm(N, Exp, Term).

	    %   good_subterm(N,E,T) <- T is a good subterm of Exp's Nth argument

		good_subterm(0, Exp, Term) :- !, Term = Exp.
		good_subterm(N, Exp, Term) :-
			arg(N, Exp, Arg),
			good_subterm(Arg, Term).
		good_subterm(N, Exp, Term) :-
			M is N-1, !,
			good_subterm(M, Exp, Term).


 % Type of equation (for schema methods)
find_type(_,_&_,conj) :- !.
find_type(_,_#_,disj) :- !.
find_type(X,Eqn,Type) :-
	parse(Eqn,Set,X),
	findtype(Type,Set).

parse(Exp,Set,Unk) :- dl_parse(Exp,Set1-[],Unk),listtoset(Set1,Set).

dl_parse(Var,L-L,_) :- var(Var),!.
dl_parse(A=_,L,Unk) :- !,dl_parse(A,L,Unk).
dl_parse(A#B,L-L1,Unk) :- !,dl_parse(A,L-L2,Unk),dl_parse(B,L2-L1,Unk).
dl_parse(A+B,L-L1,Unk) :- !,dl_parse(A,L-L2,Unk),dl_parse(B,L2-L1,Unk).
dl_parse(A*B,L-L1,Unk) :- !,dl_parse(A,L-L2,Unk),dl_parse(B,L2-L1,Unk).
dl_parse(Number,L-L,_) :- ok_number(Number),!.
dl_parse(A^B,L,Unk) :- ok_number(B), !,dl_parse(A,L,Unk).
dl_parse(Unk,[Unk|L]-L,New) :- New==Unk,!.
dl_parse(Atom,L-L,_) :- atom(Atom),!.
dl_parse(A,L-L,Unk) :- nonvar(Unk),freeof(Unk,A),!.
dl_parse(A,[A|L]-L,_) :- !.

findtype(poly,[x]) :- !.
findtype(trig,L) :- check_trigf(L),!.
findtype(log(_),L) :- check_logf(L),!.
findtype(exp,L) :- check_expp(L),!.

findtype(mixed,_).



check_trigf([]) :- !.
check_trigf([H|T]) :- trigf(H),check_trigf(T),!.


check_logf([]) :- !.
check_logf([log(_,_)|T]) :- check_logf(T).

check_expp([]) :- !.
check_expp([_^_|T]) :- check_expp(T),!.

trigf(X) :- memberchk(X,[sin(_),cos(_),tan(_),sec(_),cosec(_),cot(_)]).

 % Equation can have Homogenization applied to it
multiple_offenders_set(X,Off,Eqn=_) :-
	parse(Eqn,Off,X),
	length(Off,N),
	!,
	N>1.

 % Log method Precondition

prod_exp_terms_eqn(A+B=0,X,New) :-
	!,
	prod_exp_terms(A,X),
	prod_exp_terms(B,X),
	form_new_equation(A+B=0,New).

prod_exp_terms_eqn(A=B,X,A=B) :- 
	prod_exp_terms(A,X),
	prod_exp_terms(B,X).

	%  Describing the allowable multiplicative forms for Log Method

prod_exp_terms(A*B,X) :-
	!,
	exp_term(B,X),		% Note the precedence of *
	prod_exp_terms(A,X).

prod_exp_terms(A,X) :- exp_term(A,X).

exp_term(A,X) :- freeof(X,A), !.
exp_term(A^_,X) :- freeof(X,A).

% Change of unknown stuff
match_subterm_set(Set,X,N,term(T1,[T1|List])) :-
	member(Term,Set),
	occ(X,Term,M),
	M < N,
	eval(N/M,Int),
	integer(Int),
	delete_one(Term,Set,NewSet),
	m_s_s(NewSet,1,Int,Term,[Term],[T1|List]).

m_s_s(_,N,N,_,List,List) :- !.
m_s_s([H|List],Acc,N,Term,LAcc,NList) :-
	match(H,Term),
	NewAcc is Acc+1,
	!,
	m_s_s(List,NewAcc,N,Term,[H|LAcc],NList).
m_s_s([_|List],Acc,N,Term,LAcc,NList) :-
	m_s_s(List,Acc,N,Term,LAcc,NList).

find_best_term(List,Subst,Old,New) :-
	make_list_in_form(List,NewList,Rest),
	extreme_term(NewList,>,Subst),
	nmember(Subst,NewList,N),
	nmember(Others,Rest,N),
	list_subst(Others,Subst,Old,New),
	!.

make_list_in_form([],[],[]).
make_list_in_form([term(A,B)|T],[A|T1],[B|T2]) :-
	make_list_in_form(T,T1,T2).

list_subst([],_,New,New) :- !.
list_subst([H|T],H,Old,New) :- !,
	list_subst(T,H,Old,New).
list_subst([H|T],Subst,Old,New) :-
	subst(H=Subst,Old,Mid),
	!,
	list_subst(T,Subst,Mid,New).
