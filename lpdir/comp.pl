%   File   :  /usr/bs/lpdir/comp.pl
%   Author : Bernard Silver
%   Updated: Tue Oct 15 11:36:20 1985
%   Purpose: Misc code for compilation

% Includes code by Alan, Lawrence, Leon and Richard


:- dynamic 
	integral/1,
	unknown/1.



 % Delete first occ of H from second arg to get third.
delete_one(_,[],[]).
delete_one(H,[H|T],T) :- !.
delete_one(H,[X|T],[X|Ans]) :- delete_one(H,T,Ans).

subterms(Term,X,Sub) :- 
	Term=..[_|Args],
	member(Sub1,Args),
	\+ atomic(Sub1),
	contains(X,Sub1),
	subterms(Sub1,X,Sub).

subterms(Term,X,Term1) :- Term \= X,contains(X,Term),Term1=Term.

subterms2(A*B,X,Sub) :-
	decomp(A*B,[*|List]),
	!,
	subterms2a(X,List,Sub).
subterms2(A,X,A) :- contains(X,A).
subterms2a(X,List,El) :-
	select(El,List,_),
	contains(X,El).
subterms2a(X,List,Term)	 :-
	select(_,List,Rest),
	recomp(Term,[*|Rest]),
	contains(X,Term).


match_member(X,[Y|_],Y) :- functor(X,F,N),functor(Y,F,N),match_check(Y,X).
match_member(X,[_|T],Z) :- match_member(X,T,Z).

closeness(X,Exp,Arcs) :-
	tree_size(X,Exp,Nodes), 
	Arcs is Nodes - 1.


tree_size(X, X, 1) :- !.
tree_size(_X, Exp, 0) :-
	atomic(Exp), !.
tree_size(X,-1*A,Size) :- !,
	tree_size(X,A,Size).
tree_size(X, Exp, Size) :-
	functor(Exp, _, N),			%  cut not needed after all
	tree_size(N, Exp, X, 0, Size).

tree_size(0, _Exp, _X, 0, 0) :- !.		%  X doesn't occur in Exp
tree_size(0, _Exp, _X, M, N) :- !,		%  X does occur in Exp,
	N is M+1.				%  so count Exp node too.
tree_size(N, Exp, X, Acc, Size) :-
	arg(N, Exp, Arg),
	tree_size(X, Arg, ArgSize),
	NewAcc is Acc+ArgSize,
	M is N-1, !,
	tree_size(M, Exp, X, NewAcc, Size).

old_attract(X,Old,New1) :-
	closeness(X,Old,Closeness),
	mult_occ(X,Old),
	least_dom(X,Old), 
	attrax(U & V,Template,Rewrite,Cond), % Assumes attraction between 2
	applicable(Template,Old,Rest),	% subterms only
	contains(X,U),
	contains(X,V),
	!,
	newform(Old,Rewrite,Rest,New),
	check_cond(Cond),
	tidy(New,New1),
	closeness(X,New1,NewC),
	NewC < Closeness,
	!.

old_attract(X,Old,New) :- 
	mult_occ(X,Old),
	decomp(Old,[Fun|Args]),
	corresponding_arguments(Args,Arg,NewArgs,NewArg),
	old_attract(X,Arg,NewArg),
	recomp(New,[Fun|NewArgs]).

dottoand([],true) :-!.
dottoand([Head|Tail], Head & Rest) :-
	dottoand(Tail, Rest).



	binary_to_list(Nil, _, Nil, List, List) :- !.
	binary_to_list(Term, Op, Nil, Head, Tail) :-
		Term =.. [Op, Arg1, Arg2],
		binary_to_list(Arg1, Op, Nil, Head, Middle), !,
		binary_to_list(Arg2, Op, Nil, Middle, Tail).
	binary_to_list(Term, _, _, [Term|Tail], Tail).


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

	com_ass_idn(+, 0).		com_ass_idn(*, 1).

least_dom(Term, Exp) :-
	functor(Exp, Op, 2),
	com_ass_idn(Op, Unit),
	binary_to_list(Exp, Op, Unit, List,[]), !,
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


%   position(Term, Exp, Path) is true when Term occurs in Exp at the
%   position defined by Path.  It may be at other places too.

position(Term, Term, []).
position(_Term, Exp, _Path) :-
	(   var(Exp) ; atomic(Exp) ; ok_number(Exp)   ), !, fail.
position(Term, Exp, Path) :-
	functor(Exp, _, N),
	position(N, Term, Exp, Path).

	position(0, _Term, _Exp, _Path) :- !, fail.
	position(N, Term, Exp, [N|Path]) :-
		arg(N, Exp, Arg),
		position(Term, Arg, Path).
	position(N, Term, Exp, Path) :-
		M is N-1, !,
		position(M, Term, Exp, Path).

 
%   generate intermediate variables, or arbitray integer tokens.

arbint(Var) :-
	gensym(n, Var),
	assert(integral(Var)).

identifier(Var) :-
	asserta(unknown(Var)),
	gensym(x, Var),!.

%   correspond(X, Xlist, Ylist, Y) is true when the position of X and Xlist
%   and the position of Y in Ylist (which is as long as Xlist) are the same.

correspond(X, [X|_], [Y|_], Y) :- !.
correspond(X, [_|T], [_|U], Y) :-
	correspond(X, T, U, Y).


%   apply a substitution, tidy the result

subst_mesg(Substitution, Old, New) :-
	subst(Substitution, Old, Mid),
	tidy(Mid, New).


%   Find the smallest (if C = <) or greatest (if C = >) term in a list of
%   terms, where comparison is by the size of a term.

extreme_term([Head|Tail], C, Term) :-
	term_size(Head, Size),
	extreme_term(Tail, Head, Size, C, Term).

	extreme_term([Head|Tail], _Hold, Sold, C, Term) :-
		term_size(Head, Size),
		compare(C, Size, Sold), !,
		extreme_term(Tail, Head, Size, C, Term).
	extreme_term([_Head|Tail], Hold, Sold, C, Term) :-
		extreme_term(Tail, Hold, Sold, C, Term).
	extreme_term([],	  Term, _,    _, Term).

	term_size(Term, 1) :-
		(   var(Term) ; atomic(Term) ; ok_number(Term)   ), !.
	term_size(Term, Size) :-
		functor(Term, _, N),
		term_size(N, Term, 1, Size).

		term_size(0, _Exp, Ans, Ans) :- !.
		term_size(N, Exp, Acc, Ans) :-
			arg(N, Exp, Arg),
			term_size(Arg, Size),
			Nxt is Acc+Size+1, M is N-1, !,
			term_size(M, Exp, Nxt, Ans).

 % Delete all occurrences of X from list Y to get list Z

delete([], _, []) :- !.
delete([Kill|Tail], Kill, Rest) :- !,
	delete(Tail, Kill, Rest).
delete([Head|Tail], Kill, [Head|Rest]) :- !,
	delete(Tail, Kill, Rest).


 % Remove 'false' and duplications in a disjunction

tidy_up_disjunction(Term,Ans) :- 
	decomp(Term,[#|List]),
	!,
	listtoset(List,List1),
	delete(List1,false,New),
	recomp(Ans,[#|New]).

tidy_up_disjunction(X,Y) :- tidy(X,Y).  % For cases that fall through


wordsin(Term, List) :-
	scan_term(Term, _Some, Tree),
	tree_list(Tree, 1, [], Pairs),
	keysort(Pairs, Inorder),
	strip_num(Inorder, List).

	scan_term(Simp, Old_Tree, Old_Tree) :-
		var(Simp), !.
	scan_term(Simp, Old_Tree, Old_Tree) :-
		ok_number(Simp), !.	%  was integer(Simp)
	scan_term(Atom, Old_Tree, New_Tree) :-
		atom(Atom), !,
		insert_word(Old_Tree, Atom, New_Tree).
	scan_term(List, Old_Tree, New_Tree) :-
		List = [_|_], !,
		scan_list(List, Old_Tree, New_Tree).
	scan_term(Term, Old_Tree, New_Tree) :-
		Term =.. [_Functor|Args], !,
		scan_list(Args, Old_Tree, New_Tree).

		insert_word(t(C, W, L, R), W, t(D, W, L, R)) :- !,
			(   var(C), D = 1
			;   integer(C), D is C+1
			),  !.
		insert_word(t(C, X, L, R), W, t(C, X, M, R)) :-
			W @< X, !,
			insert_word(L, W, M).
		insert_word(t(C, X, L, R), W, t(C, X, L, S)) :-
			W @> X, !,
			insert_word(R, W, S).

		scan_list([Head|Tail], Old_Tree, New_Tree) :-
			scan_term(Head, Old_Tree, Mid_Tree), !,
			scan_list(Tail, Mid_Tree, New_Tree).
		scan_list([],          Old_Tree, Old_Tree).

	tree_list(Tree,		 _Thresh, Accum, Accum) :-
		var(Tree), !.
	tree_list(t(N, _X, L, R), Thresh, Accum, Answer) :-
		N < Thresh, 
		tree_list(L, Thresh, Accum, Sofar), !,
		tree_list(R, Thresh, Sofar, Answer).
	tree_list(t(C, W, L, R), Thresh, Accum, Answer) :-
		tree_list(L, Thresh, Accum, Sofar),
		Key is -C, !,
		tree_list(R, Thresh, [Key-W|Sofar], Answer).

	strip_num([_Key-Word|Rest], [Word|More]) :- !,
		strip_num(Rest, More).
	strip_num([],		   []).

odd(X) :-
	eval(odd(X)).

even(X) :-
	eval(even(X)).

gcd(X, Y, Z) :-
	eval(gcd(X,Y), Z).

commutative(+) :- !.
commutative(*) :- !.

associative(+) :- !.
associative(*) :- !.


 % Is term ground

ground(Term) :- var(Term),!,fail.
ground(Term) :- simple(Term),!.
ground(Term) :- 
	functor(Term,_,N),
	!,
	ground(Term,N).

ground(_,0) :- !.
ground(Term,N) :- 
	arg(N,Term,Arg),
	ground(Arg),
	M is N - 1,
	!,
	ground(Term,M).



 % Recursive Tidy to fix bug in Tidy

final_tidy_expr(Old,Tidy) :-
	tidy(Old,Mid),
	final_tidy1(Old,Mid,Tidy),
	!.



final_tidy1(Old,Mid,Old) :- match(Old,Mid),!.  % No Change
final_tidy1(_,Mid,Tidy) :-
	final_tidy_expr(Mid,Tidy),
	!.





 % Old PRESS code, except that cuts have been removed from collect/3.
 % Need to do this as LP learns new rules.  At present, these rules go
 % above old rules, stopping the old ones firing.  Could assertz new rules,
 % but I prefer new rules to be considered first on relevance grounds.

collect(X,Exp,New1) :- 
	mult_occ(X,Exp),
	least_dom(X,Exp),
	collax(U,Template,Rewrite),
	applicable(Template,Exp,Rest),
	contains(X,U),
	newform(Exp,Rewrite,Rest,New),
	tidy(New,New1).

collect(X,Old,New) :- 
	mult_occ(X,Old),
	decomp(Old,[Fun|Args]),
	corresponding_arguments(Args,Arg,NewArgs,NewArg),
	collect(X,Arg,NewArg),
	recomp(New,[Fun|NewArgs]).


% Does a rewrite rule match an expression? 
% A more efficient version than relying on the built in commutativity
% and associativity of the matcher

applicable(Template,Exp,Rest) :-
	ident_operators(Template,Exp),		% quick test
	template_match(Template,Exp,Rest).

	ident_operators(A, B) :-
		functor(A, Op, _),
		functor(B, Op, _).

template_match(Template,Exp,Rest) :-
	Template=..[Op,C,D],
	ac_op(Op,_,_,_,_),
	!,
	decomp(Exp,[Op|Args]),
	select(A,Args,Rem),
	perm2(C,D,Pat1,Pat2),
	exp_match(A,Pat1,Pat2,Rem,Rest),
	!.

template_match(Template,Exp,[]) :-
	match(Exp,Template).

exp_match(A,C,D,Rem,Rest) :-
	match(A,C),		% stop match backtracking (the key idea) (LS)
	!,			% Not so sure its a good one! BS
	exp_match1(A,C,D,Rem,Rest).

exp_match1(_,_,D,Rem,_) :-
	ops_to_find(D,Ops),
	tidy_ops(Ops,Term),
	absent(Term,Rem),
	!,
	fail.

exp_match1(A,C,D,Rem,Rest) :-
	match(A,C),
	select(B,Rem,Rest),
	match(B,D),
	!.

ops_to_find(Pat,Pat) :- atomic(Pat), !.
ops_to_find(Pat,var) :- var(Pat), !.
ops_to_find(Pat,Term) :-
	Pat =.. [Op|Args],
	ops_list(Args,NewArgs),
	Term =.. [Op|NewArgs].

ops_list([],[]) :- !.
ops_list([H|T],[NewH|NewT]) :-
	ops_to_find(H,NewH),
	ops_list(T,NewT).

absent(_,[]) :- !.
absent(_,[H|_]) :-
	compatible(_,H),
	!,
	fail.

absent(Term,[_|Rest]) :- absent(Term,Rest).

compatible(var,_) :- !.
compatible(Term,H) :-
	Term=..[Op|Args],
	H=..[Op|Terms],
	list_compatible(Args,Terms).

list_compatible([],[]) :- !.
list_compatible([H|T],Terms) :-
	select(A,Terms,Rest),
	compatible(H,A),
	list_compatible(T,Rest),
	!.

newform(_,Rewrite,[],Rewrite) :- !.

newform(Exp,Rewrite,Rest,New) :-
	Exp=..[Op|_],
	recomp(Term,[Op|Rest]),
	New=..[Op,Rewrite,Term].

tidy_ops(var*Term,New) :- !, tidy_ops(Term,New).
tidy_ops(Term*var,New) :- !, tidy_ops(Term,New).
tidy_ops(var+Term,New) :- !, tidy_ops(Term,New).
tidy_ops(Term+var,New) :- !, tidy_ops(Term,New).
tidy_ops(Term,Term).

