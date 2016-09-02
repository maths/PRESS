%   File   :  /usr/bs/lpdir/rew.pl
%   Author : Bernard Silver
%   Updated: Tue Oct 15 11:37:46 1985
%   Purpose: Apply Rewrite rule in LP

apply_new_rule1(X,Exp,New,Name,Type) :-
	ok_vars(New,Name),  % this is the rule we want or its a worked example
	occ(X,Exp,No1),
	(Type=tl,occ(X,New,No2);Type=sol),
	get_user_rule(Name,X,Template => Rewrite,Cond,No1,No2,Cond1,Type),
	((nonvar(No2),check_cond(Cond1));var(No2)),
	try_use_rule(X,Exp,New,Template=>Rewrite,Cond,Cond1,No2).

apply_new_rule1(X,Exp,New,Name,Type) :- 	
	\+ ground(New),
	var(Name),  % Must be careful
	occ(X,Exp,No1),
	get_user_rule(Name,X,Template => Rewrite,Cond,No1,No2,Cond1,Type),
	examine_template(Template,N),
	apply_new_rule2(X,Exp,New,Template=>Rewrite,Cond,N,Cond1,No2).

 % One is instantiated
ok_vars(New,_) :-  ground(New),!.
ok_vars(_,Y) :- nonvar(Y),!.


 % Rule has at least two terms in LHS

apply_new_rule2(X,Exp,New1,Template=>Rewrite,Cond,N,Cond1,F) :- 
	N>1,
	apply_new_rule3(X,Exp,New1,Template=>Rewrite,Cond,Cond1,F).

 % Rule has only one term on LHS, eg sin(2*x) => 2*sin(x)*cos(x)
apply_new_rule2(X,Exp,New1,Lhs=>Rhs,Cond,_,Cond1,F) :-
	apply_new_rule4(X,Exp,New1,Lhs=>Rhs,Cond,Cond1,F).


 % Basically Collection Code
apply_new_rule3(X,Exp,New1,Template=>Rewrite,Cond,Cond1,F) :-
	mult_occ(X,Exp),
	least_dom(X,Exp),		% Expression is in weak normal form
	applicable(Template,Exp,Rest),
	((nonvar(F),check_cond(Cond));var(F)),
	newform(Exp,Rewrite,Rest,New),
	check_cond(Cond),
	choice_tidy(New,New1),
	occ(X,New1,F),
	check_cond(Cond1).



 % Recursive case of Collection
apply_new_rule3(X,Exp,New1,Template=>Rewrite,Cond,Cond1,F) :-
	mult_occ(X,Exp),
	decomp(Exp,[Fun|Args]),
	corresponding_arguments(Args,Arg,NewArgs,NewArg),
	delete_one(Arg,Args,Rest), % These arent being passed
	occ(X,Rest,F1),		% so deduct their occ of x
	NewF is F-F1,		% from the old value
	apply_new_rule3(X,Arg,NewArg,Template=>Rewrite,Cond,Cond1,NewF),
	recomp(New,[Fun|NewArgs]),
	choice_tidy(New,New1).



 % Other case
apply_new_rule4(X,Exp,New,Lhs=>Rhs,Cond,Cond1,F) :- 
	match(Exp,Lhs),
	check_cond(Cond),
	choice_tidy(Rhs,New),
	occ(X,New,F),
	check_cond(Cond1).



apply_new_rule4(X,Exp,New,Lhs=>Rhs,Cond,Cond1,F) :- 
	setof(Term,subterms(Exp,X,Term),Set),
	apply_new_rule5(Set,X,Lhs,Rhs,Exp,New,Cond,Cond1,F).


apply_new_rule5(Set,X,Lhs,Rhs,Exp,New,C,C1,F) :- 	
	match_member(Lhs,Set,Term),
	((ground(F),check_cond(C));\+ground(F)),
	subst(Term=Rhs,Exp,New1),
	check_cond(C),
	choice_tidy(New1,New),
	occ(X,New,F),
	check_cond(C1),
	!.



 % What sort of rule is it?
examine_template(_+_,X) :- !,X=2.
examine_template(_*_,X) :- !,X=2.
examine_template(_,X) :- X=1.   % Single term or more complex rule
				% Collect can't cope with either

try_use_rule(X,Exp=C,New1,Template=C1 =>Rewrite,Cond,Cond1,No2) :-
	match_check(C,C1),
	functor(Exp,F,_),
	functor(Template,F,_),
	associative(F),
	decomp(Exp,[F|Arg1]),
	decomp(Template,[F|Arg2]),
	length(Arg2,2),
	length(Arg1,N),
	((N>2,!);N>1),
	pairfrom(Arg1,A,B,Rest),
	recomp(Term,[F,A,B]),
	match_check(Term,Template),
	recomp(Rest1,[F|Rest]),
	New=..[F,Rest1,Rewrite],
	check_cond(Cond),
	choice_tidy(New,New1),
	occ(X,New1,No2),
	check_cond(Cond1).

try_use_rule(X,Exp=C,New1,Template=>Rewrite,Cond,Cond1,No2) :-
	functor(Exp,F,_),
	functor(Template,F,_),
	associative(F),
	decomp(Exp,[F|Arg1]),
	decomp(Template,[F|Arg2]),
	length(Arg2,2),
	length(Arg1,N),
	((N>2,!);N>1),
	pairfrom(Arg1,A,B,Rest),
	recomp(Term,[F,A,B]),
	match_check(Term,Template),
	recomp(Rest1,[F|Rest]),
	New=..[F,Rest1,Rewrite],
	check_cond(Cond),
	choice_tidy(New=C,New1),
	occ(X,New1,No2),
	check_cond(Cond1).


try_use_rule(X,Exp,New1,Template=>Rewrite,Cond,Cond1,No2) :-
	examine_template(Template,N),
	apply_new_rule2(X,Exp,New1,Template=>Rewrite,Cond,N,Cond1,No2).

 % Get user_rule of the right type

 % Get a tidy type rule, these can loop so only used for tl case
 % or if Name is instantiated
get_user_rule(Name,X,Template => Rewrite,Cond,No1,No2,Cond1,Type) :-
	((nonvar(Name),Type=tl);var(Name)),
	call(simple_rule(Name,X,Template => Rewrite,Cond,No1,No2,Cond1)).

get_user_rule(Name,X,Template => Rewrite,Cond,No1,No2,Cond1,_) :-
	call(user_rule(Name,X,Template => Rewrite,Cond,No1,No2,Cond1)).

choice_tidy(Old,New) :- choice_tidy1(Old,New).

choice_tidy(Old,New) :- 
	hard_tidy_expr(Old,Tidy),
	Old \= Tidy,
	choice_tidy1(Tidy,New).

choice_tidy1(Old,Mid) :- tidy_expr(Old,New),match_check(Mid,New),!.
choice_tidy1(Old,Mid) :- tidy(Old,New),match_check(Mid,New),!.

hard_tidy_expr(Exp#Exp1,New) :- !,
	hard_tidy_expr(Exp,Exp2),
	hard_tidy_expr(Exp1,Exp3),
	!,
	tidy_expr(Exp2#Exp3,New),
	!.
hard_tidy_expr(Exp=A,New) :- !,
	hard_tidy_expr(Exp,Exp1),
	simplify(A,A2),
	tidy_expr(A2,A1),
	!,
	tidy_expr(Exp1=A1,New),
	!.

hard_tidy_expr(Exp,Exp) :- (atomic(Exp);ok_number(Exp)),!.

hard_tidy_expr(A+B,New) :- !,
	hard_tidy_expr(A,A1),
	hard_tidy_expr(B,B1),
	tidy_expr(A1+B1,New1),
	simplify(New1,New),
	!.

hard_tidy_expr(A*B,New) :- !,
	hard_tidy_expr(A,A1),
	hard_tidy_expr(B,B1),
	tidy_expr(A1*B1,New1),
	simplify(New1,New),
	!.

hard_tidy_expr(Exp,New) :-
	functor(Exp,F,N),
	functor(New1,F,N),
	hard_tidy_expr(N,Exp,New1),
	tidy_expr(New1,New).


hard_tidy_expr(0,_,_) :- !.
hard_tidy_expr(N,Term,New) :-
	arg(N,Term,ArgN),
	hard_tidy_expr(ArgN,ArgN1),
	arg(N,New,ArgN1),
	M is N - 1,
	!,
	hard_tidy_expr(M,Term,New).

 % Simplify rules
simple_rule(sec,_,sec(Y)=>1/cos(Y),[non_zero(cos(Y))],M,M,[]).
simple_rule(cos,_,cos(Y)=>1/sec(Y),[],M,M,[]).

simple_rule(cosec,_,cosec(Y)=>1/sin(Y),[non_zero(sin(Y))],M,M,[]).

simple_rule(sin,_,sin(Y)=>1/cosec(Y),[],M,M,[]).

simple_rule(cot,_,cot(Y)=>1/tan(Y),[non_zero(tan(Y))],M,M,[]).

simple_rule(tan,_,tan(Y)=>1/cot(Y),[non_zero(cot(Y))],M,M,[]).
