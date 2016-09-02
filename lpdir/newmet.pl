%   File   : NEWMET
%   Author : Bernard Silver
%   Updated: 7 October 1984
%   Purpose: Create new methods




/* Work backwards through the description of the worked example.
X is the unknown in the example, U is the variable representing the unknown 
in the rules.

Find the first hard step, one which we do not know the reason for, although
we know that a certain rule, Name was used to perform the step.
(If we just have a fail, or tellequal marker, due to not having the
rules, no new method can be created.  Type is a flag to indicate this.)
This step transforms the equation from From to To (say!), is step N
in the input list.

Find which method was applied next, call this next method NM.
Find the preconditions of NM, making two copies Pre and Pre1.  Also
make two copies of the pattern that NM expects for input, Patt and Patt1.

Find which of the preconditions Pre, (of NM), are not satisfied by From.
These are called MP.  ND is a message giving information about MP.

Those preconditions of NM that are satisfied are called Rest.  The
two copies are needed to avoid  unwanted instantiations.

Now create a new method that makes the bad preconditions satisfied, we
already know that rule Name does this.  We insist that after application of
the new method all preconditions of NM are satisfied, so that method can be
applied.

*/

find_new_method(X,List,Method,RList) :- 
	allowed_new_methods,
	find_first_hard_step(List,RList,From,To,NM,Type,N),
	get_preconditions(U,NM,Patt1,Pre1),
	get_preconditions(U,NM,Patt,Pre), % Need two copies
	find_missing_preconds(To,U,X,From,MP,Pre,Patt),
	find_ok_preconds(U,X,From,Patt1,Pre1,Rest),
	tell_reason(From,To,NM,MP,ND),
create_method(From,U,Patt1,Patt,NM,ND,List,NewL,N,Rest,Type,Pre,X),
	!,
	find_new_method(X,NewL,Method,RList).

find_new_method(_,List,List,_).

get_preconditions(X,user(Name),Pattern,PreCond) :- !,
	user_rule(Name,X,Pattern=>_, PreCond,_,_,_).

get_preconditions(X,Method,Pattern,Precond) :- 
clause(known_method_tl(X,Pattern,_,Method,_,_,Precond,_),_),
	!.

get_preconditions(X,Method,Pattern,Precond) :- 
clause(known_method_auto(X,Pattern,_,Method,_,Precond,_),_),
	!.

get_preconditions(X,Method,Pattern,Precond) :- 
clause(known_method2(X,Pattern,_,Method,_,_,Precond,_),_),
	!.

find_missing_preconds(To,U,X,Old,Rd,Precond,Pattern) :-
	check_the_preconds(X,U,Precond,Pattern,To),
	find_missing_preconds1(U,X,Old,Pattern,Precond,[],Rd),
	!.



find_missing_preconds1(_,_,_,_,[],Method,Method) :- !.
find_missing_preconds1(U,X,Eqn,P1,[H|T],Acc,Method) :-
	not not (U=X,Eqn=P1,call(H)),
	!,
	find_missing_preconds1(U,X,Eqn,P1,T,Acc,Method).

find_missing_preconds1(U,X,Eqn,P1,[H|T],Acc,Method) :- !,
	find_missing_preconds1(U,X,Eqn,P1,T,[H|Acc],Method).


 % These preconditions are satisfied and standardized apart form the others
find_ok_preconds(U,X,Eqn,P1,List,Method) :-
	find_ok_preconds1(U,X,Eqn,P1,List,[],Method).


find_ok_preconds1(_,_,_,_,[],Acc,Acc) :- !.
find_ok_preconds1(U,X,Eqn,P1,[H|T],Acc,Method) :-
	not not (U=X,Eqn=P1,call(H)),
	!,
	find_ok_preconds1(U,X,Eqn,P1,T,[H|Acc],Method).

find_ok_preconds1(U,X,Eqn,P1,[_|T],Acc,Method) :-!,
	find_ok_preconds1(U,X,Eqn,P1,T,Acc,Method).

check_the_preconds(X,U,Pre,Pattern,New) :-
	test_preconds(X,U,Pre,Pattern,New),
	fail.	 % Undo bindings

check_the_preconds(_,_,_,_,_).

test_preconds(X,U,Pre,Pattern,New) :-
	X=U,
	New = Pattern,
	!,
	check_preconds1(Pre). 

check_preconds1([]) :- !.
check_preconds1([H|T]) :- call(H),!,check_preconds1(T).

find_first_hard_step(L1,L2,From,To,Next,Type,Number) :-
	find_first_hard_step(L1,L2,From,To,Next,Type,Number,2).

find_first_hard_step([N,fail,_|_],[_,To,From|_],From,To,N,fail,A,A) :-
	!.
find_first_hard_step([N,user_rule(Name,From,To),_|_],[_,To,From|_],From,To,
	N,ur(Name),A,A) :- !.

find_first_hard_step([N,tellequal(From,To),_|_],
	[_,To,From|_],From,To,N,te,A,A) :- !.

find_first_hard_step([_|T],[_|T1],From,To,Next,Type,A,N) :-
	M is N + 1,
	find_first_hard_step(T,T1,From,To,Next,Type,A,M).



tell_reason(From,To,Me,Difference,NewDiff) :-
	writef('\nTrying to find a reason why the step from\n'), 
	writef('\n\t%t\n\nto\n\n\t%t\n\nwas performed.\n',[From,To]),
	translate_difference(Difference,NewDiff,Message),
	!,
writef('\nReason found is that next method to be applied,\n\n\t%w\n',[Me]),
	call(Message).

 % No differences found
tell_reason(_,_,_,_,[]) :- !.

translate_difference([H|T],upc([H|T]),explain_g([H|T])) :- !.

translate_difference(_,_,_) :- !,fail.


explain_g(List) :-
	length(List,Length),
	single_plural(List,S,_),
	writef('\n\nrequires its input to satisfy the following unsatisfied
precondition%w:\n\n',[S]),
	explain_g1(List),
	!.

explain_g1(List) :-
	writef('\nPrecondition\t\t\tExplanation\n'),
	output_preconds(List),
	!.

output_preconds([]).
output_preconds([H|T]) :-
	functor(H,F,N),
	get_explanation(F/N,Explan),
	writef('\n%t/%t\t\t%w\n',[F,N,Explan]),
	!,
	output_preconds(T).


 % Create new method

 % From is eqn before rule Name was applied, To is after, X is the unknown.
 % OldP, NewP and U are variable forms of these.  NM is the next method
 % Type is the type of reason discovered for the step, List is the list
 % of steps in the worked example, NL is this list after the step with
 % the user rule application has been relabeled with the new method name.
 % Pre is the list of preconditions, Rest are those that were satisfied by 
 % From.  

create_method(From,U,OldP,NewP,NM,Type,List,NL,N,Rest,ur(Name),Pre,X) :- !,
  create_method1(U,OldP,NewP,NM,Type,Method,Rest,Name,Pre,From,X,NewM,List),
	cond_create(NewM),
	nmember(Term,List,N),
	subst(Term= method(Method),List,NL),
	add_new_method(Method,Type),
	!.

create_method(_,_,_,_,_,_,_,_,_,_,_,_,_) :- !,
	writef('\nNo means of producing new method.\n'),
	fail.

create_method1(U,OldP,NewP,NMethod,upc([H|T]),Method,Rest,Name,_,_,_,MT,_) :-!,
	MT = [upc([H|T]),Rest,[H|T],U,OldP,NewP,Method,Name,NMethod].

create_method1(U,OldP,NewP,NMethod,[],Method,Pre,Name,Pre1,From,X,MT,_) :- 
	applicable_next_method(NMethod,X,From),
	!,
	MT= [[para(NMethod,Name)],Pre,Pre1,U,OldP,NewP,Method,Name,NMethod].

create_method1(U,Old,New,method(NM),[],Method,Pre,Name,Pre1,From,X,MT,_) :- 
	consider_parallel(U,Old,Name,From,X,NM,NName),
	!,
	MT= [[ei(NM,Name,NName)],Pre,Pre1,U,Old,New,Method,Name,method(NM)].
	


create_method1(U,OldP,NewP,NMethod,[],Method,Pre,Name,Pre1,_,_,MT,_) :- !,
	MT= [[ok(NMethod)],Pre,Pre1,U,OldP,NewP,Method,Name,NMethod].

create_method1(_,_,_,_,_,_,_,_,_,_,_,_,_) :-
	writef('\nCan not create a new method.\n'),
	!,
	fail.


 % Only create methods if there are not already ones like them.

cond_create([]) :- !.

 % Already have method like this

cond_create([Flag,Pre,Post,U,OldP,NewP,Method,Name,NMethod|_]) :-
	known_method_auto(U,OldP,NewP,method(Method),
try_auto_method(Method,U,OldP,NewP),Pre,
[applicable_next_method(NMethod,U,NewP)|Post]),
	!,
	already_suitable_method(Method,Name,Flag).

 % Create new method
cond_create([upc([H|T]),Rest,[H|T],U,OldP,NewP,Method,Name,NMethod]) :-
	!,
	mod_gensym(auto,Method),
	pretty_print_conds([H|T],method(Method)),
	store_rules_list(Method,Name),
	mod_asserta(known_method_auto(U,OldP,NewP,method(Method),
try_auto_method(Method,U,OldP,NewP),Rest,
[applicable_next_method(NMethod,U,NewP)|[H|T]])).

cond_create([[Flag],Pre,Pre1,U,OldP,NewP,Method,Name,NMethod]) :-
	mod_gensym(auto,Method),
	tell_create(Flag),
	writef('\nCreating new method, named %t.\n',[method(Method)]),
	store_rules_list(Method,Name),
	mod_asserta(known_method_auto(U,OldP,NewP,method(Method),
try_auto_method(Method,U,OldP,NewP),Pre,
[applicable_next_method(NMethod,U,NewP)|Pre1])).

tell_create(para(NMethod,Name)) :-
	writef('\nRule %t is applied in parallel with %t.\n',[Name,NMethod]),
	mod_asserta(para(NMethod,Name)),
		!.

tell_create(ei(NMethod,Name,NName)) :- 
	writef('\nMethod %t (rule %t) and %t can be applied in either order.\n',
[NMethod,NName,Name]),
	mod_asserta(ei(NMethod,Name,NName)),
	!.


tell_create(ok(NMethod)) :-
	writef('\n
The next method, %w, can not be applied unless the step occurs, but 
no outstanding preconditions can be found.\n
Assuming that this step is preparation for %t.\n',[NMethod,NMethod]),
	!.

already_suitable_method(Method,Name,Flag) :-
	flag(method(method(Method)),on,on),
	!,
	a_s_m1(Method,Name,Flag).


already_suitable_method(M,_,_) :- !,
	writef('\n[Method %t is disabled, will not create duplicate]\n',[M]).

 % First clause should only be used if using a file of new rules
 % or adding just created rule
a_s_m1(M,Name,Flag) :-
	(not auto_rule(M,Name);(Flag= [ei(_,Name,N)],just_created(rule(N)))),
	!,
	writef('\nMethod %w is applicable.\n
Adding rule %w to rule list of this method.\n',[M,Name]),
	mod_assert(auto_rule(M,Name)),
	!.

a_s_m1(Method,_,_) :-
	just_created(Method),
	writef('
This is the newly defined method %t again.\nNothing done.\n',[Method]),
	!.

applicable_next_method('Factorization',_,_*_=0) :- !.
applicable_next_method('Split Disjunctions',_,_#_) :- !.
applicable_next_method('Isolation',X,Eqn) :- !,
	single_occ(X,Eqn).
applicable_next_method('Change of Unknown',X,Eqn) :- !,
	identical_subterms(Eqn,X,_).
applicable_next_method('Function Stripping',X,Eqn) :-
	dominated(X,Eqn,_),
	!.
applicable_next_method('Prepare for Factorization',X,A+B=0) :- !,
	common_subterms(X,A+B,_).

applicable_next_method(NMethod,X,NewP) :-   %Not right yet!
	known_method(X,NewP,_,NMethod,_,Call,Pre,Post),
	check_cond(Pre),
	method_transform(yes,Call,_,_,_,gag,0,_,10000),
	check_cond(Post),
	!.

consider_parallel(U,Old,Name,From,X,NName,NName1) :-
	auto_rule(NName,NName1),
	NName1 \==Name,
	not not try_parallel(U,X,From,Old,NName1),
	!.

try_parallel(U,X,From,Old,Name) :-
	U=X,
	From=Old,
	apply_new_rule1(X,Old,_,Name,sol),
	!.

 % Store the rules list for the new method
store_rules_list(Method,Name) :-
	not auto_rule(Method,Name),
	!,
	mod_assert(just_created(rule(Name))),
	mod_assert(auto_rule(Method,Name)),
	!.

store_rules_list(_,_).

pretty_print_conds(List,Name) :-
	single_plural(List,S,S1),
writef('\nCreating a new method, named  %t,\n\nthat transforms equation so that the
following precondition%w %w satisfied:\n\n<',[Name,S,S1]),
	map_prettify(List),
	!.

map_prettify([H]) :- !,
	functor(H,F,N),
	writef('%t/%t>\n',[F,N]),!.
map_prettify([H|T]) :- !,
	functor(H,F,N),
	writef('%t/%t,  ',[F,N]),
	!,
	map_prettify(T).


