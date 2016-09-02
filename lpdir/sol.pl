%   File   :  /usr/bs/lpdir/sol.pl
%   Author : Bernard Silver
%   Updated: Tue Oct 15 11:35:05 1985
%   Purpose: Solve routine of LP

:- dynamic
	already_said/4,
	bad_schema/1,
	last_equation/1,
	schema/1,
	schema_abandon/3,
	schema_used/1,
	tidy_last_equation/1.

 % Solve new equations
solve(Eqn) :- solve(Eqn,x,_).
solve(Eqn,X,Ans) :- solve(Eqn,X,Ans,_,_).

solve(Eqn,X,Ans,Time,NewFlag) :-
	initialize_screen,
	statistics(runtime,_),
	show_initialize_and_store(X,Eqn,Eqn1),
	get_bounds(X,Eqn1,Bound),
	sort_solve(Eqn1,X,Ans1,Flag,EqnSteps-[],0,Total,Bound),
	!,
        tidy_up_disjunction(Ans1,Ans2),
        simplify(Ans2,Ans),		
	mod_writef(Ans),
	store_steps(Eqn1,X,Flag,EqnSteps,NewFlag,Total),
	!,
	statistics(runtime,[_,Time]),
	writef('\n[Problem took %t milliseconds]\n',[Time]),
	equation_string(solved,Eqn),
	!.

solve(E,_,_,T,F) :-
	statistics(runtime,[_,T]),
	modify_schema_flag(failtest,F),
	writef('\nCouldn''t solve the equation\n%t.\n
	[The attempt took %t milliseconds]\n',[E,T]),
	equation_string(fails,E).

 % Equation is solved

sort_solve(Eqn,X,Eqn,win,T-T,N,N,_) :-
	dis_solution(Eqn,X),
	!.


sort_solve(Eqn,X,Eqn,win,E-E,N,N,_) :-
	freeof(X,Eqn),
	!,
	writef('\n%t does not contain the unknown %t.\n',[Eqn,X]).

 % Look for and use suitable schema
sort_solve(Eqn,X,Ans,new_flag(Flag,Flag1),EqnSteps,Acc,Total,Bound) :-
	suitable_schema(Eqn,X,List,Eqn1,Type,Flag,Name,AssocCheck),
	!,
	loop_flag(warn1),
	mod_asserta(schema_used(Name)),
	writef('\nAttempting to use schema method %t\n',[Name]),
	write_informative_message(AssocCheck,Eqn1),
  sort_solve_main(Eqn,X,Ans,List,Type,Flag1,EqnSteps,Name,Acc,Total,Bound).


sort_solve(Eqn,X,Ans,F,Steps,Acc,Total,Bound) :-
	sort_solve_one_step(Eqn,X,Ans,F,Steps,Acc,Total,Bound).

 % Do one step
sort_solve_one_step(Eqn,X,Ans,F,T-T1,Acc,Total,Bound) :- 
	loop_flag(yes),		% Switch on loop check
	mod_weak_normal_form(Eqn,X,Eqn1),
	looping(Eqn1,X),
	!,
	step_solve_eqn(no,Eqn1,X,_,New1,T-T2,Acc,NewAcc,Bound),
	tell_redo(New1,Eqn1),
	sort_solve(New1,X,Ans,F,T2-T1,NewAcc,Total,Bound).

 % Tell if fail to solve new equation
tell_redo(_,_).
tell_redo(New,Old) :- 
	writef('\nFailed to solve equation %t.\n\nBack to %t.\n',[New,Old]),
	fail.

 % Main schema loop
sort_solve_main(Eqn,X,Ans,List,Type,Flag,EqnSteps,Name,Acc,Total,Bound) :- 
	sort_solve1(Eqn,X,Ans,List,Type,EqnSteps,Acc,Total,Bound),
	(retract(schema_abandon(X,Eqn,Name)) -> 
		Flag = bad(Name),
		assert(bad_schema(Name))
	;
		Flag = good(Name)),
	!.

 % Start again without the schema
sort_solve_main(Eqn,X,Ans,_,_,bad,EqnSteps,Name,_,Total,Bound) :- 
 writef('\nCan''t use schema %t\n\n',[Name]),
	reset,
	mod_asserta(schema_abandon(X,Eqn,Name)),
	mod_asserta(bad_schema(Name)),
	sort_solve(Eqn,X,Ans,_,EqnSteps,0,Total,Bound).

sort_solve1(Eqn,X,Ans,List,Type,EqnSteps,Acc,Total,Bound) :-
	looping(Eqn,X),
	sort_solve1a(Eqn,X,Ans,List,Type,EqnSteps,Acc,Total,Bound),
	!.

 % Equation is solved
sort_solve1a(Eqn,X,Eqn,_,_,T-T,Total,Total,_) :-
	dis_solution(Eqn,X),
	!,
	writef('\n[End of solution]\n').

sort_solve1a(Eqn,X,Eqn,_,_,T-T,Total,Total,_) :-
	freeof(X,Eqn),
	!,
	writef('Equation doesn''t contain the unknown\n'),
	writef('\n[End of solution]\n').

 % Try to apply schema at once
sort_solve1a(Eqn,X,_,_,_,_,Acc,_,Bound) :- 
	too_many_steps(Eqn,X,Acc,Bound),
	!,
	fail.
sort_solve1a(Eqn,X,Ans,List,Type,EqnSteps,Acc,Total,Bound) :- 
	sort_solve_now(Eqn,X,Ans,List,Type,EqnSteps,Acc,Total,Bound),
	!.
sort_solve_now(Eqn,X,Ans,[L1,L2|L3],'Change of Unknown',T-T1,Acc,Tot,B) :- !,
	writef('\nAttempting to change the unknown.\n'),
	prep_chunk(Term,Eqn,X,New,L1,T-F1,Acc,NewA,B),
	action_after_p_c(New,X,Ans,T1,F1,[L2|L3],Term,NewA,Tot,B).


sort_solve_now(Eqn,X,Ans,[L1|F],'Factorization',T-T1,Acc,Tot,B) :- !,
	writef('\nAttempting to manipulate equations into factors.\n'),
	prep_factors(Eqn,X,New,L1,T-T2,Acc,NewAcc,B),
	!,
	action_after_prep_factors(New,X,Ans,F,T2-T1,NewAcc,Tot,B),
	!.

sort_solve_now(Eqn,X,Ans,List,Type,T-T1,Acc,Total,Bound) :-
	step_cycle(Eqn,X,New,List,Rest,T-T2,Acc,NewAcc,Bound),
	!,
	sort_solve1(New,X,Ans,Rest,Type,T2-T1,NewAcc,Total,Bound).


action_after_prep_factors(New,X,Ans,F,T2-T1,Acc,Tot,Bound) :-
	(decomp(New,[#|_]) ->
	writef('\nFactorizing equation to obtain\n\n%t\n',[New]),
	writef('\nTrying to solve the factors.\n');true),
	NewAcc is Acc+1,
	factor_solve(New,X,Ans,F,T2-T1,NewAcc,Tot,Bound),
	!.


 % Do the initial part of chunk schema
prep_chunk(_,Eqn,X,Eqn,_,T-T,Acc,Acc,_) :-
	dis_solution(Eqn,X),
	!,
	writef('\nThe equation is solved.\n').

prep_chunk(Term,Eqn,X,New,_,T-T,Acc,Acc,_) :-
	hard_identical_subterms(Eqn,X,Term,New),
	!.

prep_chunk(Term,Eqn,X,New,List,T-T1,Acc,Tot,Bound) :-
	not too_many_steps(Eqn,X,Acc,Bound),
	step_cycle(Eqn,X,Eqn1,List,Rest,T-T2,Acc,NewAcc,Bound),
	not too_many_steps(Eqn1,X,NewAcc,Bound),
	prep_chunk(Term,Eqn1,X,New,Rest,T2-T1,NewAcc,Tot,Bound),
	!.


action_after_p_c(Eqn,X,Eqn,F,F,_,_,_,_,_) :-
	dis_solution(Eqn,X),
	!.
action_after_p_c(New,X,Ans,T1,[NV=Term,CV|T2],[L2|L3],Term,NewA,Tot,B) :-
	identifier(NV),
	mod_subst_mesg(Term=NV,New,CV),
	NewA1 is NewA+1,
	writef('\nTrying to solve the changed variable equation.\n'),
	sort_solve1(CV,NV,NewEqn,L2,'General',T2-[SE|T3],NewA1,NA,B),
	mod_subst_mesg(NV=Term,NewEqn,SE),
	NA1 is NA+1,
	writef('\nTrying to solve the substitution equation.\n'),
	split_disjunct_solve(SE,X,Ans,L3,T3-T1,NA1,Tot,B),
	!.

 % Do initial part of factor schema
prep_factors(A*B=0,X,New,_,[New1|T]-T,Acc,Acc,_) :- 
	decomp(A*B,[*|List]),
	remove_safe_divisors(X,List,New),
	length(New,Length),
	Length > 1,
	recomp(New1,[#|New]).


prep_factors(Eqn,X,New,List,T-T1,Acc,Tot,Bound) :-
	not too_many_steps(Eqn,X,Acc,Bound),
	step_cycle(Eqn,X,New1,List,Rest,T-T2,Acc,NewAcc,Bound),
	not too_many_steps(New1,X,NewAcc,Bound),
	prep_factors(New1,X,New,Rest,T2-T1,NewAcc,Tot,Bound).


 % Split the Change of Unknown case
split_disjunct_solve(A#B,X,Ans,L,Steps,Acc,Tot,Bound) :- !,
	decomp(A#B,[#|List]),
	writef('\nSolving disjuncts.\n'),
	NewAcc is Acc+1,
	factor_solve(List,X,Ans,L,Steps,NewAcc,Tot,Bound).

split_disjunct_solve(Eqn,X,Ans,L,Steps,Acc,Tot,Bound) :-
	sort_solve1(Eqn,X,Ans,L,other,Steps,Acc,Tot,Bound).

 % Rest of factor schema
factor_solve(New,X,Ans,Schema,EqnSteps,Acc,Tot,Bound) :-
	f_s1(New,X,Ans,Schema,false,EqnSteps,Acc,Tot,Bound),
	!.

factor_solve(New,X,Ans,_,EqnSteps,Acc,Tot,Bound) :-
	reset,
	recomp(Eqn,[#|New]),
	writef('\nCan''t use schema to solve factors.  
Trying to solve factorized equation\n\n%t\n\n without schema.\n',[Eqn]),
	sort_solve_one_step(Eqn,X,Ans,_,EqnSteps,Acc,Tot,Bound).

f_s1([],_,Ans,_,Acc,S-S,Tot,Tot,_) :- !,tidy_up_disjunction(Acc,Ans).
f_s1([H|List],X,Ans,Schema,Acc,[H|T]-T1,LAcc,LTot,Bound) :-
	not too_many_steps(H,X,LAcc,Bound),
	choose_member_schema(H,X,S,Schema),
	disjunct_writef([H,X]),
	writef('\nChoosing schema to solve this factor.\n'),
	sort_solve1(H,X,Ans1,S,other,T-T2,LAcc,NewLAcc,Bound),
	not too_many_steps(List,X,NewLAcc,Bound),
	!,
	tidy(Acc#Ans1,NewAcc),
	f_s1(List,X,Ans,Schema,NewAcc,T2-T1,NewLAcc,LTot,Bound).

step_cycle(Eqn,X,New,List,Rest,Steps,Acc,Tot,Bound) :-
	get_next_element(List,Step,Rest1),
	step_cycle1(Eqn,X,New,List,Rest,Step,Rest1,Steps,Acc,Tot,Bound).

step_cycle1(Eqn,X,New,List,Rest,Step,Rest1,Steps,Acc,Tot,Bound) :-
	suitable_step(X,Eqn,Step,_,New,Steps,Flag,Acc,Tot,Bound),
	(Flag = win -> Rest = Rest1; Rest = List),
	!.

 % Miss out steps if possible
step_cycle1(Eqn,X,New,_,Rest,Step,Rest1,Steps,Acc,Tot,Bound) :-
	no_major_effect(Step),
	writef('\nAttempting to omit step.\n'),
	step_cycle(Eqn,X,New,Rest1,Rest,Steps,Acc,Tot,Bound).

 % Perform one step, using method Name to give New1
step_solve_eqn(Flag,Eqn,X,Name,New,EqnSteps,Acc,Tot,Bound) :-
	step_solve_eqn1(Flag,Eqn,X,Name,New,EqnSteps,Mess,Acc,Tot,Bound),
	call(Mess).


step_solve_eqn1(Flag,Eqn,X,Name,New,EqnSteps,Mess,Acc,Tot,Bound) :-
	known_method(X,Eqn,New,Name,Where,Call,Precond,Post),
	not schema(Name),
	check_cond(Precond),
	(key_method(Name)->!;true), 
	method_transform(Flag,Call,Where,Mess,EqnSteps,v,Acc,Tot,Bound),
	maybe_check_cond(Flag,Name,Post).

maybe_check_cond(no,method(_),_) :- !.
maybe_check_cond(_,_,Post) :-
	check_cond(Post),
	!.




 % Using the methods from step_solve_eqn 

method_transform(_,try_to_isolate(X,Old,New),all,Mess,[New|S]-S,F,Acc,NA,_) :-
	position(X,Old,Posn),
	isolate(Posn,Old,New),
	NA is Acc + 1,
	tidy(New,New1),
	!,
	(F= gag -> Mess=true;Mess = mod_iso_trace(New1)).

method_transform(Type,try_disjunct(X,A#B,Ans),all,true,[A#B|E]-E1,F,Acc,NA,Bound) :-
	decide_message(Type,F,NewType),
	(NewType=no;
	writef('\nSplitting into disjuncts and solving each in turn.\n')),
	decomp(A#B,[#|List]),
	disjunct_solve(NewType,List,X,Ans,false,E-E1,Acc,NA,Bound),
	!.

method_transform(Type,try_factorize(X,A*B=0,Ans),all,true,Z-E1,F,Acc,Tot,Bound) :- 
	decide_message(Type,F,NewType),
	decomp(A*B,[*|List]),
	remove_safe_divisors(X,List,New),
	recomp(Disj,[#|New]),
	(length(New,1) -> Z=E;
	(Z=[Disj|E],(NewType=no ->true;
	writef('\nSplitting into factors and solving each in turn.\n')))),
	disjunct_solve(NewType,New,X,Ans,false,E-E1,Acc,Tot,Bound).

 % New Methods without schemas (Messy!!)
method_transform(no,try_auto_method(Method,X,Patt,New),all,true,[Mid|S]-S1,F,Acc,Tot,Bound) :-
	auto_rule(Method,Name),
	apply_new_rule1(X,Patt,Mid,Name,sol),
	auto_next_applicable_method(Method,NewMethod,X,Mid,Rest),
	check_cond([applicable_next_method(NewMethod,X,Mid)|Rest]),
	m_t1(X,Mid,Name,Method,NewMethod,New,S-S1,F,Acc,Tot,Bound).

 % New methods using schemas 
method_transform(yes,try_auto_method(Method,X,Patt,New),all,
 writef('\nUsing rule %t to apply new method,\n%t, to obtain\n\n%t.\n',	[Name,Method,New]),[New|S]-S,_,Acc,Tot,_) :-
	auto_rule(Method,Name),
	apply_new_rule1(X,Patt,New,Name,sol),
	Tot is Acc + 1.

method_transform(_,try_prep_fact(X,A+B=0,C*D=0),all,
 writef('\nPreparing for Factorization to obtain \n\n%t.\n',[C*D=0]), [C*D=0|S]-S,_,Acc,Tot,_) :-
	mod_collect(X,A+B,New),
	check_tidy(New,C*D=0),
	Tot is Acc+1,
	!.

method_transform(_,try_function_stripping(X,Old,Posn,New),all,
 writef('\nApplying Function Stripping to obtain\n\n%t.\n',[New]),[New|S]-S,_,Acc,Tot,_)
	:-
	isolate(Posn,Old,New1),
	tidy(New1,New2),
	mod_weak_normal_form1(New2,expr,X,New),
	Tot is Acc+1,
	!.

method_transform(_,try_collect(X,Old=B,New),part,
 writef('\nCollecting Equation to obtain \n\n%t.\n',[New]),[New|S]-S,_,Acc,Tot,_) :-
	tidy(Old=B,Old1=E),
	recurse_collect(X,Old1,New1),
	Tot is Acc+1,
	tidy(New1=E,New).



method_transform(_,try_attract(X,Old=B,New),part,
 writef('\nAttracting Equation to obtain \n\n%t.\n',[New]),[New|S]-S,_,Acc,Tot,_) :-
	recurse_attract(X,Old,New1),
	Tot is Acc + 1,
	tidy(New1=B,New).



method_transform(_,try_poly(X,Old,New),all,
 writef('\nUsing Polynomial methods to obtain\n\n%t.\n',[New]),[New|S]-S,_,Acc,Tot,_) :-
	!,  % Nothing else should be tried
	poly_solve(Old,X,Ans,_),
	Tot  is Acc + 1,
	tidy(Ans,New).



method_transform(_,try_chunk(X,Old,New,Term),all,true,S-S1,_,Acc,Tot,Bound) :-
	try_chunk(X,Old,New,Term,S-S1,Acc,Tot,Bound),
	!.


 % Log methods
method_transform(_,remove_logs(X,New,Mid,Base),all,
 writef('\nApplying Log method, base %t, to obtain\n\n%t.\n',[Base,New]),[New|S]-S,_,Acc,Tot,_) :-
 	log_reduce(Mid,X,Base,New1),
	Tot is Acc + 1,
	weak_normal_form(New1,X,New),
	!.

method_transform(_,try_homog(X,Eqn,Set,New),all,
 writef('\nApplying Homogenization to obtain \n\n%t.\n',[New]),[New|S]-S,_,Acc,Tot,_) :-
	homog(Eqn,X,Set,HomEqn),
	tidy(HomEqn,New1),
	Tot is Acc+1,
	weak_normal_form(New1,X,New),
	!.

method_transform(_,remove_nasty(X,Old,New),all,
 writef('\nApplying Nasty method to obtain\n\n%t.\n',[New]),[New|S]-S,_,Acc,Tot,_) :-
	nasty_method(Old,X,New1),
	Tot is Acc + 1,
	weak_normal_form(New1,X,New),
	!.

% New methods without schemas
m_t1(X,New1,_,_,NewMethod,New,S-S1,gag,Acc,Tot,Bound) :-
	step_solve_eqn1(no,New1,X,NewMethod,New,S-S1,_,Acc,Tot,Bound),
	!.

m_t1(X,New1,Name,Method,NewMethod,New,S-S1,v,Acc,Tot,Bound) :-
 writef('\nUsing rule %t to apply new method,\n%t, to obtain\n\n%t.\n',
[Name,Method,New1]),
	paraphrase_goal('Trying',NewMethod,Method),
	(step_solve_eqn(no,New1,X,NewMethod,New,S-S1,Acc,Tot,Bound);
	paraphrase_goal('Failed',NewMethod,Method),fail),
	!.

 % Change of Unknown

try_chunk(X,Old,Ans,Term,[Var=Term,NewEqn|S]-S1,Acc,Tot,Bound) :- 
	!,
	not too_many_steps(Old,X,Acc,Bound),
	identifier(Var),
	mod_subst_mesg(Term=Var, Old, NewEqn),
	sort_solve(NewEqn,Var,NewAns,_,S-[XAns|S2],Acc,NewAcc,Bound),
	mod_subst_mesg(Var=Term, NewAns, XAns),
	not too_many_steps(XAns,X,NewAcc,Bound),
	sort_solve(XAns,X,Ans,_,S2-S1,NewAcc,Tot,Bound).

 % Do disjunctions properly
disjunct_solve(_,[],_,Ans,Acc,S-S,T,T,_) :- !,tidy_up_disjunction(Acc,Ans).
disjunct_solve(Type,[A|B],X,Ans,Acc,[A|S]-S1,LAcc,Tot,Bound) :-
	not too_many_steps(A,X,LAcc,Bound),
	(Type=no;
	writef('\nSolving factor %t.\n',[A])),
	sort_solve(A,X,Ans1,_,S-S2,LAcc,NewLAcc,Bound),
	not too_many_steps(B,X,NewLAcc,Bound),
	tidy(Acc#Ans1,NewAcc),
	disjunct_solve(Type,B,X,Ans,NewAcc,S2-S1,NewLAcc,Tot,Bound).



% Get number of steps in best solution so far.  


get_bounds(X,Eqn,Bound) :-
	subst(X='$var'(x),Eqn,VarE),
	new_test(X,Eqn,Test),
	get_bounds_cont(VarE,Test,Bound),
	report_bound(Bound),
	!.

% Exact equation
get_bounds_cont(VarE,Test,bound(exact,NewLowBound)) :-
	setof(Bound,g_b_c(Test,VarE,Bound),BoundBag),
	least_el(BoundBag,LowBound),
	NewLowBound is LowBound + 1,
	!.

% Others
get_bounds_cont(_,Test,bound(average,Bound)) :-
	setof(EBound,g_b_c1(Test,EBound),BoundBag),
	!,
	find_bound_and_average(BoundBag,Average,N,HighBound),
	great_el([N,2],M),
	AddOn is Average/M,
	IntAddOn is integer(AddOn),
	Bound is HighBound + IntAddOn+1,
	!.

% Use large number if no data recorded yet
get_bounds_cont(_,_,bound(none,100)) :- !.

report_bound(bound(none,_)) :- !.
report_bound(bound(Type,Bound)) :-
	(Type=exact ->W=this,W1='';(W=similar,W1=s)),
	writef('\n [Setting step limit to %t
 (based on previous experience of %w equation%w)]\n',[Bound,W,W1]),
	!.



g_b_c(Test,Var,Bound) :-
	c_d(Test,Eqn,_,_,Bound),
	number(Bound),
	match(Var,Eqn).


g_b_c1(Test,Bound) :-
	c_d(Test,_,_,_,Bound),
	number(Bound).


find_bound_and_average([H|T],Average,Number,HighBound) :-
	f_b_a(T,Total,H,Number,1,HighBound,H),
	Average is Total/Number,
	!.

 % Combination of length/2,sumlist/2 and great_el/2.
f_b_a([],Total,Total,Number,Number,High,High) :- !.
f_b_a([H|T],Total,TAcc,Number,NAcc,HighBound,HSoFar) :-
	NewTAcc is TAcc + H,
	New is NAcc + 1,
	(H < HSoFar -> NewHSoFar = HSoFar;NewHSoFar=H),
	!,
	f_b_a(T,Total,NewTAcc,Number,New,HighBound,NewHSoFar).

 % Check for same equation as before

suitable_schema(Eqn,X,Sc,Eqn1,Type,Flag,Name,AC) :-
	tidy_last_equation(Eqn),	% Stops schemas being considered at 
	known_method_schema(_,_,_,_,_,_,_,_,_), % than top level other
	all_schemas_check,
	subst(X='$var'(x),Eqn,VarE),
	new_test(X,Eqn,Test),
	suitable_schema_cont(Eqn,X,Sc,Eqn1,Type,Flag,Name,AC,VarE,Test),
	!.
suitable_schema_cont(Eqn,X,S,Eqn1,Type,win(Name),Name,AC,VarE,Test) :-
	known_method_schema(X,Eqn,_,Name,schema(S,Eqn1,Y),Type,_,_,_),
	flag(method(Name),on,on),
	not schema_used(Name),
	check_all_associated_equations(Test,Y,Eqn1,VarE,Name,AC),
	!.
	
suitable_schema_cont(Eqn,X,Schema,Eqn1,Type,Flag,Name,no,VarE,Test) :-
	all_schemas_check,
	setof(p_s(Schema1,Name1,Type1),
		possible_schema(X,Eqn,Schema1,Name1,Type1,VarE,Test),Bag),
	action_on_pick(Bag,Schema,Name,Type,Flag,Eqn1,Test),
	!.

% Equation that generated schema matches current equation
check_all_associated_equations(_,Y,Eqn1,VarE,_,exact) :-
	subst(Y='$var'(x),Eqn1,VarE1),
	match(VarE,VarE1),
	!.

% An equation that has been successfully solved with this schema
% matches the given equation

check_all_associated_equations(Test,_,_,VarE,method(Name),found) :-
	c_d(Test,Eqn,Name,s,_),
	match(VarE,Eqn),
	!.

write_informative_message(exact,_) :- !,
	writef('which was generated by this equation originally\n').
write_informative_message(found,_) :- !,
	writef('which has already been used to solve this equation\n').
write_informative_message(_,Eqn) :- !,
	writef('generated for equation\n\n%t.\n',[Eqn]).

% all_schemas_check checks to see if LP is allowed to use more than
% one schema.  If not, it checks to see if a schema has already been used.

all_schemas_check :-
	flag(allschemas,yes,yes),
	!.
all_schemas_check :-
	not schema_used(_).

 % Schema method is possible if its preconds are satisfied and if
 % it hasnt failed with this equation before and is same type
possible_schema(X,Eqn,schema(S,E,U),Name,Type,VarEqn,[H|Test]) :-
	known_method_schema(X,Eqn,_,method(Name),schema(S,E,U),Type,_,Pre,_),
	flag(method(method(Name)),on,on),
	not schema_used(Name),
	not (
		c_d([_|Test],VarEqn1,Name,f(_),_),
		match(VarEqn,VarEqn1)
	),
	new_test(U,E,E1Type),
	E1Type = [H|Test],
	schema_check_cond(Pre).


 % Only one schema passes, could check more but wont for now.
action_on_pick([p_s(schema(S,Eqn1,Y),N,T)],S,N,T,only(N),Eqn1,_) :- !.

 % Use tie break, if no schemas pass, don't use any
 % Use strongest test (function symbols as well.  If no schema passes, 
 % bagof will fail, and next clause is tried
action_on_pick(Bag,S,N,T,choice(N),Eqn1,Test) :-
	bagof(Votes,schema_votes(Test,Votes,Bag),VotesList),
	!,
	action_on_pick_cont(VotesList,S,Eqn1,N,T,Bag).

% As above, but function symbol stuff isnt used
action_on_pick(Bag,S,N,T,choice(N),Eqn1,[_|Test]) :-
	bagof(Votes,Y^schema_votes([Y|Test],Votes,Bag),VotesList),
	!,
	action_on_pick_cont(VotesList,S,Eqn1,N,T,Bag).

action_on_pick_cont(VotesList,S,Eqn1,N,T,Bag) :-
	great_el(VotesList,Winner),
	Winner >= 0,
	correspond(p_s(schema(S,Eqn1,_),N,T),Bag,VotesList,Winner),
	!.

schema_votes(Test,Vote,Bag) :-
	member(p_s(schema(_,_,_),Name,_),Bag),
	get_type_vote(Name,Test,p,PosVote),
	get_type_vote(Name,Test,n,NegVote),
	calculate_vote(PosVote,NegVote,Vote),
	!.

calculate_vote(0,0,0) :- !.
calculate_vote(Pos,Neg,Vote) :-
	Vote is (Pos-Neg)/(Pos+Neg),
	!.


get_type_vote(Schema,Test,Type,Vote) :-
	vote(Schema,Test,Type,Vote),
	!.

get_type_vote(_,_,_,0) :- !.

new_test(X,Eqn,[Bag|Test]) :-
	test(X,Eqn,Test),
	find_functions1([Eqn],X,check,List-[]),
	list_to_bag(List,Bag),
	!.

test(X,Eqn,[Occ,ZeroRhs,DomFunc]) :-
	occ(X,Eqn,Occ),
	type_of_rhs(Eqn,ZeroRhs),
	principle_functor(Eqn,DomFunc),
	!.

type_of_rhs(_=0,0) :- !.
type_of_rhs(_=_,n) :- !.
type_of_rhs(_,other) :- !.
principle_functor(A=_,func(F,N)) :- !,
	functor(A,F,N).
principle_functor(_#_,#).


show_initialize_and_store(X,Eqn,New) :-
	mod_abolish(current_equation,1),
	mod_abolish(bad_schema,1),
	mod_abolish(asserted,1),
	mod_abolish(schema_used,1),
	mod_abolish(last_equation,1),
	mod_abolish(already_said,4),
	mod_abolish(store_we,1),
	mod_abolish(tidy_last_equation,1),
	asserta(last_equation(Eqn)),
	equation_string(solving,Eqn),
	writef('\nSolving %t for %t.\n',[Eqn,X]),
	tidy(Eqn,New1),
	find_functions([New1],X,check),
		(match_check(Eqn,New1);writef('\nTidying to %t.\n',[New1])),
	mod_weak_normal_form(New1,X,New),
	asserta(tidy_last_equation(New)),
	reset,
	mod_abolish(schema_abandon,3),
	!.


 % Get next method and postcond from auto method
auto_next_applicable_method(Method,NewMethod,X,New1,Rest) :-
	known_method_auto(_,_,_,method(Method),_,_,
[applicable_next_method(NewMethod,X,New1)|Rest]),
	!.

get_next_element([[H|T]|T1],H,[T|T1]) :- !.
get_next_element([H|T],H,T) :- !.

 % Try to apply indicated schema step

suitable_step(X,Eqn,Purpose,Name,New,Steps,Flag,Acc,Tot,Bound) :-
	not too_many_steps(Eqn,X,Acc,Bound),
	arg(1,Purpose,Name1),
writef('\nTrying indicated schema step of applying %w.\n',[Name1]),
	suitable_step1(X,Eqn,Purpose,Name,New,Steps,Flag,Name1,Acc,Tot,Bound),
	!.

suitable_step1(X,Eqn,_,Name,New,Steps,win,Name,Acc,Tot,Bound) :-
	not too_many_steps(Eqn,X,Acc,Bound),
	step_solve_eqn(yes,Eqn,X,Name,New,Steps,Acc,Tot,Bound),
	!.

 % Try to apply one that satisfies required constraints
suitable_step1(X,Eqn,conditions(_,Sat,UnSat),Name,New,Steps,Flag,N1,Acc,Tot,Bound) :-
		not too_many_steps(Eqn,X,Acc,Bound),
 writef('\nFailed to apply schema step %w.\n',[N1]),
	expand_constraints(Sat,UnSat),
 suitable_methods(X,Eqn,Sat,UnSat,Name,New,Steps,Flag,N1,Acc,Tot,Bound),
	   !.

expand_constraints(sat(Sat,_,_),unsat([],_,_)) :-
	writef('\nNo new constraints are satisfied by schema step.\n'),
	(Sat = [];single_plural(Sat,S,_),
	writef('\nAny new step must still satisfy the following 
	precondition%w\n',[S]),
	explain_g1(Sat)),
	!.

expand_constraints(sat(Sat,_,_),unsat(UnSat,_,_)) :-
	UnSat \== [finish],
	single_plural(UnSat,W,_),
	writef('\nTrying to find a method whose result satisfies the
following precondition%w\n', [W]),
	explain_g1(UnSat),
	(Sat=[];single_plural(Sat,W1,_),
	writef('\n\nwhile keeping the following condition%w true\n',[W1]),
	explain_g1(Sat)),
	!.

suitable_methods(X,Eqn,Sat,UnSat,Name,New,Steps,win,N1,Acc,Tot,Bound) :-
	find_satisfy_unsat(UnSat,Name),
	not bad_name(Name),
	Name \== N1,
	maintain_satisfy(Sat,Name),
	flag(method(Name),on,on),
	step_solve_eqn1(yes,Eqn,X,Name,New,Steps,Mess,Acc,Tot,Bound),
	report_mod_check_cond(UnSat,New,X,Name),
	call(Mess),
	!.

suitable_methods(X,Eqn,Sat,UnSat,Name,New,Steps,no_win,N1,Acc,Tot,Bound) :-
	arg(1,UnSat,Arg),
	(Arg =[_|_] -> 
	writef('\nCan''t find method to help satisfy new conditions.\n');
	Arg=[]),
	arg(1,Sat,Arg1),
	Arg1=[_|_],        % If no satisfied conditions its too unconstrained
writef('\nLooking for method that keeps satisfied conditions satisfied.\n'),
	known_method(_,_,_,Name,_,_,_,_),
	Name \== N1,
	not bad_name(Name),
	flag(method(Name),on,on),
	maintain_satisfy(Sat,Name),
	step_solve_eqn1(yes,Eqn,X,Name,New,Steps,Mess,Acc,Tot,Bound),
	report_mod_check_cond(Sat,New,X,Name),
	call(Mess),
	!.

report_mod_check_cond(UnSat,New,X,Name) :-
	writef('\n[Method %w can be applied\n',[Name]),
	mod_check_cond(UnSat,New,X),
	!,
	writef('and satisfies all the conditions]\n').

mod_check_cond(Term,New,X) :-
	not not (arg(1,Term,List),
	arg(2,Term,Unk),
	arg(3,Term,Eqn),
	New = Eqn,
	X = Unk,
	mod_check_cond1(List)).

mod_check_cond1([]) :- !.
mod_check_cond1([H|T]) :-
	call(H),
	!,
	mod_check_cond1(T).

mod_check_cond1([H|_]) :-  % Only report first failure
	writef('but fails to satisfy (at least) the precondition\n'),
	explain_g1([H]),
	writef('\n]\n'),
	!,
	fail.
	


find_satisfy_unsat(unsat(UnSat,_,_),Name) :- find_satisfy_unsat1(UnSat,Name).
find_satisfy_unsat1([],_) :- !.
find_satisfy_unsat1([H|T],Name) :-
	(must_satisfy(H,List);might_satisfy(H,List)),
	member(Name,List),
	find_satisfy_unsat1(T,Name).

maintain_satisfy(sat(Sat,_,_),Name) :- m_s1(Sat,Name).
m_s1([],_) :- !.
m_s1([H|_],Name) :-
	excludes(H,List),
	member(Exclude,List),
	must_satisfy(Exclude,List1),
	member(Name,List1),
	!,
	fail.

m_s1([_|T],Name) :- m_s1(T,Name).

 % Choose a schema for the factors
choose_member_schema(Eqn,X,S,Schema) :-
	member(S,Schema),
	satisfy_satisfied(X,Eqn,S).

satisfy_satisfied(X,Eqn,[conditions(_,sat(Sat,X,Eqn),_)|_]) :-
	check_cond(Sat),
	!.


decide_message(yes,_,no) :- !.
decide_message(_,gag,no) :- !.
decide_message(_,_,yes) :- !.

bad_name('Factorization') :- !.
bad_name('Change of Unknown') :- !.
bad_name(X) :- schema(X).

no_major_effect(conditions(Name,_,unsat([],_,_))) :- Name \== 'Isolation',!.





 % Store solution steps
store_steps(Eqn,X,Flag,EqnSteps,NewFlag,Length) :-
	maptidy_example(X,[Eqn|EqnSteps],New,sol),
	mod_asserta(store_we(work(New,X))),
	check_found_schema(Eqn,X,Flag,Length),
	modify_schema_flag(Flag,NewFlag).


modify_schema_flag(Old,failed(Name)) :-
	atomic(Old),
	bad_schema(Name),
	!.
modify_schema_flag(failtest,_) :- !.
modify_schema_flag(X,X).

 % There is an old schema that works
check_found_schema(Eqn,Unk,new_flag(Flag,Flag1),L) :- 
	((Flag1 = win,writef('\nTrouble! Whats its name %t\n',[Flag,Flag1]));
	(arg(1,Flag,Name),Flag1=good(Name))),
	!,
	store_concept_data(Eqn,Unk,Name,s,L),
	writef('\n[No problems with schema]\n').


 % Old schema fails
check_found_schema(Eqn,X,new_flag(win(Name),bad),L) :- !,
	store_concept_data(Eqn,X,Name,f(b),L),	
	writef('\n[**Old schema for this problem fails**]\n').

 % No schema before , or rough-match schema failed

check_found_schema(Eqn,X,Flag,L) :-
	((good_flag(Flag),store_concept_data(Eqn,X,none,none,L));
	(arg(1,Flag,Name),
	writef('\n[Rough-match schema failed]\n'),
	store_concept_data(Eqn,X,Name,f(r),L))),
	 writef('\n[Type ''work solution'' to create a schema for this equation]\n').

:- dynamic c_d/5.
store_concept_data(Eqn,X,Name,Type,L) :-
	subst(X='$var'(x),Eqn,VarEqn),
	new_test(X,Eqn,TestResult),
	remove_bits_round(Name,NewName),
	cond_assertz(c_d(TestResult,VarEqn,NewName,Type,L)),
	store_schema_abandons,
	store_votes(NewName,TestResult,Type).

remove_bits_round(method(X),X) :- !.
remove_bits_round(X,X) :- atomic(X),!.
remove_bits_round(Arg,Name) :- arg(1,Arg,method(Name)),!.
remove_bits_round(Term,Name) :- arg(1,Term,Name).

:- dynamic vote/4.

store_votes(Name,TestResult,Type) :-
	type_pos_or_neg(Type,Type1),
	get_previous_vote(Name,TestResult,Type1,Number),
	NewNumber is Number + 1,
	mod_asserta(vote(Name,TestResult,Type1,NewNumber)),
	!.

type_pos_or_neg(s,p) :- !.
type_pos_or_neg(_,n) :- !.


get_previous_vote(Name,Test,Type,Number) :-
	retract(vote(Name,Test,Type,Number)),
	!.
get_previous_vote(_,_,_,0).

good_flag(win).
good_flag(noschema).


store_schema_abandons :-
	schema_abandon(X,Eqn,Name),
	s_s_a1(X,Eqn,Name),
	fail.
store_schema_abandons :- !.

s_s_a1(X,Eqn,Name) :-
	subst(X='$var'(x),Eqn,VarEqn),
	new_test(X,Eqn,TestResult),
	remove_bits_round(Name,NewName),
	cond_assertz(c_d(TestResult,VarEqn,NewName,f(r),fail)),
	store_votes(NewName,TestResult,f(r)).

 % Too many steps checks to see if bound limit has been exceeded
 % If things are fine, this should fail.  First arg is eqn
 % or list from disjuct_solve or factor_solve etc.  If eqn is
 % near solution (dis_solution or single_occ we are not too fussy
 % about the bounds
too_many_steps([H|_],X,SoFar,Bound) :- !,
	too_many_steps(H,X,SoFar,Bound).

too_many_steps([],_,_,_) :- !,fail.
too_many_steps(Eqn,X,SoFar,bound(Type,Bound)) :-
	number(Bound),
	number(SoFar),
	SoFar >  Bound,
	t_m_s_c(Eqn,X,SoFar,Bound),
	!,
	(Type=exact ->W=this,W1='';(W=similar,W1=s)),
	 writef('\n [**Bound Limit Exceeded: Estimated bound of %t steps
 (based on previous experience of %w equation%w.)
 So far this has taken %t**]\n',[Bound,W,W1,SoFar]).

t_m_s_c(Eqn,X,SoFar,Bound) :-
	(occ(X,Eqn,1);dis_solution(Eqn,X)),
	!,
	(already_said(Eqn,X,SoFar,Bound);
	(mod_assert(already_said(Eqn,X,SoFar,Bound)),
	writef('\n [Bound limit of %t has been exceeded (problem has so far
 taken %t).  However, equation appears close to solution, so continuing]\n',
 [Bound,SoFar]))),
	!,
	fail.
t_m_s_c(_,_,_,_).


% "Justified" Hack of sorts
% When checking Preconditions of schemas we should try harder
% to make them satisfied than at other times

schema_check_cond([]) :- !.
schema_check_cond([H|T]) :-
	s_c_c1(H),
	!,
	schema_check_cond(T).

% Only chunk at the moment
s_c_c1(identical_subterms(Eqn,X,Term,New)) :- !,
	hard_identical_subterms(Eqn,X,Term,New).
s_c_c1(X) :- call(X).

