%   File   :  /usr/bs/lpdir/tl.pl
%   Author : Bernard Silver
%   Updated: Tue Oct 15 11:35:43 1985
%   Purpose: Worked example code for LP

:- dynamic 
	asserted/1,
	last_example/2,
	p_m_r/3,
	warning/2.


 % Find steps between lines

 % Run last solution as an example

work(solution) :-
	retract(store_we(X)),
	writef('\nRunning last solution trace.\n'),
	!,
	call(X).

work(solution) :- !,writef('\nNo solution trace is stored.\n').
 
 % Top level (Arg must be non-empty list, and can't just contain a solution)


work(List) :- work(List,x),!.

work([],_) :- !,writef('\nThat''s not an example!\n').
work([_],_) :- 	!,
	writef('That''s not an interesting example!\n').

work([H|T],X) :- !,
	mod_abolish(last_example,1),
	reset1,
	initialize_screen,
	work_main([H|T],X).

work(_,_) :- !,writef('\n[**The example must be a list!**]\n').

work_main([Eqn|Example],X) :-
	statistics(runtime,_),
	equation_string(work,Eqn),
	show_the_example([Eqn|Example],X,Example1),
	find_functions(Example1,X,check),
	work1(Example1,X,Method1,[],Unksteps,[]),
	report_steps(Example1,X,Method1,Method),
	check_contains_solution(Method1),
	conjecture_steps(Example1,X,Unksteps,Conjlist,UnkList,Method,Method2),
	construct_method(Example1,Conjlist,Method2,X,UnkList),
	statistics(runtime,[_,Time]),
	writef('\n[Example took %t milliseconds]\n',[Time]),
	check_example(X,Example1,[Eqn|Example]),
	equation_string(done,Eqn),
	!.

work_main([E|_],_) :-
	statistics(runtime,[_,Time]),
	writef('\nCould not do problem.
[The attempt took %t milliseconds]\n',[Time]),
	equation_string(failed,E).

	% SPECIAL CASES

work1([],_,L,L,L1,L1) :- !.
work1([X=_],X,[message(Var)|L],L,L1,L1) :- !,
	(nonvar(Var);Var=solution),
	!.
work1([false],_,[message(false)|L],L,L1,L1) :- !.
work1([true],_,[message(true)|L],L,L1,L1) :- !.

	% Superficial Factorization, number removed
work1([Z*Y=0,Eqn=0|Rest1],X,[message(removecf)|L1],L3,US,L2) :-
	decomp(Z*Y,[*|List]),
	select_the_number(_,List,_),
	!,
	work1([Eqn=0|Rest1],X,L1,L3,US,L2),
	!.

 % Factorization 
work1([A*B=0,A1|Rest],X,[message('Factorization')|T],L1,UnknownSteps,L2) :-
	is_factorization(X,A*B,A1,DisList),	
	factor_part(DisList,Rest,StepsList),
	!,
	work2(StepsList,X,T,L1,UnknownSteps,L2).
	

 % Disjunction
work1([A#B|Rest],X,[message(Var)|T],L1,Unknownsteps,L2) :-
	dis_solution(A#B,X),
	(nonvar(Var);Var=solution),
	!,
	work1(Rest,X,T,L1,Unknownsteps,L2).

work1([A#B,A|Rest],X,T,L1,UnknownSteps,L2) :-
	ortodot(A#B,C),
	factor_part(C,[A|Rest],StepsList),
	!,
	work2(StepsList,X,T,L1,UnknownSteps,L2).
	
 % Change of unknown
work1([Old,Y=Subs,Neweqn|R],X,[message('Change of Unknown')|T],L1,Unks,L2) :-
	freeof(X,Y),
	contains(X,Subs),
	contains(X,Old),
	identical_subterms(Old,X,Sub),
	subst(Sub=Y,Old,New),
	tidy(New,New1),
	match_check(New1,Neweqn),
	chunk_part(X,Sub,R,CVsteps,SubsSteps),
	!,
	work3(X,Y,Sub,Neweqn,CVsteps,SubsSteps,T,L1,Unks,L2).

 /* Normal case  */

work1([L1,Head|R],X,NewMess,L2,NewUnks,L3) :-
	find_step(L1,Head,X,NewMess,L4,NewUnks,L5),
	writef('\n[Processing]\n'),
	work1([Head|R],X,L4,L2,L5,L3),
	!.

 % Case for examples that don't end with solutions
work1([_],_,[message(nosol)|L1],L1,L,L) :-
	writef('\nNo solution step found.\n').



 % find_step(Eqn1,Eqn2,Unk,_,_,_,_) tries to find
 % the single PRESS step that transforms Eqn1 to Eqn2, with X as the unknown.

find_step(Old,New,X,[message(Name)|L1],L1,L2,L2) :-
	known_method(X,Old,New,Name,all,Goal,PreCond,PostCond),
	check_cond(PreCond),
	check_cond(PostCond),
	find_step_cont(Goal,Name,Old,New),
	!.


find_step(Eqn1=A,Eqn2=B,X,Mess,L1,Unks,L2) :-
	understood_constants(A,B),
	find_step1(Eqn1=A,Eqn2=B,X,Mess,L1,Unks,L2),
	!.

 % Not understood
find_step(_,New,_,[message(fail)|L1],L1,[unknown_steps(New)|L2],L2) :- !.


find_step_cont(Goal,_,_,_) :-
	call(Goal),
	!.

find_step_cont(_,Name,Old,New) :-
	Name \= user_rule(_,_,_),
	mod_assert(
warning(writef('\n\t[Possibly missing rule for method %w.]\n',[Name]),New)
	),
	mod_assert(p_m_r(Name,Old,New)),
	fail.

understood_constants(A,A) :- !.

understood_constants(A,B) :- match_check(A,B),!.

find_step1(Term=_,Term=_,_,L1,L1,L2,L2) :- !.

	
find_step1(Term1=A,Term2=B,X,[message(Name)|L1],L1,L2,L2) :-
	known_method(X,Term1,Term2,Name,part,Goal,PreCond,PostCond),
	check_cond(PreCond),
	check_cond(PostCond),
	find_step_cont1(Goal,Name,Term1=A,Term2=B),
	!,
	call(Goal),
	!.


find_step_cont1(Goal,_,_,_) :-
	call(Goal),
	!.


find_step_cont1(_,Name,Old,Eqn) :-
	Name \= user_rule(_,_,_),
	mod_assertz(
warning(writef('\n\t[Possibly missing rule for method %w.]\n',[Name]),Eqn)
	),
	mod_assert(p_m_r(Name,Old,Eqn)),
	fail.

 % Isolation
try_to_isolate(X,Eqn,New) :-
	position(X,Eqn,Posn),
	isolate(Posn,Eqn,Isol),
	mod_weak_normal_form1(Isol,expr,X,Isol1),
	remove_arbs(Isol1,NewIsol),
	consider_isolation1(NewIsol,New,X),
	!.

 % Isolation is complete
consider_isolation1(NewIsol,New,_) :-
	remove_arbs(New,New1),
	match_check(NewIsol,New1),
	!.

 % Partial isolation has occurred
consider_isolation1(NewIsol,New,X) :-
	occ(X,New,1),
	tidy(New,Tidy),
	position(X,Tidy,Posn),
	isolate(Posn,Tidy,New1),
	remove_arbs(New1,NewEqn),
	match_check(NewEqn,NewIsol),
	!.

 % Collection	
try_collect(X,Eqn1,Eqn2) :-
	tidy(Eqn1,New1),
	collect(X,New1,New),
	collect_check(X,Eqn2,New),
	!.

 % Collection is complete
collect_check(_,Eqn,New) :- match_check(Eqn,New),!.

 % Partial Collection
collect_check(X,Eqn,New) :-
	mult_occ(X,New),
	recurse_collect1(X,New,Normal),
	!,
	collect_check1(X,Normal,Eqn).

collect_check1(_,Normal,Eqn) :- match_check(Normal,Eqn),!.
collect_check1(X,Normal,Eqn) :-
	mult_occ(X,Eqn),
	recurse_collect1(X,Eqn,N1),
	!,
	match_check(N1,Normal).

 % Attraction
try_attract(X,Eqn1,Eqn2) :- 
	tidy(Eqn1,New1),
	attract(X,New1,New),
	match_check(Eqn2,New),
	attract_check(X,Eqn2,New),
	!.

 % Attraction is complete
attract_check(_,Eqn,New) :- match_check(Eqn,New),!.

 % Partial Attraction
attract_check(X,Eqn,New) :-
	mult_occ(X,New),
	recurse_attract1(X,New,Normal),
	!,
	attract_check1(X,Normal,Eqn).

attract_check1(_,Normal,Eqn) :- match_check(Normal,Eqn),!.
attract_check1(X,Normal,Eqn) :-
	mult_occ(X,Eqn),
	recurse_attract1(X,Eqn,N1),
	!,
	match_check(N1,Normal).

 % Function Stripping
try_function_stripping(X,Old,Posn,New) :-
	isolate(Posn,Old,NewIsol),
	mod_weak_normal_form1(NewIsol,expr,X,New1),
	remove_arbs(New1,New2),
	remove_arbs(New,New3),
	match_check(New3,New2),
	!.

 % Prepare for Factorization
try_prep_fact(X,A+B=0,C*D=0) :-
	decomp(C*D,[*|Args]),
	member(Term,Args),
	functor(Term,+,_),
	mod_collect(X,A+B,New),
	tidy(New,New1),
	match_check(New1,C*D),
	!.

 % Polynomial 

try_poly(X,Old,New)  :-
	poly_solve(Old,X,Ans,_),
	poly_step(Old,X,New,Ans),
	!.

/*
 % Can't solve poly, but normal form is the same
try_poly(X,A=B,NL=NR) :-
	poly_norm(A+ -1*B,X,Plist),	
	poly_norm(NL+ -1*NR,X,Plist1),
	!,
	poly_tidy(Plist,Qlist),
	poly_tidy(Plist1,Qlist1),
	check_same_poly(_,Qlist,Qlist1).
*/	

poly_step(_,_,New,Ans) :-
	tidy(Ans,Ans1),
	match_check(Ans1,New),
	!.

 % Solve to solution
poly_step(_,X,A=B,Ans) :-
	poly_solve(A=B,X,Ans1,_),
	tidy(Ans1,Ans2),
	match_check(Ans,Ans2),
	!.

 % New methods


try_auto_method(Method,X,Term1,Term2) :-   
	auto_rule(Method,NewName),
	apply_new_rule1(X,Term1,Term2,NewName,tl),
	!.



remove_logs(X,New,Mid,Base) :-
	log_reduce(Mid,X,Base,New).

try_homog(X,Eqn,Set,New) :-
	homog(Eqn,X,Set,Homog),
	mod_weak_normal_form1(Homog,expr,X,New2),
	match_check(New2,New).

remove_nasty(X,Eqn,New) :-
	nasty_method(Eqn,X,New1),
	mod_weak_normal_form1(New1,expr,X,New2),
	match_check(New2,New).

try_user_rule(X,Term1,Term2,Name) :- 
	apply_new_rule1(X,Term1,Term2,Name,tl),
	!.


 % Enter worked example line by line

give(example) :-
	writef('\nEnter example, line by line, using x as the unknown.\n
Terminate with ''<CR>'' on new line.\n\n'),
	prompt(_,'First line:'),
	get_lines(Example),
	writef('\n
Example has been stored, and can be rerun by typing xredo.\n\n'),
	asserta(last_example(Example,x)),
	work(Example,x).

get_lines(New) :-
	repeat,
	read_in_line(Line),
	!,
	new_mod_tidy_expr(Line,NewLine),
	(NewLine=end;writef('\n\n%t.\n\n',[NewLine])),
	!,
	get_lines1(New,NewLine).


get_lines1([],end) :-
	writef('\nEnd of Example Input.\n'),
	!.

get_lines1([Line|Rest],Line) :-
	prompt(_,'Next line:'),
	get_lines(Rest).


% Dont tidy expressions like 0=0 to true or false.   Hack!

new_mod_tidy_expr(true,true) :- !.
new_mod_tidy_expr(false,false) :- !.
new_mod_tidy_expr(A,B) :- 
	tidy_expr(A,New),
	assign_tidy(A,New,B).

assign_tidy(A,false,A) :- !.
assign_tidy(A,true,A) :- !.
assign_tidy(_,New,New).


 % Run last example again

xredo :-
	last_example(Example,X),
	!,
	writef('\nRerunning Example.\n\n'),
	work(Example,X).

xredo :- writef('\nNo previous example, nothing done.\n\n').

old_xredo :-
	asserted(_),
	!,
	writef('\n[Retracting added facts]\n'),
	o_x1.

old_xredo :- xredo.

o_x1 :-
	retract(asserted(X)),
	retract(X),
	fail.

o_x1 :- xredo.




check_contains_solution(List) :-
	member(message(nosol),List),
	!,
writef('\nNo solution step has been found, the example will not be processed
further.  Please supply a new example with a solution step!\n'),
	fail.

check_contains_solution(_) :- !.


select_the_number(Num,List,Rest) :- select(Num,List,Rest),ok_number(Num),!.

is_factorization(X,A*B,A1,List1) :-
	decomp(A*B,[*|List]),
	remove_safe_divisors(X,List,New),
	maptidy_example(X,New,New1,work),
	recomp(Term,[#|New1]),
	(decomp(A1,[#|List1])->
	recomp(Term1,[#|List1]); Term1 = A1),
	match(Term,Term1),
	!.


ortodot(B,C) :- decomp(B,[#|C]),!.
ortodot(B,[B]).

