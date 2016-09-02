%   File   : OUT
%   Author : Bernard Silver
%   Updated: 8 August 1985
%   Purpose: Output for LP




 % Output routines etc 

 % Report the reasons and get list of method used
report_steps([H|List],X,Method,[start|Good]) :-
	writef('\n\n\tOperator Identification Complete.\n\n
Solving\n\t\t%t\nfor %t\n\nThe steps were as follows.\n',[H,X]),
	report_steps1(List,Method,Mid),
	construct_good_example_list(Mid,Good).
	
report_steps1([],[],[]) :- writef('\n\tEnd of Example.\n'),!.

report_steps1([H|T],[message(M)|Rest],[M|Method]) :-
	output_mess(M),
	warn_if_possible_missing_rule(M,_Warn,H),
 	writef('\n %t.\n',[H]),
	output_solution(Rest,NewRest),
	report_steps1(T,NewRest,Method),
 	!.

 % Check to see if missing rule messages exist for this Eqn

warn_if_possible_missing_rule(fail,Warn,Eqn) :-
	retract(warning(Warn,Eqn)),
	!,
	call(Warn).

warn_if_possible_missing_rule(_,_,_) :- !.



output_solution([],[]).
output_solution([message(M)|T],List) :-
	sol_mess(M),
	!,
	output_solution(T,List).

output_solution(T,T).

sol_mess(false) :- !,
	writef('\n[No (real) solutions]\n').


sol_mess(true) :- !,
	writef('\n[Expression is an identity]\n').

sol_mess(nosol) :- !.

sol_mess(solution) :- !,
	writef('\n[Solution]\n').

output_mess(removecf) :- !,
	writef('\nRemoving constant factor\n').

output_mess(ff) :- !,
	writef('\nSolving first factor\n').

output_mess(nf) :- !,
	writef('\nSolving next factor\n').

output_mess(cve) :- !,
	writef('\nSolving Changed Variable Equation\n').

output_mess(ses) :- !,
	writef('\nSolving Substitution Equation\n').

output_mess(fail) :- !,
	writef('\nStep not understood, continuing processing\n').

output_mess(user_rule(Name,_,_)) :- !,
	writef('\nStep uses new rule %w\n',[Name]).

output_mess(Name) :-
	writef('\nApplying %w\n',[Name]).

  % Check is example should be output
show_the_example(Example,X,Example1) :-
	asserta(last_example(Example,X)),
	(unknown(X);asserta(unknown(X))),
	(flag(output,no,no);
  (writef('\nWorking on the following example, %t is the unknown.\n',[X]),
	output_example(Example))),
	maptidy_example(X,Example,Example1,work),
	!.

  % Output tidied example

output_example([]) :- writef('\n\t[End of Worked Example]\n'),!.
output_example([H|T]) :-
	writef('\n\t%t.\n',[H]),
	!,
	output_example(T).


 % Find difficulties in the example
find_unknown_steps(Example1,Example2,OldUnk,Steps) :-
	get_unknown_steps(OldUnk,List),
	find_unknown_steps1(Example1,Example2,List,Steps),
	!.

get_unknown_steps(OldUnk,[us(Step)|T]) :-
	member(unknown_steps(Step),OldUnk),
	delete_one(unknown_steps(Step),OldUnk,Rest),
	!,
	get_unknown_steps(Rest,T).

get_unknown_steps([],[]) :- !.


find_unknown_steps1(_,_,[],[]) :- writef('\nNo difficulties encountered.\n'),!.

find_unknown_steps1(Example1,Example2,List,Steps) :- 
	find_unknown_steps2(Example1,Example2,List,Steps),
	!.

find_unknown_steps2(_,_,[],[]) :- !.
find_unknown_steps2(Ex1,Ex2,[us(Step)|T],[st(Step1,X,Step,Y)|T1]) :- 
	nmember(Step,Ex1,N),
	nmember(su(Step,Y),Ex2,N),
	M is N - 1,
	nmember(Step1,Ex1,M),
	nmember(su(Step1,X),Ex2,M),
	!,
	find_unknown_steps2(Ex1,Ex2,T,T1).


 % Output for conjectures
write_step(st(A,_,B,_)) :-
	writef('\nWorking on step from \n\n%t\n\nto\n\n%t\n',[A,B]),
	!.

 % Process conjectures for storing
process_diff(re(diff(X)),T1,T2,rule(F,A,B),
conj_mess(F,rule(A,B),T1,T2)) :- 
	functor(X,F,2),
	!,
	arg(1,X,A),
	arg(2,X,B),
	!.
process_diff(diff(X),T1,T2,rule(F,A,B),
conj_mess(F,rule(A,B),T1,T2)) :- 
	functor(X,F,2),
	!,
	arg(1,X,A),
	arg(2,X,B),
	!.

process_diff(re(X),T1,T2,Rule,Conj) :- !,process_diff(X,T1,T2,Rule,Conj).


process_diff(diff(A,B),T1,T2,rule(_,A,B),
conj_mess(equal,rule(A,B),T1,T2)) :- !.

 % Output conjectures

output_diff(re(diff(A,B))) :-
	tidy_up(A=B,C,D),
	writef('\nConjecture that \n\n%t\n\n\t\t = \n\n%t.\n',[C,D]),
	!.

output_diff(re(diff(X))) :-
	functor(X,_,2),
	arg(1,X,A),
	arg(2,X,B),
	tidy_up(A=B,C,D),
	writef('\n%t =>\n\n%t.\n',[C,D]),
	output_diff(diff(X)).

output_diff(diff(X)) :-
	functor(X,F,_),
	writef('\nAppears to be a new %w step.\n',[F]),
	!.

output_diff(diff(A,B)) :- 
	tidy_up(A=B,C,D),
	writef('\nConjecture that \n\n%t\n\n\t\t = \n\n%t.\n',[C,D]),	
	!.


 % Output message telling where the new rule is being stored
write_type(Ty) :- writef('\nRule is being stored as a %w rule.\n',[Ty]).


num_writef(Format,Vars) :-
	numbervars(Vars,0,_),
	writef(Format,Vars),
	fail.

num_writef(_,_).

paraphrase_goal(W,M1,M2) :-
  writef('\n%w to apply the indicated next method of %t,\n %w\n',[W,M2,M1]).

 % Get wording right for disjunct solving
disjunct_writef([X=A,X]) :-
	%	freeof(X,A),  % Should be anyway
	!,
	writef('\n%t = %t is a solution.\n',[X,A]).

 disjunct_writef([A,X]) :- writef('\nSolving %t for %t.\n',[A,X]).

mod_writef(Ans) :-
	writef('\nAnswer is:\n'),
	mod_writef1(Ans).

mod_writef1(A#B) :- !,
	writef('%t  v \n',[A]),
	mod_writef1(B).
mod_writef1(A) :-
	writef('%t\n\n',[A]),
	extra_message(A),
	!.

extra_message(true) :- !,
	writef('\n[Expression is an identity]\n').

extra_message(false) :- !,
	writef('\n[No real values satisfy the equation]\n').

extra_message(_).

  % Get the wording right for the isolate case
mod_iso_trace(false) :- !,
	writef('\nNo real values satisfy the equation.\n').

mod_iso_trace(true) :- !,
	writef('\nExpression is an identity.\n').

mod_iso_trace(New) :- 
	writef('\nIsolating Equation to obtain \n\n%t.\n',[New]).


 % Text for rule input

rule_text1 :-
	flag(text1,used,used),
	!.

rule_text1 :- 
	flag(text1,_,used),
	writef('\n
Enter the rewrite rule, in the form Old => New, terminate with a "."
(You will then be prompted for further details)\n').

rule_text1 :-
	flag(text1,_,notused),
	fail.

rule_text2 :-
	flag(text2,used,used),
	!.

rule_text2 :-
	flag(text2,_,used),
	writef('Terminate with <cr> (Just type <cr> if none.)\n').


rule_text2 :-
	flag(text2,_,notused),
	fail.

rule_text2a :-
	flag(text2a,used,used),
	!.

rule_text2a :-
	flag(text2a,_,used),
writef('Terminate with <cr> (Just type <cr> if no distinguished unknown.)\n').

rule_text2a :-
	flag(text2a,_,notused),
	fail.

rule_text3 :- writef('\nGive a name for this rule (use an atom).\n').
	

 % Text for when conditions are not understood
option_text :-
	writef('\nType


	a  : to abort,

	b  : to break,
	
	c  : to (re)consult the definition from a file,

	u  : to consult user.\n').


 %  Show the steps that led to the solution
solve_steps(L) :- 
	writef('\nSolve steps were as follows:\n'),
	solve_steps1(L).

solve_steps1([]) :- !,writef('\n[End of trace]\n').
solve_steps1([H|T]) :- 
	writef('\n%t\n',[H]),
	!,
	solve_steps1(T).

method_list_on(ListOn,ListOff) :- 
	bagof(X,method1(X),List1),
	submethod_on(List1,ListOn,ListOff).

method1(X) :- method(X),X\=user_rule(_,_,_).

submethod_on([],[],[]) :- !.
submethod_on([Name|T],[Name|T1],T2) :-
	flag(method(Name),on,on),
	!,
	submethod_on(T,T1,T2).

submethod_on([Name|T],T1,[Name|T2]) :- submethod_on(T,T1,T2).



 % Pretty print listings
mod_list_listing([]) :- !.
mod_list_listing([H|T]) :- 
	mod_listing(H),
	!,
	mod_list_listing(T).

mod_listing(X) :-
	current_predicate(X,Goal),
	clause(Goal,Body),
	mod_listing1(Goal,Body),
	fail.

mod_listing(_).

mod_listing1(Goal,true) :- !,
	num_writef('\n%t.\n',[Goal]).

mod_listing1(Goal,Body) :- !,
	num_writef('\n%t :- %t.\n',[Goal,Body]).

 % quick form

m(X) :- mod_listing(X).
m1(X) :-
	flag(number_portray,Old,off),
	m(X),
	flag(number_portray,_,Old).

 % Show Commands 

s(X) :- show(X).

show(X) :- var(X),!,variable_argument(show).

 % Show rules to user
show(rules) :-  !,
	user_macro(is_rules,rules,s_rules_new).

s_rules_new :-
	s_rules_new1,
	s_r_n1.

s_rules_new1 :-
	user_rule(Name,Unk,L=>R,Cond,_,_,_),
	s_r_n(Name,Unk,L,R,Cond),
	fail.
s_rules_new1.

s_r_n(Name,Unk,L,R,Cond) :-
	(Cond=[]-> C = none;C=Cond),
	(occurs_in(Unk,[L,R,Cond]) ->U=Unk;U='_'),
	num_writef('\nRule %w\n%t => %t\n\nUnknown:%t\n\nConditions:%t\n\n',
	[Name,L,R,U,Cond]),
	!.

s_r_n1 :-
	new_rule_stored(_),
	!,
	writef('\nRules added to initial methods\n'),
	s_r_n1a.
s_r_n1.

s_r_n1a :-
	new_rule_stored(Rule),
	num_writef('\n%t\n',[Rule]),
	fail.
s_r_n1a.

 % Show which rules are used by each method

show(rules_list) :- !,
	user_macro(setof(X,method(method(X)),S),'new methods',s_method(S)).

 % Show schemas
show(schemas) :- !,
	user_macro(schemas,schemas,s_k_m_s).

 % Show new methods
show(new(methods)) :- !,
	user_macro(bagof(M,method(method(M)),Bag), 'new methods',s_n_m(Bag)).

 % Show trace to user
show(solve) :- user_macro(store_we(worked(L,_)),trace,solve_steps(L)).

 % Show settings of flags  (Rest of code in FLAG)

show(flags) :-
	writef('\nThe flags are set as follows:\n'),
	show_all_flags([clearscreen,output,loop,new_methods,rules,allschemas]).


% Show which methods are available
show(methods) :- 
	method_list_on(BagOn,BagOff),
	write_bag(enabled,BagOn),
	write_bag(disabled,BagOff),
	!.

write_bag(enabled,[]) :- !,
	writef('\n[**No methods are enabled**]\n').

write_bag(_,[]) :- !,
	writef('\nNo methods are disabled.\n').

write_bag(Word,Bag) :-
	single_plural(Bag,W,W1),
	writef('\nThe following method%w %w %w:\n\n',[W,W1,Word]),
	output_list_write(Bag),
	!.

output_list_write([]) :- !.
output_list_write([H|T]) :-
	writef('\n\t\t%w\n',[H]),
	!,
	output_list_write(T).

 % Illegal show command
show(X) :- !,command_error(show,X).

s_method(Set) :-
	(length(Set,1) -> W=is,W1=' ';W=are,W1='s '),
	writef('\nThe rule list%w%w as follows:\n',[W1,W]),
	s_method1(Set,W1).

s_method1([],Word) :- writef('\n[End of rule list%w]\n',[Word]),!.
s_method1([H|T],W) :-
	(setof(Rules,auto_rule(H,Rules),Set);Set = none),
	writef('\nMethod %t: %t\n',[H,Set]),
	s_method1(T,W).

s_n_m(Bag) :-
	length(Bag,N),
	(N=1->W=' ',W1='s:';W='s ',W1=':'),
	writef('\nThe following new method%wexist%w\n',[W,W1]),
	s_n_m1(Bag).


s_n_m1([]) :- ttynl,!.
s_n_m1([Method|T]) :-
	(((known_method_auto(_,_,_,method(Method),_,_,[A|_]),
	arg(1,A,Name)) ->
	writef('\nMethod %t has applicable next method %w\n',[Method,Name]));
	(schema(method(Method)),
	writef('\nMethod %t is a schema method.\n',[Method]))),
	!,
	s_n_m1(T).

s_k_m_s :- 
	known_method_schema(_X,_Eqn,_,Name,schema(S,E,U),Type,_,_,_),
	writef('\n\t\tSchema Method %t\n\n',[Name]),
	output_schema(S),
	writef('\nType:%w\n\nGenerating Equation:%t\n\nUnknown:%t\n\n',
[Type,E,U]),
	fail.

s_k_m_s.

output_schema(S) :-
	writef('\nMethod                         		Purpose\n'),
	output_schema1(S).

output_schema1([H|T]) :- !,
	o_s2(H),
	writef('\n_______________________________________________________\n'),
	output_schema1(T).

output_schema1([]) :- writef('\n[End of Schema]\n').

o_s2([]) :- !.
o_s2([conditions(H,_,unsat(Cond,_,_))|T]) :-
	assemble_preconds(H,Cond,List),
	make_name_atomic(H,H1),
	writef('\n%25l\t\t<',[H1]),
	write_horiz_list(List),
	!,
	o_s2(T).

write_horiz_list([H]) :- !,
	convert_name_right(H,H1),
	writef('%w>\n',[H1]).
write_horiz_list([H|T]) :-
	convert_name_right(H,H1),
	writef('%w, ',[H1]),
	!,
	write_horiz_list(T).

convert_name_right('finish/0','') :- !.
convert_name_right(X,X).

make_name_atomic(method(H),H) :- !.
make_name_atomic(H,H).

assemble_preconds(M,[],[H]) :-
	(key_method(M) -> H='key method';H=none),
	!.
assemble_preconds(_,List,Conds) :-
	assemble_preconds(List,Conds).

assemble_preconds([],[]).
assemble_preconds([H|T],[Term|T1]) :-
	functor(H,F,N),
	concat(F,/,Mid),
	concat(Mid,N,Term),
	!,
	assemble_preconds(T,T1).

  % Writeout Commands 

w(X) :- writeout(X).

writeout(X) :- var(X),!,variable_argument(writeout).

 % Write the trace to a file


writeout(solve) :- user_macro(store_we(worked(L,_)),trace,w_solve_steps(L)).

 % Write user rules to a file

writeout(rules) :- user_macro(some_rules_stored,rules,w_rules).

writeout(schemas) :- user_macro(schemas,schemas,w_schemas).

writeout(new(methods)) :- !,
	user_macro(new_method_test, 'new methods',w_n_m).

 % Error
writeout(X) :- command_error(writeout,X).

:- dynamic new_rule_stored/1.

some_rules_stored :- (rules_stored;new_rule_stored(_)),!.

new_method_test :-
	known_method_auto(_,_,_,_,_,_,_),
	!.


w_schemas :-
	file_name1(schemas,File),
	tell(File),
	w_schemas_meat,
	told,
	writef('\nSchemas written to file %t.\n',[File]).

w_schemas_meat :-
	writef('\n:- dynamic known_method_schema/9.\n'),
	m1 known_method_schema,
	write_schemas.

write_schemas :-
	schema(X),
	writef('\n:- asserta(schema(%t)). \n',[X]),
	fail.

write_schemas :-
	(new_method_test;add_method_statements),
	!.

w_rules :-
	file_name1(rules,File),
	tell(File),
	w_rules_meat,
	told,
	writef('\nRules written to file %t.\n',[File]).

w_rules_meat :-
	write(':- flag(rules,_,yes).'),
	writef('\n:- dynamic user_rule/7.\n'),
	m1 user_rule,
	mod_list_new_rules.


mod_list_new_rules :-
	new_rule_stored(_),
	writef('\n:- dynamic new_rule_stored/1.\n'),
	!,
	m_l_n_r1.
mod_list_new_rules.

m_l_n_r1 :-
	new_rule_stored(Rule),
	writef('\nnew_rule_stored((%t)).\n',[Rule]),
	writef('\n:- asserta((%t)).\n',[Rule]),
	fail.
m_l_n_r1.


w_solve_steps(L) :-
	file_name1(trace,File),
	tell(File),
	solve_steps(L),
	told,
	writef('\nTrace written to file %t.\n',[File]).

w_n_m :-
	file_name1('new methods',File),
	tell(File),
	w_n_m_meat,
	told.

w_n_m_meat :-
	writef('\n:- dynamic known_method_auto/7.\n'),
	writef('\n:- dynamic auto_rule/2.\n'),
	m1 known_method_auto,
	l auto_rule,
	add_method_statements,
	writef('\n:- mod_abolish(must_satisfy,2).\n'),
	writef('\n:- dynamic must_satisfy/2.\n'),
	l must_satisfy.

add_method_statements :-
	method(method(X)),
	writef('\n:- flag(method(method(%t)),_,on).\n',[X]),
	writef('\n:- asserta(method(method(%t))).\n',[X]),
	fail.
add_method_statements.

dump_stats :-
	dump_stats(_).

dump_stats(File) :-
	c_d(_,_,_,_,_),
	!,
	file_name_var(stats,File,File1),
	tell(File1),
	writef('\n:- dynamic c_d/5.\n'),
	m1 c_d,
	writef('\n:- dynamic vote/4.\n'),
	m1 vote,
	told,
	writef('\nStats  written to file %w\n',[File1]).
dump_stats(_) :-
	writef('\nNo stats stored, nothing done\n'),
	!.

writeall :- writeall(_),!.

writeall(File) :-
	(some_rules_stored;schemas;new_method_test),
	!,
	file_name_var(everything,File,File1),
	tell(File1),
	writef('\n:- no_style_check(single_var).\n'),
	(some_rules_stored -> w_rules_meat;true),
	(schemas -> w_schemas_meat;true),
	(new_method_test -> w_n_m_meat;true),
	writef('\n:- style_check(all).\n'),
	told,
	!,
	writef('\nNew structures written to file %w\n',[File1]).

writeall(_) :-
	writef('\nNo new structures exist, nothing done.\n').

file_name_var(Type,File,File1) :- 
	nonvar(File),
	!,
	file_name_check(File,Type,File1).
file_name_var(Type,File,File) :- file_name1(Type,File).

 % writeout, show or remove Thing, after calling Test to check all is OK
 % Thing can be trace, rules, new methods, or schemas.
user_macro(Test,_,Call) :- call(Test),!,call(Call).
user_macro(_,Thing,_) :- 
	(Thing=trace -> IsAre = is;IsAre=are),
	writef('\nNo %w %w stored, nothing done.\n',[Thing,IsAre]).

 % Illegal arguments
command_error(Type,X) :-
	writef('\n[**%w is not a valid argument in %w command**]\n',[X,Type]),
	valid_commands(Type,List),
	output_valid_list(Type,List).

output_valid_list(Type,List) :-
	writef('\nValid arguments for the %w command are:\n',[Type]),
	output_v_list(List).

output_v_list([]) :- ttynl,!.
output_v_list([H|T]) :-
	writef('\n%w',[H]),
	output_v_list(T).

valid_commands(remove,[rules,'rule(List)','rule(Name)',schemas,
'new methods']).

valid_commands(show,[flags,methods,rules_list,schemas,rules,'new methods',solve]).

valid_commands(writeout,[solve,rules,schemas,'new methods']).

valid_commands(X,[output,clearscreen,'new methods',allmethods,
allschemas,'MethodName'])
	:-
	(X=enable;X=disable),
	!.



process_reply(List,Call,Var,Prompt,Text) :-
	repeat,
	call(Text),
	prompt(_,Prompt),
	get0(Var1),
	name(NewVar,[Var1]),
	process_reply1(List,Call,Var,Var1,NewVar),
	!.

process_reply1(_,_,_,Var,_) :-
	Var >= "A", Var =< "Z",
	skip(10),
	!,
	writef('\nYou have used a variable, please try again.\n'),
	fail.

	
process_reply1([H|List],Call,Var,Var1,NewVar) :-
	((Var1 = 10 -> Var = H,writef('\n[Assuming default value: %w]\n',[H]));
	(skip(10),member(NewVar,[H|List]),Var=NewVar)),
	!,
	call(Call),
	!.


process_reply1(_,_,_,_,a) :- !,writef('\n[Aborting]\n'),abort.
process_reply1(_,_,_,_,b) :-
	!,
	writef('\n[Entering break]\n'),
	break,
	writef('\n[Leaving break]\n'),
	fail.
process_reply1(_,_,_,_,t) :-
	writef('\n[Tracing]\n'),
	trace,
	fail.

process_reply1(L,_,_,_,?) :- !,help_response(L).
process_reply1(L,_,_,_,h) :- !,help_response(L).

process_reply1(L,_,_,_,_) :-
writef('\nNot a valid response!  Use one of \n%l\n\nPlease try again.\n',[L]),
	fail.

help_response(L) :-
	writef('\nValid responses are\n%l\n
[Also: a(bort),b(reak),h(elp),t(race)]\n',[L]),
	!,
	fail.

mod_subst_mesg(T=Var,Old,New) :-
	subst_mesg(T=Var,Old,New),
writef('\n\nApplying substitution %t = %t to obtain\n\n%t\n\n',[Var,T,New]),
	!.

 % Get plural of word is list contains more than one element
single_plural([_],'',is) :- !.
single_plural([_,_|_],'s',are) :- !.



 % Construct a list of Methods without extra comments such as "solution"
construct_good_example_list([],[]) :-!.
construct_good_example_list([M|T],T1) :-
	comment_name(M),
	!,
	construct_good_example_list(T,T1).

construct_good_example_list([H|T],[H|T1]) :- !,
	construct_good_example_list(T,T1).

comment_name(false) :- !.
comment_name(true) :- !.
comment_name(nosol) :- !.
comment_name(solution) :- !.

	% Commands for the cifer terminal

initialize_screen :-
	clear,
	writef('\nRun begins at:  '),
	ttyflush,
	unix(system(date)),
	ttynl.




clear :- 
	(nocifer;
	unix(system(clear));
	true),
	!.
