%   File   :  /usr/bs/lpdir/confir.pl
%   Author : Bernard Silver
%   Updated: Tue Oct 15 11:58:39 1985
%   Purpose: Confirming and acting on conjectures

:- dynamic
	rule/4,
	user_rule/7.


 % Ask user to confirm conjectures
confirm_conj(no,_,_,_,_,_,_) :- !.
confirm_conj(Flag,Step,X,A,B,Type,Name) :-
	process_reply([y,n],act_ans(Ans,Step,X,Flag,A,B,Type,Name),
Ans,'Confirm:',writef('\nIs the conjecture correct? (y/n)\n')).

act_ans(y,Step,X,yes,A,B,Type,Name) :- !,
	obtain_rule(Step,X,A,B,Type,Name).

act_ans(y,Step,X,yes1,_,_,_,_) :- !.
act_ans(n,_,_,_,_,_,_,_) :-
	writef('\nCan''t proceed then, entering break.\n'),
	!,
	break,
	fail.

act_ans(a,_,_,_,_,_,_,_) :- reset,writef('\n[Aborting]\n'),abort.


 % Just created new rule accounts for step
obtain_rule(_,X,Old,New,_,Name) :-
	asserted(user_rule(Name,_,_,_,_,_,_)),
	apply_new_rule1(X,Old,New,Name,tl),
	writef('\nRule %w is applicable\n',[Name]),
	!.

 % Ask user for rule
obtain_rule(Step,X,_,_,Type,Name) :-
	rule_text1,
	repeat,
	read_in_rule_conds([Unk1,Rule1,Cond]),
	know_conditions(Cond),
	tidy_withvars(Rule1,Rule),
	not not obtain_rule1(X,Unk,Rule,Cond,Step),
	ob_condition(X,Unk,Rule,Cond,Step,Type,Name),
	!.


 % Fill in conditions if none are given
ob_condition(X,Unk,Rule,Cond,Step,Type,Na) :- 
	!,
	fill_in_values(X,Step,Occ1,Occ2,Cond1),
	obtain_the_rules1(X,Unk,Rule,Cond,Step,Occ1,Occ2,Cond1,Type,Na).

ob_condition(_,_,_,_,_,_,_) :- !,
	writef('\n[**Can not fill in values!!**]\n'),
	!,
	fail.

obtain_the_rules1(X,Unk,L=>R,Cond,Step,Occ1,Occ2,Cond1,Type,Name) :-
	is_type_rule(Type,X,Unk,L=>R,Cond,Step),
	get_functions_from_step(Step,X),
	((var(Type);Type = method(_)) ->
	repeat,
	rule_text3,
	prompt(_,'Name:'),
	read_name(Name),
	check_name(Name),
	!,
	store_rule(Name,Unk,L=>R,Cond,Occ1,Occ2,Cond1,Type);
	true).

get_functions_from_step(rule(_,A,B),X) :-
	find_functions([A,B],X,add).
	

is_type_rule(Type,_,_,L=>R,Cond,_) :-
	(Type == 'Isolation';Type == 'Function Stripping'),
	!,
	write_type('Isolation'),
	mark_mod_asserta((isolax(1,L,R) :- check_cond(Cond))).

is_type_rule(Type,_,Unk,L=>R,Cond,_) :-
	Type == 'Collection',
	check_for_conds_and_var(Unk,L,Cond),
	write_type('Collection'),
	vars_for_collect_set(L,R,Set),
	mark_mod_asserta(collax(Set,L,R)).

is_type_rule(Type,_,Unk,L=>R,Cond,_) :-
	Type == 'Attraction',
	write_type('Attraction'),
	check_unk_in_exp(Unk,L,Type),
	find_attracted_set(L,R,Set),
	mark_mod_asserta((attrax(Set,L,R,Cond))).  %probably wrong!

is_type_rule(Type,X,Unk,L=>R,Cond,rule(_,Old,_)) :-
	Type == 'Homogenization',
	parse(R,S,Unk),
	S= [R1],
	parse(Old,Set,X),
	findtype(T1,Set),
	(T1 = mixed -> T2 = _;T2=T1),
	write_type('Homogenization'),
	mark_mod_asserta((rew_rule(T2,R1,L,R,Unk) :- check_cond(Cond))).	

is_type_rule(_,_,_,_,_,_).

 % Stuff for checking rules
check_for_conds_and_var(Unk,L,Cond) :-
	check_for_conds(Cond),
	check_unk_in_exp(Unk,L,'Collection'),
	!.

check_for_conds([]) :- !.
check_for_conds(_) :-
   writef('\nSorry, can''t store conditions on Collection rule.  Try again\n'),
	fail.
check_unk_in_exp(Unk,L,T) :-
	variables(L,Var),
	my_var_member_check(Unk,Var,T),
	!.

my_var_member_check(_,[],_) :- !.
my_var_member_check(Var,[H|_],T) :-
	H==Var,
	!,
	writef('\nCan''t store variable in %w rule, try again\n\n',[T]),
	fail.

my_var_member_check(Var,[_|T],Ty) :-
	my_var_member_check(Var,T,Ty).
 % Find which vars are Collected in Collection rule

vars_for_collect_set(X,Y,Z) :-
	setof(Var,vars_for_collect(X,Y,Var),Set),
	and_recomp(Set,Z),
	!.

vars_for_collect(X,Y,Var) :-
	variables(X,L),
	variables(Y,L1),
	!,
	member(Var,L),
	member(Var,L1),
	occ(Var,X,N),
	N>1,
	occ(Var,Y,M),
	M<N.

and_recomp([H],H) :- !.
and_recomp([H|T],H&Rest) :-
	!,
	and_recomp(T,Rest).

	
% Find attracted variables
find_attracted_set(L,R,Set) :-
	variables(L,L1),
	variables(R,R1),
	setof(Var,close_var(L1,R1,Var),Set1),
	and_recomp(Set1,Set).

close_var(L1,R1,Var) :-
	member(Var,L1),
	member(Var,R1),
	closeness(Var,L1,C),
	closeness(Var,R1,C1),
	C1 < C.

check_name(Name) :- 
	var(Name),
	!,
	writef('\nYou have used a variable,please try again.\n'),
	fail.

check_name(Name) :-
	user_rule(Name,_,_,_,_,_,_),
	!,
writef('\nThere is already a rule of name %t, please try another name.\n',[Name]),
	fail.

check_name(_).     

 % Store the rules
store_rule(Name,Unk,X,Y,Occ1,Occ2,G,Type) :-
	flag(rules,_,yes),    % Mark that rules are stored
	assert(rule(Name,Unk,X,Y)),
	cond_add_to_list(Type,Name),
	mod_assertz(user_rule(Name,Unk,X,Y,Occ1,Occ2,G)).

cond_add_to_list(Type,_) :- var(Type),!.
cond_add_to_list(method(Name),Name1) :-
	write_type(Name),
	store_rules_list(Name,Name1).
 % Are the conditions understood?
know_conditions([]).
know_conditions([H|T]) :- know_conditions1(H),know_conditions(T),!.

know_conditions1(H) :-
	functor(H,F,N),
	functor(New,F,N),
	current_predicate(F,New),
	!.

know_conditions1(H) :- 
	functor(H,F,N),
	writef('\nThe condition %t/%t is not understood.\n',[F,N]),
	process_reply([b,c,u],act_option(A,H),A,'Option:',option_text),
	!.



act_option(b,H) :- !,
	writef('\n[Entering break]\n'),
	break,
	writef('\n[Leaving break]\n'),
	know_conditions1(H).

act_option(c,H) :- !,
	repeat,
	writef('\nWhich file?\n'),
	prompt(_,'Filename:'),
	read(File),
	file_consult(File,H),
	!.


act_option(u,H) :- !,
	writef('\nConsulting [user].\n'),
	consult(user),
	know_conditions1(H).

file_consult(File,H) :-
	((file_exists(File) -> reconsult(File),know_conditions1(H));
	(writef('\n[**File %w does not exist**] Please try again.\n',[File]),
	!,fail)),
	!.


 %  Fill in Occurrence information
fill_in_values(X,rule(_,Old,New),H1,H2,Cond) :-
	occ(X,Old,H3),
	occ(X,New,H4),
	fill_in_values1(H3,H4,H1,H2,Cond),
	!.

fill_in_values(_,_,_,_,[]).  % Play safe
fill_in_values1(A,A,B,B,[]) :- !. % Old and New have same number of occurrences
				  % Guess this is always the case

fill_in_values1(A,B,C,D,[eval(C>D)]) :-  % Old greater than New, e.g. collect
	A>B,
	!.

fill_in_values1(A,B,C,D,[eval(C<D)]) :- % Old less than new ,e.g sin double 
	A<B,			% angle.
	!.

fill_in_values1(_,_,_,_,[]).  % Just in case! Should never happen


 % Check example if difficulty was found, to see if things are understood now
check_example(X,[Eqn|_],Example) :-
	difficult_once(Eqn,X),
	!,
	check_example1(X,Example).

 % No problem last time, so end the output

check_example(_,_,_) :-  terminate_text.

terminate_text :- 
	writef('\n[Type ''generate problem'' to test the schema]\n'),
	writef('\n[End of Output]\n').

check_example1(X,Example) :-
	process_reply([y,n],check_example2(X,Example,Ans),Ans,'Reply:',
writef('\nDo you want to run the problem again now? Type y or n.\n')).

check_example2(_,_,n) :- !,reset,writef('\nOk.\n'),terminate_text.
check_example2(X,Example,y) :-
	writef('\nRerunning example.\n'),
	initialize_screen,
	work_main(Example,X).


mark_mod_asserta(Rule) :- 
	mod_asserta(Rule),
	mod_asserta(new_rule_stored(Rule)).
