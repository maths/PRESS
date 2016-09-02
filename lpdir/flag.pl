%   File   : FLAG
%   Author : Bernard Silver
%   Updated: 8 August 1985
%   Purpose: Flags for LP

:- dynamic
	auto_rule/2,
	known_method_schema/9.


		 % Initialize flags


 % These flag are set by system, 
 % user shouldn't change these
 % (No interface provided)

:-	flag(rules,_,no),     
	flag(number_portray,_,on), 
	flag(text1,_,notused),
	flag(text2,_,notused),
	flag(text2a,_,notused).

 % These are user settable flags, via, enable, disable and flag/2
 % for loop.  The defaults are sensible, but when running in
 % background it may be better to disable loop.

:-	flag(output,_,no),
	flag(loop,_,yes),
	flag(new_methods,_,yes),
	flag(allschemas,no).

 % This flag is set by LP once it has determined the running 
 % environment (suntools and/or emacs), so its default value
 % is not important.  The user may turn clearscreen off if its 
 % on, turning it on when its off probably won't work (The
 % user is warned)

:- flag(clearscreen,_,no).


 % Initialize method flags
:-	flag(method('Isolation'),_,on),
	flag(method('Factorization'),_,on),
	flag(method('Prepare for Factorization'),_,on),
	flag(method('Split Disjunctions'),_,on),
	flag(method('Polynomial Methods'),_,on),
	flag(method('Change of Unknown'),_,on),
	flag(method('Function Stripping'),_,on),
	flag(method('Collection'),_,on),
	flag(method('Attraction'),_,on),
	flag(method('Homogenization'),_,on),
	flag(method('Logarithmic Method'),_,on),
	flag(method('Nasty Function Method'),_,on),
	flag(method(user_rule(_,_,_)),_,on).

	/* remove commands */

r(X) :- remove(X).

remove(X) :- var(X),!,variable_argument(remove).


 % Remove stored rules
remove(rules) :- !,user_macro(is_rules,rules,r_rules).

is_rules :- rules_stored;new_rule_stored(_).

 % Remove specific rules
remove(rules(L)) :- user_macro(rules_stored,rules,remove_rules1(L)).

 % Remove new methods
remove(new(methods)) :- !,
	user_macro(known_method_auto(_,_,_,_,_,_,_),'new methods',r_nm).

remove(schemas) :- !,
	user_macro(schemas, 'schemas',r_s).

% Remove rule of specific name, doesn't use user_macro
remove(rule(Name)) :- 
	rules_stored,
	retract(user_rule(Name,A,B,C,D,E,F)),
	retract_auto_rule(Name),
	!,
	writef('\nRule %t retracted.\n',[Name]),
	still_rules.   	% Check if any rules at all are left

retract_auto_rule(Name) :-
	retract(auto_rule(_,Name)),
	fail.
retract_auto_rule(_) :- !.

remove(rule(Name)) :- !,
	(rules_stored   ->
	writef('\nNo rules of name %t are stored, nothing done.\n',[Name]);
	writef('\nNo rules are stored, nothing done.\n')).

 % Error
remove(X) :- command_error(remove,X).

r_rules :-
	mod_abolish(user_rule,7),
	mod_abolish(auto_rule,2),
	writef('\nUser rules removed.\n'),
	flag(rules,_,no),
	!.


 % Schemas
r_s :- 
	mod_abolish(known_method_schema,9),
	retract(schema(Name)),
	retract(method(Name)),
	fail.

r_s :- 	writef('\nSchema methods removed.\n').

 % New methods
r_nm :-
	mod_abolish(known_method_auto,7),
	mod_abolish(ei,3),
	mod_abolish(para,2),
	mod_abolish(auto_rule,2),
	remove_table_entries,
	remove_new_method_markers,
	writef('\nAll new (non-schema) methods removed.\n'),
	(not schemas;
	writef('\n[Use ''remove schemas'' to remove schema methods.]\n')),
	!.


remove_table_entries :-
	must_satisfy(Cond,List),
	member(method(auto(X)),List),
	delete(auto(X),List,New),
	retract(must_satisfy(Cond,List)),
	assert(must_satisfy(Cond,New)),
	fail.

remove_table_entries.

remove_rules1([]) :- writef('\nFinished.\n').
remove_rules1([H|T]) :- remove(rule(H)),!,remove_rules1(T).



 % Still user rules around?
still_rules :- user_rule(_,_,_,_,_,_,_),!.
still_rules :-  flag(rules,_,no),writef('\n[No rules are left]\n').

 % Define various flags
rules_stored :- flag(rules,yes,yes).

allowed_new_methods :- flag(new_methods,yes,yes).

schemas :- known_method_schema(_,_,_,_,_,_,_,_,_).

nocifer :- flag(clearscreen,no,no).

 % Change flags

en(X) :- enable(X).

enable(Var) :- var(Var),!,variable_argument(enable).

enable([]) :- !.
enable([H|T]) :- !,enable(H),enable(T).

enable(allmethods) :- !, e_a_m.
enable(Flag) :- flag2(Flag,yes),!.
enable(Flag) :- method(method(Flag)),flag2(method(method(Flag)),on),!.
enable(Flag) :- method(Flag),flag2(method(Flag),on),!.
enable(Method) :- short_name(Method,New),!,enable(New).
enable(Error) :- no_such_flag(Error,enable),!.

d(X) :- disable(X).

disable(Var) :- var(Var),!,variable_argument(disable).

disable([]) :- !.
disable([H|T]) :- !,disable(H),disable(T).

disable(allmethods) :- !,d_a_m.
disable(Flag) :- flag2(Flag,no),!.
disable(Flag) :- method(method(Flag)),flag2(method(method(Flag)),off),!.
disable(Flag) :- method(Flag),flag2(method(Flag),off),!.
disable(Method) :- short_name(Method,New),!,disable(New).
disable(Error) :- no_such_flag(Error,disable),!.



flag2(Flag,X) :- flag(Flag,X,X),!,flag_error(X).
flag2(Flag,X) :- flag(Flag,_,X),flag_message(Flag,X),!.



flag_message(loop,warn) :- !,writef('\nLoop checker will now  warn.\n').
flag_message(loop,X) :- 
	(X=yes->Word=on;X=no,Word=off),
	writef('\nLoop checker turned %w.\n',[Word]),
	!.

flag_message(output,X) :- !,
	get_word(X,W),
	writef('\nThe example will now%wbe output.\n',[W]).
flag_message(allschemas,X) :-
	get_word(X,W),
	writef('\nAll schemas will now%wbe considered.\n',[W]).
flag_message(new(methods),X) :- !,
	get_word(X,W),
	writef('\nNew methods are now%wallowed.\n',[W]),
	(X=yes->enable_new_methods;disable_new_methods),
	!.
flag_message(clearscreen,X) :- !,
	give_possible_warning(X),
	get_word(X,W),
	writef('\nThe screen will now%wbe cleared.\n',[W]).

flag_message(method(Method),X) :- !,
	(X=on;X=off),
	method(Method),
	(Method=user_rule(_,_,_) -> N='Apply New Rule';N=Method),
	writef('\nMethod %w turned %t.\n',[N,X]),
	(method_list_on([_|_],_);writef('\n[**No methods are left**]\n')),
	!.

give_possible_warning(yes) :-
	flag(emacs,yes,yes),
	!,
	writef('\n [I think you are running LP under emacs.  If you are,
 clearscreen will not work very well.  Giving it a try none the less...]\n').
give_possible_warning(_).

e_a_m :-
	method_list_on(_,ListOff),
	e_a_m1(ListOff),
	!.

e_a_m1([]) :- !,writef('\nNo methods are disabled, nothing done.\n').

e_a_m1(List) :-
	e_a_m2(List),
	!.

e_a_m2([]) :- writef('\nAll methods have been turned on.\n'),!.
e_a_m2([H|T]) :-
	enable(H),
	!,
	e_a_m2(T).

d_a_m :-
	method_list_on(ListOn,_),
	d_a_m1(ListOn),
	!.

d_a_m1([]) :- !,writef('\nNo methods are disabled, nothing done.\n').

d_a_m1(List) :-
	d_a_m2(List),
	!.

d_a_m2([]) :- writef('\nAll methods have been turned off.\n'),!.
d_a_m2([H|T]) :-
	disable(H),
	!,
	d_a_m2(T).


get_word(yes,' ') :- !.
get_word(no,' not ') :- !.


show_all_flags([]) :- !.
show_all_flags([H|T]) :- 
	flag(H,Old,Old),
	interpret_value(H,Old),
	!,
	show_all_flags(T).

interpret_value(rules,Val) :- !,
	(Val=yes -> W=' ';W=' no '),
	writef('\nThere are%wrules stored.\n',[W]).

interpret_value(loop,Val) :- !,

	writef('\nThe loop flag is set to %t.\n',[Val]).

interpret_value(Flag,Val) :-
	(Val=yes->W=enabled;W=disabled),
	!,
	writef('\nThe %t flag is %w.\n',[Flag,W]).

 % Change loop flag when needed by solve, do nothing if flag is set to no
loop_flag(Value) :-
	flag(loop,Old,Old),
	((Old==no;Old==warn);flag(loop,_,Value)),
	!.

 % Reinitialize runs by removing difficulty marker and stored equations

reset :- 
	mod_abolish(seen_eqn,1),
	mod_abolish(p_m_r,3),
	mod_abolish(warning,2),
	text_not_used,
	mod_abolish(just_created,1),
	(retract(difficult_once(_,_));true),
	!.

 % Abolish asserted markers
reset1 :- mod_abolish(asserted,1),reset.

text_not_used :-
	flag(text1,_,notused),
	flag(text2,_,notused),
	flag(text2a,_,notused),
	!.


 % Flag errors

 % Flag already has the new value
flag_error(X) :- 
	((X=yes; X = on) -> W = enabled;W=disabled),
writef('\n Already %w, nothing done.\n',[W]).

 % No flag or method of that name exists
no_such_flag(Err,Ty) :-
	writef('\n[**No such flag or method as %t, %w ignored**]\n',[Err,Ty]),
	valid_commands(Ty,List),
	output_valid_list(Ty,List).

 % Argument is  a variable
variable_argument(Type) :-
	writef('\n[**Variable in %w command**, command ignored]\n',[Type]).


 % Adding rules to methods
add(new(rule)) :-
	repeat,
	writef('\nWhich method do you want to add a rule to?\n\n'),
	prompt(_,'Method:'),
	read_name(Method1),
	check_exists_method(Method1,Method,Args),
	!,
	add_new_rule_cont(Method,Args).

 % For new methods only
add_new_rule_cont(method(M),Args) :- !,
	process_reply([y,n],add_new_rule1(method(M),Ans,Args),Ans,'Answer:',
writef('\nDo you want to add an already existing rule to %t? (y/n)\n',[M])),
	!.

add_new_rule_cont(M,Args) :-
	add_rule_to_method(M,Args).

add_new_rule1(method(Method),y,_) :- !,
	writef('\nName of Rule?\n'),
	prompt(_,'Name:'),
	read_name(Rule),
	check_exists_rule(Method,Rule),
	mod_asserta(auto_rule(Method,Rule)),
	writef('\nOK\n\n'),
	!.

add_new_rule1(Method,n,Args) :- !,
	add_rule_to_method(Method,Args).

add_rule_to_method(Method,[Old1,New1,Pre,Post]) :-
	repeat,
	writef('\nGive a step where the new rule is used.
Use x as the unknown.  Terminate line with a ''.''\n\n'),
	prompt(_,'First Line:'),
	read_in_line_and_trap(Old),
	tidy_expr(Old,Old1),
	writef('\n%t\n\n',[Old1]),
	prompt(_,'Second Line:'),
	read_in_line_and_trap(New),
	tidy_expr(New,New1),
	writef('\n\n%t\n\n',[New1]),
	mod_check_cond2(Pre,Old),
	mod_check_cond2(Post,New),
	obtain_rule(rule(_,Old1,New1),x,Old1,New1,Method,Name),
	!.


check_exists_method(M,M1,[Old,New,Pre,Post]) :-
	(known_method(x,Old,New,M,Type,_,Pre,Post)-> M1=M;
	known_method(x,Old,New,method(M),Type,_,Pre,Post),M1=method(M)),
	!,
	(Type \= schema(_,_) ; 
	(writef('\n[**Can''t add to schema methods**]\n'),fail)),
	!.
check_exists_method(M,M1,List) :-
	short_name(M,M2),
	writef('\n[Assuming %w]\n\n',[M2]),
	!,
	check_exists_method(M2,M1,List).
check_exists_method(Method,_,_) :-
	writef('\n[**No such method as %t**  Try again]\n',[Method]),
	!,
	fail.

check_exists_rule(Method,Rule) :-
	user_rule(Rule,_,_,_,_,_,_),
	c_e_r1(Method,Rule),
	!.
check_exists_rule(_,R) :-
	writef('\n[**No such rule as %t**]\n',[R]),
	!,
	fail.

c_e_r1(M,R) :-
	auto_rule(M,R),
	writef('\n[*Method %t already has rule %t!*]\n',[M,R]),
	!,
	fail.
c_e_r1(_,_).


mod_check_cond2([],_) :- !.
mod_check_cond2([H|T],Eqn) :-
	call(H),
	!,
	mod_check_cond2(T,Eqn).

mod_check_cond2([H|_],E) :-
	writef('\n[**Equation\n\n%t\n\n
fails to satisfy (at least) the precondition\n\n',[E]),
	explain_g1([H]),
	writef('\n**  Try again.]\n'),
	!,
	fail.


read_in_line_and_trap(X) :-
	read_in_line(X1),
	!,
	(X1=end -> complain;X=X1).

complain :-
	writef('\n[**LP needs an equation here, not a <CR>!!**  Try again]\n'),
	!,
	fail.

	
	
	

