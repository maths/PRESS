%   File   : Method
%   Author : Bernard Silver
%   Updated: 8 March 1984
%   Purpose: LP methods


:- dynamic
	known_method_auto/7,
	known_method_tl/8,
	known_method2/8,
	method/1,
	method_list/2.

  % known_method is the top level call that checks whether the method is
  % currently enabled by the user.


 % Schema methods

known_method(X,Old,New,Name,schema(S,Type),Call,Pre,Post) :-
	var(New),	% Solve only
	known_method_schema(X,Old,New,Name,S,Type,Call,Pre,Post),
	flag(method(Name),on,on).
	
 % Top level

known_method(X,Old,New,Name,Type,Call,Pre,Post) :-
	known_method1(X,Old,New,Name,Type,Call,Pre,Post),
	flag(method(Name),on,on).


 % 'Top level' methods that should be tried first.  These are 
 % Factorize, Isolatation and Disjunction, Change of Unknown



known_method1(X,Old,New,Name,Type,Call,Pre,Post) :-
	known_method_tl(X,Old,New,Name,Type,Call,Pre,Post).


 % New methods
known_method1(X,Old,New,Name,all,Call,Pre,Post) :- 
	known_method_auto(X,Old,New,Name,Call,Pre,Post).

 %  All other methods
known_method1(X,Old,New,Name,Type,Call,Pre,Post) :-
	known_method_interp(X,Old,New,Name,Type,Call,Pre,Post).

		 /*  THE METHODS  */

known_method_tl(X,Old,Ans,'Split Disjunctions',all,
try_disjunct(X,Old,Ans),[is_disjunct(X,Old)],[]) 
		:- 
	not ground(Ans).
 
known_method_tl(X,Old,New,'Isolation',all,
	try_to_isolate(X,Old,New),[single_occ(X,Old)],[]).

  % Only to be used for solving, hence not ground(Ans)
known_method_tl(X,Old,Ans,'Factorization',all,
try_factorize(X,Old,Ans),[rhs_zero(Old),mult_occ(X,Old),is_product(X,Old)],[])
		:- 
	not ground(Ans).


known_method_tl(X,Eqn,New,'Change of Unknown',all,
try_chunk(X,Eqn,New,Term),[mult_occ(X,Eqn),identical_subterms(Eqn,X,Term)],[]) 
		:-
	not ground(New).

 % known_method_interp tries the lowest priority methods in the order
 % they occur in method_list(MethodList).  This list can be changed by the
 % user.  Different for tl and solve.

known_method_interp(X,Eqn,New,Name,Type,Call,PreCond,Effect) :-
	(ground(New)->Task=tl;Task=sol),
	get_method_list(Task,MethodList),
	member(Name,MethodList),
	known_method2(X,Eqn,New,Name,Type,Call,PreCond,Effect).


known_method2(X,Old,New,'Prepare for Factorization',all,
        try_prep_fact(X,Old,New),
	[is_sum(X,Old),rhs_zero(Old),mult_occ(X,Old),common_subterms(X,Old,_)],
	[is_product(X,New),rhs_zero(New),less_occ(X,Old,New)]).


known_method2(X,Old,New,'Function Stripping',all,
try_function_stripping(X,Old,Posn,New),[dominated(X,Old,Posn)],
	[same_occ(X,Old,New),not dominated(X,New,_)]).

known_method2(X,Old,New,'Polynomial Methods',all,try_poly(X,WNF,New),
	[is_mod_poly(X,Old,WNF)],[]).

known_method2(X,Old,New,'Collection',part,try_collect(X,Old,New),
	[mult_occ(X,Old)],[less_occ(X,Old,New)]).

  % Attract 
known_method2(X,Old,New,'Attraction',part,try_attract(X,Old,New),
	[mult_occ(X,Old)],[same_occ(X,Old,New),closer(X,Old,New)]).


 % Log Method
known_method2(X,Eqn,New,'Logarithmic Method',all,remove_logs(X,New,Mid,Base),
	[mult_occ(X,Eqn),prod_exp_terms_eqn(Eqn,X,Mid)],[]).

 % Homogenization
known_method2(X,Eqn,New,'Homogenization',all,try_homog(X,Eqn,Set,New),
	[mult_occ(X,Eqn),multiple_offenders_set(X,Set,Eqn)],
	[identical_subterms(New,X,_)]).

 % Nasty
known_method2(X,Eqn,New,'Nasty Function Method',all,
remove_nasty(X,Eqn,New),[mult_occ(X,Eqn),contains_nasties(X,Eqn)],
	[less_nasty(X,Eqn,New)]).

 % User Rule  (Work only at present)
known_method2(X,Eqn,New,user_rule(Name,_,_),_,try_user_rule(X,Eqn,New,Name),
	[rules_stored],[]).


method('Isolation').
method('Factorization').
method('Prepare for Factorization').
method('Split Disjunctions').
method('Polynomial Methods').
method('Change of Unknown').
method('Collection').
method('Function Stripping').
method('Attraction').
method('Logarithmic Method').
method('Homogenization').
method('Nasty Function Method').
method(user_rule(_,_,_)).

disable_new_methods :- method(method(X)),flag(method(method(X)),_,off),fail.
disable_new_methods.

enable_new_methods :- method(method(X)),flag(method(method(X)),_,on),fail.
enable_new_methods.


remove_new_method_markers :-
	retract(method(method(X))),
	flag(method(method(X)),_,off),
	fail.
remove_new_method_markers.

get_method_list(Type,List) :- method_list(Type,List),!.
get_method_list(Type,_) :- 
	writef('\n[**Error.  No method_list for type %t present**.]\n',[Type]),
	!,
	fail.

method_list(sol,['Prepare for Factorization','Polynomial Methods',
'Function Stripping','Collection','Attraction','Homogenization',
'Logarithmic Method','Nasty Function Method']).

method_list(tl,['Prepare for Factorization','Polynomial Methods',
'Homogenization','Function Stripping','Collection','Attraction',
'Logarithmic Method','Nasty Function Method',user_rule(_,_,_)]).

 % Key Methods
key_method('Factorization').
key_method('Change of Unknown').
key_method('Isolation').
key_method('Polynomial Methods').
key_method('Split Disjunctions').

 % Change method order in method list

change_method_order :-
	c_m_o(solve,sol),
	c_m_o('worked example',tl).

c_m_o(Word,Type) :-
	writef('\nChange list for %w.\n',[Word]),
	retract(method_list(Type,List)),
	writef('\nCurrent method order is:\n%l\nEnter new order.\n\n',[List]),
	prompt(_,'New List:'),
	repeat,
	read(New),
	check_order(List,New),
	!,
	asserta((method_list(Type,New))).

check_order(Old,New) :- 
	perm(Old,New),
	!,
	writef('\nOK, new order is being stored.\n').

check_order(_,_) :- 
	writef('\n
The new list is not a permutation of the old one, please try again.\n'),
	fail.

short_name(chunk,'Change of Unknown') :- !.
short_name(user_rule,user_rule(_,_,_)) :- !.
short_name(Short,Method) :-
	atom(Short),
	name(Short,Chars), 
	Chars = [F1,F2,F3,F4|ShortName], % Must be at least 4 letters long
	convert_case([F1,F2,F3,F4|ShortName],[L1,L2,L3,L4|_]),
	!,
	method1(Method),
	atom(Method),
	name(Method,MethodName),
	convert_case(MethodName,[L1,L2,L3,L4|_]),
	!.

convert_case([],[]) :- !.
convert_case([H|T],[H1|T1]) :-
	H>="A",
	H=<"Z",
	!,
	H1 is H +32,
	!,
	convert_case(T,T1).
convert_case([H|T],[H|T1]) :-
	convert_case(T,T1).
