%   File   : TABLE
%   Author : Bernard Silver
%   Updated: 2 October 1984
%   Purpose: Tables of Connection and Conditions for LP

:- dynamic
	must_satisfy/2.

			/* Table of Connections */


must_satisfy(less_occ(_,_,_),['Collection','Prepare for Factorization']).
must_satisfy(identical_subterms(_,_,_),['Homogenization']).
must_satisfy(closer(_,_,_),['Attraction']).
must_satisfy(is_product(_,_),['Prepare for Factorization']).
must_satisfy(rhs_zero(_),['Prepare for Factorization']).
must_satisfy(mult_occ(_,_),['Attraction','Logarithmic Method']).

might_satisfy(single_occ(_,_),['Collection']).
might_satisfy(mult_occ(_,_),['Collection','Nasty Function Method',
'Function Stripping','Polynomial Methods']).
might_satisfy(is_mod_poly(_,_,_),['Collection','Attraction',
	'Nasty Function Method','Logarithmic Method','Function Stripping']).
might_satisfy(rhs_zero(_),['Nasty Function Method','Logarithmic Method',
	'Function Stripping']).
might_satisfy(common_subterms(_,_,_),['Collection','Attraction',
'Nasty Function Method','Logarithmic Method','Function Stripping']).


			/* Table of Conditions */

excludes(single_occ(_,_),[mult_occ(_,_),common_subterms(_,_,_),
	multiple_offenders_set(_,_,_),closer(_,_,_),identical_subterms(_,_,_),
	prod_exp_terms_eqn(_,_,_),dominated(_,_,_)]).

excludes(mult_occ(_,_),[single_occ(_,_)]).
excludes(identical_subterms(_,_,_),[single_occ(_,_)]).
excludes(prod_exp_terms_eqn(_,_,_),[single_occ(_,_)]).
excludes(multiple_offenders_set(_,_,_),[single_occ(_,_)]).
excludes(common_subterms(_,_,_),[single_occ(_,_)]).
excludes(closer(_,_,_),[single_occ(_,_)]).
excludes(dominated(_,_,_),[single_occ(_,_)]).
excludes(is_product(_,_),[is_sum(_,_),is_disjunct(_,_)]).
excludes(is_sum(_,_),[is_product(_,_),is_disjunct(_,_)]).
excludes(is_disjunct(_,_),[is_product(_,_),is_sum(_,_)]).

		% Explain Preconditions
get_explanation(common_subterms/3,'Equation has common additive subterms') :- !.
get_explanation(mult_occ/2,'Unknown occurs more than once in equation') :- !.
get_explanation(single_occ/2,'Unknown occurs exactly once in equation') :- !.
get_explanation(is_mod_poly/3,'Equation is a polynomial') :- !.
get_explanation(rhs_zero/1,'Right hand side of equation is 0') :- !.
get_explanation(is_disjunct/2,'Expression is a disjunction') :- !.
get_explanation(is_product/2,'Left hand side of equation is a product') :- !.
get_explanation(is_sum/2,'Left hand side of equation is a sum') :- !.
get_explanation(dominated/3,'All occurrences of unknown are dominated') :- !.

get_explanation(prod_exp_terms_eqn/2,'Equation is a product of exponentials')
	:- !.
get_explanation(contains_nasties/2,'The equation contains nasty functions') :- 
	!.
get_explanation(identical_subterms/3,'Unknowns occur in identical subterms') :-
	 !.
get_explanation(multiple_offenders_set/3,'There are multiple offending terms')
	:- !.
			% Postconds

get_explanation(less_occ/3,'The number of occurrences of the unknown decreases')
	:- !.
get_explanation(same_occ/3,
'The number of occurrences of the unknown is unchanged') :- !.

get_explanation(closer/3,
'The distance between occurrences of the unknowns decreases') :- !.

get_explanation(less_nasty/3,'The number of nasty functions decreases') :- !.

get_explanation(applicable_next_method/3,
	'The required following method can''t apply') :- !.

 % Add new entry to table
add_to_table(Method,[H|T]) :-
	add_into_table(Method,H),
	!,
	add_to_table(Method,T).


add_to_table(_,[]).
 % Table already has entry for condition
add_into_table(Method,Cond) :-
	must_satisfy(Cond,List),
	!,
	add_into_table1(Cond,List,Method).


 % Start Table entry for condition
add_into_table(Method,Cond) :-
	mod_asserta(must_satisfy(Cond,[Method])),
	!.

 % Entry already contains method
add_into_table1(_,List,Method) :-
	member(Method,List),
	!.

 % Add new member to entry
add_into_table1(Cond,List,Method) :-
	retract(must_satisfy(Cond,List)),
	!,
	mod_asserta(must_satisfy(Cond,[Method|List])).
