/* MANIP : Manipulation of algebraic expressions

						Leon
						Updated: 10 August 82
*/

exp_distrib(S^K,Expr) :-
	mulbag(S),
	decomp(S,[*|List]),
	exp_distrib_list(K,List,Bag),
	recomp(Expr,[*|Bag]).

exp_distrib_list(_,[],[]) :- !.

exp_distrib_list(K,[S|Rest],[S^K|NewRest]) :-
	exp_distrib_list(K,Rest,NewRest).

mul_distrib(S*K,Expr) :-
	plusbag(S),
	decomp(S,[+|List]),
	mul_distrib_list(K,List,Bag),
	recomp(Expr,[+|Bag]).

mul_distrib_list(_,[],[]) :- !.

mul_distrib_list(K,[S|Rest],[S*K|NewRest]) :-
	mul_distrib_list(K,Rest,NewRest).
