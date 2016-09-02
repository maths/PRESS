/*
	Miscellaneous facts for PRESS

						Bernard Silver
						Updated: 9 September 82
*/

/* EXPORTS  */

% :-	public special_atom/1,
% 	commutative/1,
% 	associative/1.

/* MODES   (Defined as used now, may need changing later) */

% :- 	mode special_atom(+),
% 	commutative(+),
% 	associative(+).

 % Special atoms are positive, and therefore non_neg and non_zero
special_atom(e) :- !.

special_atom(pi) :- !.

 % Properties of functions
commutative(+) :- !.
commutative(*) :- !.

associative(+) :- !.
associative(*) :- !.

disjunction(_A#_B).

zero(Rhs) :- tidy(Rhs,0).

mulbag(_A*_B).

plusbag(_A+_B).



