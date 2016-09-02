/* POLTID : New simplification code using polynomial simplification

						Leon
						Updated: 9 September 82
*/
%declaration%
% :- public
% 		simplify/2,
% 		simplify/3,
% 		poly_tidy/2.
% 
% :- mode
% 		simplify(+,-),
% 		simplify(+,+,-),
% 		select_letter(-,+),
% 		poly_tidy(+,-),
% 		pol_tidy(+,-).

simplify(Expr,Expr) :- atomic(Expr), !.

simplify(Expr,Simp) :-
	wordsin(Expr,List),
	select_letter(A,List),
	mult_occ(A,Expr),
	is_poly(A,Expr),
	!,
	simplify(Expr,A,Simp).
	
simplify(Expr,Expr).

select_letter(A,[A|_List]).	% Use sorting property of wordsin as heuristic
				% for selecting letter for simplifying

simplify(Expr,Sub,Simp) :-
	poly_norm(Expr,Sub,Pbag),
	poly_tidy(Pbag,Tidy),
	make_poly(Sub,Tidy,Simp).

poly_tidy(Pbag,Tidy) :- pol_tidy(Pbag,Qbag), z_norm(Qbag,Tidy).

pol_tidy([],[]) :- !.

pol_tidy([polyand(N,Expr)|Rest],[polyand(N,Simp)|TidyRest]) :-
	tidy(Expr,Tidy),
	simplify(Tidy,Simp),
	pol_tidy(Rest,TidyRest).
