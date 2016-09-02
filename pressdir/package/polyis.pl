/*	POLYIS		31.3.81  */

%declarations%

:- public
		half_poly/3,
		poly_form/2,
		poly_form1/2.

/******************************
	POLYNOMIAL NORMAL FORM
******************************/


/* Use polynomial form for simplification (always succeeds) */

poly_form(true,true).
poly_form(false,false).

poly_form(Exp,Poly) :- !,
	poly_form1(Exp,New),
	tidy(New,Poly).

/* Look for terms to simplify */

poly_form1(Exp,Poly) :-
	Exp=..[Sym|Args], ispred(Sym), !,
	maplist(poly_form1,Args,PArgs),
	Poly=..[Sym|PArgs].

/* Apply to term */

poly_form1(Exp,Poly) :- !,
	wordsin(Exp,Vars),
	sublist(mult_occ(Exp),Vars,Vars1),
	poly_form(Vars1,Exp,Poly).



/* Test for predicate or logical connective */

ispred(&).	ispred(#).	ispred(=).
ispred(>).	ispred(>=).	ispred(<).	ispred(=<).



/* Put term in polynomial normal form with respect to list of variables*/

poly_form([],Exp,Exp) :- !.

poly_form([Var|Vars],Exp,Poly) :- !,
	poly(Var,Exp,Ebag1,simp),
	maplist(half_poly(Vars),Ebag1,Ebag2),
	make_poly(Var,Ebag2,Poly).


/* Apply poly_form to coeffs */

half_poly(Vars,polyand(N,E1), polyand(N,E2)) :- !,
	poly_form(Vars,E1,E2).




