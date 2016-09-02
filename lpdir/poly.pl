%   File   :  /usr/bs/lpdir/poly.pl
%   Author : Leon Sterling
%   Updated: Tue Oct 15 12:02:07 1985
%   Purpose: Polynomial code

% Poly_solve is only called when it has been determined that the
% equation is a polynomial equation.
%  i.e. a precondition that the method is called is that is_poly is true

poly_solve(Eqn1#Eqn2,X,Soln1#Soln2,Rules-Diff) :-
	poly_solve(Eqn1,X,Soln1,Rules-Inter),
	poly_solve(Eqn2,X,Soln2,Inter-Diff).

poly_solve(Lhs=Rhs,X,Soln,[Infer,Mult|Rules]-Diff) :-
	poly_norm(Lhs + -1*Rhs,X,Plist),
	poly_tidy(Plist,Qlist),
	remove_neg_powers(X,Qlist,Poly,Mult),		% Remove negative powers
	poly_method(X,Poly,Soln,Rules-Diff).

remove_neg_powers(X,Plist,Qlist,multiply(Mult)) :-
	last(polyand(N,_),Plist),
	N < 0,
	!,
	eval(-N,N1),
	map_add_power(N1,Plist,Qlist).

remove_neg_powers(_,Plist,Plist,nomult).

/*****************************************/
/* ROUTINES FOR POLYNOMIAL EQUATIONS */
/*****************************************/

/* Identities and unsatisfiable equations */

poly_method(_,[],true,[ident|Diff]-Diff) :- !.	% The polynomial has simplified away

poly_method(X,[Pterm],Ans,[single_term|Diff]-Diff) :-	% Polynomial simplified
	!,						% to a single term
	singleton_method(Pterm,X,Ans).

singleton_method(polyand(0,A),_,true) :-
	simplify(A,B),
	B = 0,
	!.

singleton_method(polyand(0,_),_,false) :- !.

singleton_method(polyand(_,_),X,X = 0) :- !.

/* LINEAR EQUATIONS */

poly_method(X,Poly,X=Ans,[linear|Diff]-Diff) :-
	linear(Poly),
	!,
	linear_method(Poly,Ans,_).

linear([polyand(1,_)|_]) :- !.

linear_method([polyand(N,A)|T],Ans,N) :- 	% Handles disguised linear also
	find1(T,B),
	tidy(-B/A,Ans).

find1([polyand(0,B)],B) :- !.
find1([],0) :- !.		% Shouldn't be needed

/* QUADRATIC EQUATIONS*/

poly_method(X,Poly,Soln,[quadratic|Diff]-Diff) :-
	quadratic(Poly),
	!,
	find_coeffs(Poly,A,B,C),
	discriminant(A,B,C,Discr),
	roots(X,A,B,C,Discr,Soln).

quadratic([polyand(2,_)|_]) :- !.

find_coeffs([polyand(2,A)|T],A,B,C) :- find2(T,B,C).

discriminant(A,B,C,Discr) :- tidy(B^2 - 4*A*C,Discr).

roots(X,A,B,_,0,X = Root) :- 			% Only 1 root
	!, 
	tidy(-B/(2*A),Root),
	!.

roots(X,A,B,C,Discr,X = Root1 # X = Root2) :-
	warn_if_complex(Discr),
	!,
	tidy((-B + Discr^(1/2))/(2*A),Root1),
	tidy((-B - Discr^(1/2))/(2*A),Root2).
roots(_,_,_,_,_,false).

warn_if_complex(Discr)  :-
	eval(Discr < 0),
 writef('\n[Roots are complex.  LP uses only reals so setting to false.]\n'),
	!,
	fail.

warn_if_complex(_).

find2([polyand(1,B),polyand(0,C)],B,C) :- !.
find2([polyand(1,B)],B,0) :- !.
find2([polyand(0,C)],0,C) :- !.
%	find2([],0,0) :- !.	Shouldn't be needed

/* Polynomial divisible by an integral power of the unknown */

poly_method(X,Plist,X = 0 # Ans,[divide(X^N)|Rules]-Diff) :-
	last(polyand(N,_),Plist),
	N > 0,
	!,
	eval(-N,M),
	map_add_power(M,Plist,Qlist),
	poly_method(X,Qlist,Ans,Rules-Diff).

/* Disguised Linear */

poly_method(X,Poly,Soln,[linear|Rules]-Diff) :-
	disguised_linear(Poly),
	!,
	linear_method(Poly,Ans,N),
	isolate([1,1],X^N=Ans,Soln).

disguised_linear([polyand(_,_),polyand(0,_)]).

/* Disguised polynomial equations  */

poly_method(X,Plist,Ans,_) :- 
	poly_hidden(X,Plist,N),		% Disguised polynomial in X^N
	!,
	map_div_power(N,Plist,Qlist),
	poly_method(X^N,Qlist,Inter,Rules-Laws),
	isolate([1,1],Inter,Ans). % Maybe needs poly_isolate

poly_hidden(X,Poly,Gcd) :-
	gcd_powers(Poly,Gcd),
	Gcd > 1,
	!.

% isolate hack until code is reformed
