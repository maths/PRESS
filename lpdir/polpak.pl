%   File   :  /usr/bs/lpdir/polpak.pl
%   Author : Leon Sterling
%   Updated: Tue Oct 15 11:54:31 1985
%   Purpose: 

/*
:- public

		gcd_powers/2,
		is_poly/2,
		make_poly/3,
		map_add_power/3,
		map_div_power/3,
		map_reify/3,
		poly/4,
		poly_norm/3,
		reify/3,
		z_norm/2.

:- mode
	add_poly(+,+,?),
	gcd_powers(+,?),
	map_add_power(+,+,?),
	map_reify(+,+,?),
	poly_norm(+,+,-),
	poly(+,+,?,?),
	reify(+,+,-),
	times(+,+,?),
	z_norm(+,?).

*/

/* Check if Expression is a polynomial */

is_poly(X,X) :- !.

is_poly(X,X^N) :- integer(N), !.

is_poly(X,(X^N)^(-1)) :- integer(N), !.

is_poly(X,E) :- freeof(X,E), !.

is_poly(X,S+T) :- !, is_poly(X,S), is_poly(X,T).

is_poly(X,S*T) :- !, is_poly(X,S), is_poly(X,T).

is_poly(X,S^N) :- !, integer(N), N >= 0, is_poly(X,S).


/* Put polynomials in normal form (succeeds only for polynomials) */

poly_norm(Poly,X,Plist) :- 
	poly(X,Poly,Pbag),
	z_norm(Pbag,Plist).

/* Forms bag of coefficients */

poly(X,X,[polyand(1,1)]) :- !.

poly(X,X^N,[polyand(N,1)]) :- integer(N), !.

poly(X,(X^N)^(-1),[polyand(N1,1)]) :- integer(N), !, eval(-N,N1).

poly(X,E,[polyand(0,E)]) :- freeof(X,E), !.

poly(X,S+T,Ebag) :- 
	!,
	poly(X,S,Sbag), 
	poly(X,T,Tbag),
	add_poly(Sbag,Tbag,Ebag).

poly(X,S*T,Ebag) :- 
	!,
	poly(X,S,Sbag), 
	poly(X,T,Tbag),
	times_poly(Sbag,Tbag,Ebag).

poly(X,S^N,Ebag) :-
	integer(N),
	eval(N > 0),
	!,
	poly(X,S,Sbag),
	binomial(Sbag,N,Ebag).

poly(X,X^K,[polyand(N,1)]) :- !, eval(K,N), ok_number(N).

poly(X,S^K,Bag) :-
	exp_distrib(S^K,Exp),
	poly(X,Exp,Bag).

/* Add two coefficients bags  */

add_poly([],T,T) :- !.

add_poly(S,[],S) :- !.

add_poly([polyand(N,E)|P],[polyand(M,F)|Q],[polyand(N,E)|Y]) :-
		eval(N > M),
		add_poly(P,[polyand(M,F)|Q],Y), 
		!.

add_poly([polyand(N,E)|P],[polyand(M,F)|Q],[polyand(N,Y)|Z]) :-
		eval(N = M),
		add_poly(P,Q,Z),
		tidy(E+F,Y), 
		!.

add_poly([polyand(N,E)|P],[polyand(M,F)|Q],[polyand(M,F)|Y]) :-   %N < M
		add_poly(Q,[polyand(N,E)|P],Y), !.


/* Multiply two coefficient bags - Distributivity of multiplication over
					addition assumed	*/

times_poly([],_,[]) :- !.

times_poly(_,[],[]) :- !.

times_poly([polyand(N,E)],S,X) :- timesingl(S,N,E,X), !.

times_poly([polyand(N,E)|R],S,Z) :- 
	timesingl(S,N,E,X), 
	times_poly(R,S,Y),
        add_poly(X,Y,Z),
	!.

timesingl([],_,_,[]) :- !.

timesingl([polyand(M,F)|R],N,E,[polyand(X,Y)|Z]) :- 
		eval(M+N,X),
		tidy(F*E,Y),
		timesingl(R,N,E,Z).          

/* Binomial expansion of coefficient bag */

binomial(Bag, 0, [polyand(0,1)]) :- !.

binomial(Bag, 1, Bag) :- !.

binomial(Sbag, N, Ebag) :- 
		!,
		eval(N-1,N1),
		binomial(Sbag,N1,Ebag1),
		times_poly(Sbag,Ebag1,Ebag).

% Remove any  terms with zero coefficient

z_norm([],[]) :- !.

z_norm([polyand(N,0)|R],Pnorm) :- z_norm(R,Pnorm), !.

z_norm([polyand(N,A)|R],[polyand(N,A)|Pnorm]) :- z_norm(R,Pnorm).


 % Converted maplists etc

map_add_power(_,[],[]) :- !.
map_add_power(N,[Pterm|P],[Qterm|Q]) :-
	add_power(N,Pterm,Qterm),
	map_add_power(N,P,Q).

add_power(N,polyand(M,Coeff),polyand(MN,Coeff)) :- 
	MN is M+N.

map_div_power(_,[],[]) :- !.
map_div_power(N,[Pterm|P],[Qterm|Q]) :-
	div_power(N,Pterm,Qterm),
	map_div_power(N,P,Q).

div_power(Num,polyand(Power,Coeff),polyand(Newpow,Coeff)) :- 
	Newpow is Power//Num.

gcd_powers([polyand(N,_)|Poly],Gcd) :-
	gcd_poly(Poly,N,Gcd).

gcd_poly(_,1,1) :- !.

gcd_poly([],Gcd,Gcd) :- !.

gcd_poly([polyand(N,_)|Poly],Sofar,Gcd) :-
	gcd(N,Sofar,New),
	gcd_poly(Poly,New,Gcd).


exp_distrib((A*B)^K,Expr) :-
	decomp(A*B,[*|List]),
	exp_distrib_list(K,List,Bag),
	recomp(Expr,[*|Bag]).

exp_distrib_list(_,[],[]) :- !.

exp_distrib_list(K,[S|Rest],[S^K|NewRest]) :-
	exp_distrib_list(K,Rest,NewRest).


/* Reconstitute bag of coefficients into polynomial */

make_poly(X,Bag1,Poly) :- !,
		map_reify(X,Bag1,Bag2),
		recomp(Poly,[+|Bag2]).

/* reify coefficient and power into product */

reify(X,polyand(0,E),E) :- !.

reify(X,polyand(1,E),Exp) :- !, tidy(E*X,Exp).

reify(X,polyand(N,E),Exp) :- !, tidy(E*X^N,Exp).


map_reify(_,[],[]) :- !.
map_reify(X,[H|T],[H1|T1]) :- reify(X,H,H1),map_reify(X,T,T1),!.

