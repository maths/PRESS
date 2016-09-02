/*			POLPAK		*/

/*	Polynomial arithmetic package 
		Gathered together by Leon 23.2.81    
		Extra methods added 3.4.81
		Guessing roots by remainder theorem by Bernard
		Made compatible with new simplification code
			Last Updated: 6 January 83
*/

%declarations%

% :- public
% 		even_anti_symmetric/1,
% 		even_symmetric/1,
% 		factor_out/3,
% 		guess_list/2,
% 		gcd_powers/2,
% 		is_poly/2,
% 		make_poly/3,
% 		map_add_power/3,
% 		map_div_power/3,
% 		map_reify/3,
% 		odd_anti_symmetric/1,
% 		odd_symmetric/1,
% 		poly/4,
% 		poly_norm/3,
% 		root/2,
% 		sym_transform/2,
% 		z_norm/2.
% 
% :- mode
% 	is_poly(+,+),
% 	poly_norm(+,+,-),
% 	poly(+,+,?,?),
% 	gcd_powers(+,?),
% 	map_div_power(+,+,?),
% 	map_reify(+,+,-),
% 	map_add_power(+,+,?),
% 	z_norm(+,?),
% 	times(+,+,?),
% 	add_poly(+,+,?),
% 	odd_symmetric(+),
% 	odd_anti_symmetric(+),
% 	symmetric(+,+),
% 	even_symmetric(+),
% 	even_anti_symmetric(+),
% 	reconst(+,-),
% 	reconst(+,+,-).
% 	reify(+,+,-).

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

times_poly([],_Bag,[]) :- !.

times_poly(_Bag,[],[]) :- !.

times_poly([polyand(N,E)],S,X) :- timesingl(S,N,E,X), !.

times_poly([polyand(N,E)|R],S,Z) :- 
	timesingl(S,N,E,X), 
	times_poly(R,S,Y),
        add_poly(X,Y,Z),
	!.

timesingl([],_N,_E,[]) :- !.

timesingl([polyand(M,F)|R],N,E,[polyand(X,Y)|Z]) :- 
		eval(M+N,X),
		tidy(F*E,Y),
		timesingl(R,N,E,Z).          

/* Binomial expansion of coefficient bag */

binomial(_Bag, 0, [polyand(0,1)]) :- !.

binomial(Bag, 1, Bag) :- !.

binomial(Sbag, N, Ebag) :- 
		!,
		eval(N-1,N1),
		binomial(Sbag,N1,Ebag1),
		times_poly(Sbag,Ebag1,Ebag).

% Remove any  terms with zero coefficient

z_norm([],[]) :- !.

z_norm([polyand(_N,0)|R],Pnorm) :- z_norm(R,Pnorm), !.

z_norm([polyand(N,A)|R],[polyand(N,A)|Pnorm]) :- z_norm(R,Pnorm).

/* Put in normal form,undo the effect of z_norm   */

denorm([polyand(0,A)],[polyand(0,A)]) :- !.

denorm([polyand(N,A)|R],[polyand(N,A)|R1]) :- denorm1(N,R,R1),!.

denorm1(0,_,[]) :- !.

denorm1(N,[polyand(L,B)|R],[polyand(L,B)|R1]) :- 
	eval(N-1 =:= L),
	!,
	denorm1(L,R,R1).
	
denorm1(N,R,[polyand(M,0)|R1]) :- 
	eval(N-1,M),
	denorm1(M,R,R1).

/* Code to factor out the linear factor x+B  */

factor_out([polyand(N,A)|Plist],B,Qlist) :-
		!,
		eval(N-1,M),
		div_lin(Plist,M,A,B,Qlist).

div_lin([],-1,0,_,[]) :- !.

div_lin([],-1,_,_,_) :- !, trace_press('Division error \n',1).

div_lin([polyand(N,C)|Plist],M,A,B,[polyand(M,A)|Qlist]) :-
		eval(N < M),			% The sparse case
		!,
		eval(M-1,M1),
		tidy(A*B* -1,A1),
		div_lin([polyand(N,C)|Plist],M1,A1,B,Qlist).

div_lin([polyand(N,C)|Plist],M,A,B,[polyand(M,A)|Qlist]) :-
		eval(N = M),		% N should never be greater than M
		!,
		eval(M-1,M1),
		tidy(C - A*B,A1),
		div_lin(Plist,M1,A1,B,Qlist).

/* Evaluate the polynomial represented by Plist,at Val to give Ans  */

poleval(Poly,Val,Ans) :- 
	denorm(Poly,Plist),
	poleval1(Plist,Val,0,Ans), !.

poleval1([polyand(_,A)|R],V,Res,Ans) :- 	% Use Horner's scheme
	eval(Res*V+A,X),			% for polynomial evaluation
	poleval1(R,V,X,Ans),
	!.

poleval1([],_,Ans,Ans) :- !.

root(Poly,Root) :- poleval(Poly,Root,0).

/*Try to guess roots by applying remainder theorem  */

guess_list(Poly,Candidates) :- 
	gcd_coeffs(Poly,M),
	M \= non_rational,
	constant(Poly,K),
	rational_gcd(M,K,K1),
	eval(K/K1,K2),
	allowed_guess(K2,Candidates).

% Find the gcd of all the coefficients, check that all are rational.

gcd_coeffs(Poly,Gcd) :- 
	coeff_list(Poly,List),
	!,
	rational_gcd_list(List,Gcd).

gcd_coeffs(_,non_rational).

coeff_list([],[]) :- !.
coeff_list([polyand(_,L)|T],[L|T1]) :- ok_number(L), coeff_list(T,T1), !.

% The constant term of a polynomial in normal form

constant(Poly,K) :- last(polyand(0,K),Poly), !.

% If Plist has an integer root then root is a factor of constant term 
% divided by the gcd of all the coefficients.

allowed_guess(K,[1,-1|T]) :- factors_of(K,T,2), !.

factors_of(_K,[],10) :- !.

factors_of(K,[M,H|T],M) :- 
	eval(K mod M,N),
	N = 0,
	!,
	eval(-M,H),
	eval(M+1,M1),
	factors_of(K,T,M1).
	
factors_of(K,T,M) :- 
	eval(M+1,M1),
	factors_of(K,T,M1).

/* Reconstitute bag of coefficients into polynomial */

make_poly(X,Bag1,Poly) :- !,
		map_reify(X,Bag1,Bag2),
		reconst(Bag2,Poly).
		
	reconst([],0).
	reconst([H|T],Poly) :-
		reconst(T,H,Poly).
		
		reconst([H|T],Acc,Ans) :-
			!,
			reconst(T,Acc+H,Ans).
		reconst([],Ans,Ans).

/* reify coefficient and power into product */

reify(_X,polyand(0,E),E) :- !.
reify(X,polyand(1,E),Exp) :- !, tidy(E*X,Exp).
reify(X,polyand(N,E),Exp) :- !, tidy(E*X^N,Exp).

/* Reduce symmetric polynomial to one with half the degree */

sym_transform([polyand(N,A)|Plist],NewPoly) :-
	eval(N/2,M),
	build_red(M,0,Plist,Qlist),
	trans([polyand(M,A)|Qlist],NewPoly),
	!.

build_red(M,M,_,[]) :- !.

build_red(M,K,[polyand(N,A)|Plist],[polyand(M1,A)|Qlist]) :-
	eval(M-K-1,M1),
	eval((2*M-K)-1,N),
	!,
	eval(K+1,K1),
	build_red(M,K1,Plist,Qlist).

build_red(M,K,Plist,[polyand(M1,0)|Qlist]) :-
	eval(M-K-1,M1),
	eval(K+1,K1),
	build_red(M,K1,Plist,Qlist).

% Special code which holds for the quartic case

trans([polyand(2,A),polyand(1,B),polyand(0,C)],
	[polyand(2,A),polyand(1,B),polyand(0,D)]) :-  tidy(C-2*A,D),!.

trans(Plist,Plist) :- writef_press('Relevant reduction code not written'),fail.


/* Test if polynomial is symmetric or anti-symmetric */

odd_symmetric([polyand(N,A)|Plist]) :-
		 odd(N),symmetric(N,[polyand(N,A)|Plist]).

even_symmetric([polyand(N,A)|Plist]) :-
		 even(N),symmetric(N,[polyand(N,A)|Plist]).

odd_anti_symmetric([polyand(N,A)|Plist]) :-
		 odd(N),anti_symmetric(N,[polyand(N,A)|Plist]).

even_anti_symmetric([polyand(N,A)|Plist]) :-
		 even(N),anti_symmetric(N,[polyand(N,A)|Plist]).

symmetric(_,[]) :- !.

symmetric(N,[polyand(M,_)]) :-	eval(N/2,M), !.

symmetric(N,[polyand(L,A)|Plist]) :-
		append(Qlist,[polyand(M,A)],Plist),
		eval(M+L,N),
		!,
		symmetric(N,Qlist).

anti_symmetric(_N,[]) :- !.
anti_symmetric(N,[polyand(L,A)|Plist]) :-
		append(Qlist,[polyand(M,B)],Plist),
		eval(M+L,N),
		eval(-A,B),
		!,
		anti_symmetric(N,Qlist).

 % Converted maplists etc

map_reify(_,[],[]) :- !.
map_reify(X,[H|T],[H1|T1]) :- reify(X,H,H1),map_reify(X,T,T1),!.

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
