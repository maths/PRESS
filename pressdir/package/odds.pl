%   Arith:Odds.					Updated: 10 September 82
%   Odd and even natural numbers
%   Gcd and factorial calculations.

% :- public odd/1.	:- mode odd(+).
% :- public even/1.	:- mode even(+).
% :- public fact/2.	:- mode fact(+, ?).
% :- public gcd/3.	:- mode gcd(+, +, -).
% :- public gcd_list/2.	:- mode gcd_list(+, -).
% :- public lcm/3.	:- mode lcm(+, +, -).
% :- public lcm_list/2.	:- mode lcm_list(+, -).
% :- public rational_gcd/3.	:- mode rational_gcd(+, +, -).
% :- public rational_gcd_list/2.	:- mode rational_gcd_list(+, -).

odd(X) :-
	eval(odd(X)).

even(X) :-
	eval(even(X)).

gcd(X, Y, Z) :- eval(gcd(X,Y), Z).

lcm(X, Y, Z) :- gcd(X,Y,G), eval(X*Y/G,Z).

gcd_list([H|T],Gcd) :- 		% Takes the gcd of a list of integers
	gcdl(T,H,Gcd).

gcdl(_,1,1) :- !.
gcdl([],Gcd,Gcd) :- !.
gcdl([H|T],Sofar,Gcd) :- gcd(H,Sofar,New), gcdl(T,New,Gcd).

lcm_list([H|T],Lcm)  :-		% Finds the lcm of a list of integers
	lcml(T,H,Lcm).

lcml([],Lcm,Lcm) :- !.
lcml([H|T],Sofar,Lcm) :- lcm(H,Sofar,New), lcml(T,New,Lcm).

rational_gcd(X,Y,Z) :-
	integer(X),
	integer(Y),
	!,
	gcd(X,Y,Z).

rational_gcd(X,Y,Z) :-
	eval(denom(X),X_denom),
	eval(denom(Y),Y_denom),
	eval(numer(X),X_numer),
	eval(numer(Y),Y_numer),
	eval(gcd(X_numer*Y_denom,Y_numer*X_denom),G),
	eval(G/(X_denom*Y_denom),Z).

rational_gcd_list([H|T],Gcd) :- 
	eval(numer(H),N),
	eval(denom(H),D),
	rgl(T,N,D,Gcd).

rgl([],N,D,Gcd) :- eval(N/D,Gcd).
rgl([H|T],N,D,Gcd) :-
	eval(numer(H),N1),
	eval(denom(H),D1),
	eval(gcd(N*D1,D*N1),G),
	eval(D*D1,Dnew),
	rgl(T,G,Dnew,Gcd).


fact(N,Fact) :- N >= 0, fact(N,1,Fact).

fact(0,Fact,Fact) :- !.
fact(N,Sofar,Fact) :- eval(N*Sofar,New), M is N-1, fact(M,New,Fact).

/* Old code
:- public natnum/1.	:- mode natnum(+).
:- public oddnum/1.	:- mode oddnum(+).

natnum(X) :-
	integer(X), X > 0.

oddnum(X) :-
	1 is X mod 2.
*/

