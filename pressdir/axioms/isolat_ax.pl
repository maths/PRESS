%   File   :  /usr/bs/pressdir/axioms/isolat.ax
%   Author :  PRESS Group
%   Updated: Tue Oct 29 14:26:21 1985
%   Purpose: Isolation axioms

% :- multifile isolax/4.
% :- public
% 		isolax/4.

/* AXIOMS FOR ISOLATION*/
/* FIRST ARGUMENT IS THE VARIABLE ISOLATED*/

/* unary minus */
isolax( 1 , -U=V , U= -1*V , true ).

/* plus */
isolax( 1 , U+V=W , U=W+(-1)*V , true ).
isolax( 2 , V+U=W , U=W+(-1)*V , true ).

/* multiplication */
isolax( 1 , U*V=W , U=W*V1 , non_zero(V) ) :- tidy(1/V,V1).
isolax( 2 , V*U=W , U=W*V1 , non_zero(V) ) :- tidy(1/V, V1).

/* logarithms */
isolax( 1 , log(U,1)=0 , U=N , arbint(N) ).
isolax( 1 , log(U,V)=W , U=V^W1 , non_zero(W) ) :- tidy(1/W,W1).
isolax( 2 , log(U,V)=W , V=U^W , true ) .

/* exponentiation */

isolax( 1 , U^0 = K , U=N , arbint(N) ) :-  K=1,!.

isolax( 1 , U^0=N,false,true) :- eval(N\= -1),
	 trace_press('\nThe equation %t^0 = %t has no real roots\n',[U,N],1),
	 trace_press('\n%t^0  must equal 1.\n',[U],1),
	 !.

isolax( 1 , U^N = 0 , false , true ) :- negative(N),
trace_press('\n%t^%t = 0 has no real roots, %t^%t can not be 0\n',[U,N,U,N],1),
	!.

isolax( 1 , U^N=V , U=V^N1 , odd(N) ) :- tidy(1/N, N1).

isolax( 1 , U^N=V , false , true ) :- negative(V),
	integer(N),
	even(N),
	tidy(1/N,N1),
	trace_press('\nThe equation %t^%t = %t has no real roots\n',[U,N,V],1),
	trace_press('\nas %t^%t is not real\n',[V,N1],1),
	!.


isolax( 1 , U^N=V , U=V^N1 , non_neg(U) & even(N) ) :- tidy(1/N, N1).

isolax( 1 , U^N=V , U=V^N1 # U=(-1)*(V^N1) , even(N) ) :- tidy(1/N, N1).

isolax( 1 , U^A=V, U=V^A1 , \+ number(A) ) :- tidy(1/A,A1).

isolax( 2 , U^V=W , false , true ) :- positive(U),
	eval(W=<0),
trace_press('\n%t^%t = %t has no real roots, %t^%t must be > 0\n',[U,V,W,U,V],1),
	!.

isolax( 2 , U^V=W , V=log(U,W) , true ) .

/* sine */

isolax( 1 ,sin(U)=0,U=180*N,arbint(N)).

isolax( 1 ,sin(U)=1,U=360*N+90,arbint(N)).

isolax( 1 ,sin(U)= -1,U=360*N-90,arbint(N)).

isolax( 1, sin(U)=V,false,true) :- (eval(V>1);eval(V< -1)),
trace_press('\n%t=%t has no real roots, sin must lie in [-1,1]\n',[sin(U),V],1),
	!.

isolax( 1 ,sin(U)=V,U=arcsin(V), acute(U)).

isolax( 1 ,sin(U)=V,U=arcsin(V)#U=180+((-1)*arcsin(V)), non_reflex(U)).

isolax( 1 , sin(U)=V , U=N*180+ (-1)^N*arcsin(V) , arbint(N) ) .

/* cosine */

isolax( 1 ,cos(U)=1,U=360*N,arbint(N)).

isolax( 1 ,cos(U)= -1,U=360*N+180,arbint(N)).

isolax( 1 ,cos(U)=0,U=180*N+90,arbint(N)).

isolax( 1, cos(U)=V,false,true) :- (eval(V>1);eval(V< -1)),
trace_press('\n%t=%t has no real roots, cos must lie in [-1,1]\n',[cos(U),V],1),
	!.

isolax( 1 ,cos(U)=V,U=arccos(V), non_reflex(U)).

isolax( 1 , cos(U)=V , U=2*N*180+arccos(V) #
                           U=2*N*180+ ((-1)*arccos(V)) , arbint(N) ) .

/* tangent */
isolax( 1 , tan(U)=V , U=N*180+arctan(V) , arbint(N) ) .

/* cosecant */
isolax( 1 , cosec(U)=V , U=N*180+ (-1)^N*arcsin(V1) ,
                             arbint(N) ) :- tidy(1/V,V1).

/* secant */
isolax( 1 , sec(U)=V , U=2*N*180+arccos(V1) #
                           U=2*N*180+ ((-1)*arccos(V1)) , arbint(N) ) :- tidy(1/V,V1).

/* cotangent */
isolax( 1 , cot(U)=V , U=N*180+arctan(V1) , arbint(N) ) :- tidy(1/V,V1).

/* inverse sine */
isolax( 1, arcsin(U)=V,false,true) :- (eval(U>1);eval(U< -1)),
	tidy(V,V1),
trace_press('\n%t=%t has no real roots, sin must lie in [-1,1]\n',[arcsin(U),V1],1),
	!.

isolax( 1 , arcsin(U)=V , U=sin(V) , true ) .

/* inverse cosine */
isolax( 1, arccos(U)=V,false,true) :- (eval(U>1);eval(U< -1)),
	tidy(V,V1),
trace_press('\n%t=%t has no real roots, cos must lie in [-1,1]\n',[arccos(U),V1],1),
	!.

isolax( 1 , arccos(U)=V , U=cos(V) , true ) .

/* inverse tangent */
isolax( 1 , arctan(U)=V , U=tan(V) , true ) .

/*inverse cosecant */
isolax( 1 , arccosec(U)=V , U=sin(V1) , true )  :- tidy(1/V,V1).

/* inverse secant */
isolax( 1 , arcsec(U)=V , U=cos(V1) , true ) :- tidy(1/V,V1).

/* inverse cotangent */
isolax( 1 , arccot(U)=V , U=tan(V1) , true ) :- tidy(1/V,V1).

/* sinh  */
isolax( 1, sinh(U)=V,  U=log(e,X),true) :- tidy(V+(V^2+1)^(1/2),X),!.

/* cosh */
isolax( 1, cosh(U)=V,false,true) :- eval(V<1),
	tidy(V,V1),
trace_press('\n%t=%t has no real roots, cosh must be >= 1\n',[cosh(U),V1],1),
	!.


isolax( 1, cosh(U)=V,U=log(e,X) # U=log(e,Y),true) :-
	tidy(V+(V^2-1)^(1/2),X),
	tidy(V-(V^2-1)^(1/2),Y),
	!.
			

/* tanh */
isolax( 1, tanh(U)=V,false,true) :- (eval(V=< -1);eval(V>=1)),
	tidy(V,V1),
trace_press('\n%t=%t has no real roots, tanh must lie in (-1,1)\n',[tanh(U),V1],1),
	!.

isolax( 1, tanh(U)=V,U=log(e,X)*(1/2),true) :- tidy((1+V)*(1-V)^ -1,X),!.

/* cosech */
isolax( 1, cosech(U)=V,U=log(e,X),non_zero(V)) :- 
	tidy(1/V,V1),
	tidy(V1+(V1^2+1)^(1/2),X),
	!.

/* sech */
isolax( 1, sech(U)=V,false,true) :- (eval(V=<0);eval(V>1)),
	tidy(V,V1),
trace_press('\n%t=%t has no real roots, sech must lie in (0,1]\n',[sech(U),V1],1),
	!.


isolax( 1, sech(U)=V1,U=log(e,X) # U=log(e,Y),non_zero(V1)) :- 
	tidy(1/V1,V),
	tidy(V-(V^2-1)^(1/2),Y),
	tidy(V+(V^2-1)^(1/2),X),
	!.

/* coth */
isolax( 1, coth(U)=V, false, true) :- 
	((eval(V>0),eval(V<1));(eval(V<0),eval(V> -1))),
	tidy(V,V1),
trace_press('\n%t=%t has no real roots, coth can not lie in (-1,1)\n',[coth(U),V1],1),
	!.

isolax( 1,coth(U)=V1,U=log(e,X)*(1/2),non_zero(V1)) :-
	tidy(1/V,V1),
	tidy((1+V)*(1-V)^ -1,X),
	!.

/* inverse sinh */
isolax( 1,arcsinh(U)=V,U=sinh(V),true).

/* inverse cosh  */
isolax( 1, arccosh(U)=V,false,true) :- eval(U<1),
	tidy(U,U1),
trace_press('\n%t=%t has no real roots, cosh must be >= 1\n',[arccosh(U1),V],1),
	!.


isolax( 1,arccosh(U)=V,U=cosh(V),true).

/* inverse tanh */
isolax( 1,arctanh(U)=V,false,true) :- (eval(U>=1);eval(U=< -1)),
	tidy(U,U1),
trace_press('\n%t=%t has no real roots, tanh must lie in (-1,1)\n',[arctanh(U1),V],1),
	!.

isolax( 1,arctanh(U)=V,U=tanh(V),true).

/* inverse sech */
isolax( 1, arcsech(U)=V,false,true) :- eval(U>0),eval(U=<1),
	tidy(U,U1),
trace_press('\n%t=%t has no real roots, sech must lie in [0,1]\n',[arcsech(U1),V],1),
	!.

isolax( 1, arcsech(U)=V,U=sech(V), true).

/* inverse cosech */
isolax( 1, arccosech(U)=V,U=cosech(V), true).

/* inverse  coth */
isolax( 1, arccoth(U)=V, false, true) :- 
	((eval(U>0),eval(U<1));(eval(U<0),eval(U> -1))),
	tidy(U,U1),
trace_press('\n%t=%t has no real roots, coth can not lie in (-1,1)\n',[arccoth(U1),V],1),
	!.

isolax( 1, arccoth(U)=V,U=coth(V), true).

