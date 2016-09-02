%   File   :  /usr/bs/lpdir/axioms.pl
%   Author : Bernard Silver
%   Updated: Tue Oct 15 12:02:35 1985
%   Purpose: Axioms for Isolation, Collection and Attraction

:- dynamic isolax/3.   % Just in case we assert one

isolax( 1 , -U=V , U= -1*V).
isolax( 1 , U+V=W , U=W+(-1)*V).
isolax( 2 , V+U=W , U=W+(-1)*V).
isolax( 1 , U*V=W , U=W*V1) :- non_zero(V),tidy(1/V,V1).
isolax( 2 , V*U=W , U=W*V1) :- non_zero(V),tidy(1/V, V1).
isolax( 1 , log(U,1)=0 , U=N) :- arbint(N).
isolax( 1 , log(U,V)=W , U=V^W1) :- non_zero(W),tidy(1/W,W1).
isolax( 2 , log(U,V)=W , V=U^W).
isolax( 1 , U^0 = K , U=N) :- K=1,!,arbint(N).
isolax( 1 , _^0 = _ , false) :- !.
isolax( 2,  A^_ = 0,false) :- non_zero(A).
isolax( 1 , _^N = 0 , false) :- negative(N),!.
isolax( 1 , U^N=V , U=V^N1) :- odd(N),tidy(1/N, N1).
isolax( 1 , U^N=V , U=V^N1) :-  non_neg(U),even(N),tidy(1/N, N1).

isolax( 1 , U^N=V , U=V^N1 # U=(-1)*(V^N1)) :-even(N),tidy(1/N, N1).

isolax( 1 , _^A=V, false) :- negative(V),integer(A),even(A),!.
isolax( 1 , U^A=V, U=V^A1) :- not number(A),tidy(1/A,A1).

isolax( 2 , U^V=_ , false) :- (eval(U>0);atom(U)),eval(V=<0),!.
isolax( 2 , U^V=W , V=log(U,W)) .
isolax( 1 ,sin(U)=0,U=180*N) :- arbint(N).
isolax( 1 ,sin(U)=1,U=360*N+90) :- arbint(N).
isolax( 1 ,sin(U)= -1,U=360*N-90) :- arbint(N).
isolax( 1, sin(_)=V,false) :- (eval(V>1);eval(V< -1)),!.
isolax( 1 , sin(U)=V , U=N*180+ (-1)^N*arcsin(V)) :-  arbint(N).

isolax( 1 ,cos(U)=1,U=360*N) :- arbint(N).
isolax( 1 ,cos(U)= -1,U=360*N+180) :- arbint(N).
isolax( 1 ,cos(U)=0,U=180*N+90) :- arbint(N).
isolax( 1, cos(_)=V,false) :- (eval(V>1);eval(V< -1)),!.
isolax( 1 , cos(U)=V , U=2*N*180+arccos(V) #
                           U=2*N*180+ ((-1)*arccos(V))) :- arbint(N).
isolax( 1 , tan(U)=V , U=N*180+arctan(V)) :- arbint(N).

isolax( 1 , cosec(_)=V , false) :- eval(V> -1),eval(V<1),!.
isolax( 1 , cosec(U)=V , U=N*180+ (-1)^N*arcsin(V1)) :- !,
	tidy(1/V,V1),
	arbint(N).

isolax( 1 , sec(_)=V , false) :- eval(V> -1),eval(V<1),!.
isolax( 1 , sec(U)=V , U=2*N*180+arccos(V1) #
                           U=2*N*180+ ((-1)*arccos(V1))) :- !,
	tidy(1/V,V1),
	arbint(N).

isolax( 1 , cot(U) = 0, U = N*180 + 90) :- !,arbint(N).
isolax( 1 , cot(U)=V , U=N*180+arctan(V1)) :- tidy(1/V,V1),arbint(N).

isolax( 1, arcsin(_)=U,false) :- (eval(U>1);eval(U< -1)),!.

isolax( 1 , arcsin(U)=V , U=sin(V)).
isolax( 1, arccos(U)=_,false) :- (eval(U>1);eval(U< -1)),!.

isolax( 1 , arccos(U)=V , U=cos(V)).
isolax( 1 , arctan(U)=V , U=tan(V)) .
isolax( 1 , arccosec(U)=_ , false) :- eval(U> -1),eval(U<1),!.
isolax( 1 , arccosec(U)=V , U=V1) :- tidy(1/sin(V),V1),!.
isolax( 1 , arcsec(U)=_ , false) :- eval(U> -1),eval(U<1),!.
isolax( 1 , arcsec(U)=V , U=V1) :- tidy(1/cos(V),V1),!.
isolax( 1 , arccot(U)=V , U=V1) :- tidy(1/tan(V),V1),!.


:- dynamic collax/3.  % As above
collax( W , U*W+V*W , (U+V)*W ) .

collax( W , W+V*W , (V+1)*W ) .

collax( W , W+W , 2*W ) .

collax( U&V , (U+V)*(U+ (-1*V)) , U^2+ -1*(V^2) ) .

collax( W , W^U*W^V , W^(U+V) ) .

collax( W , W*W^V , W^(V+1) ) .

collax( W , W*W , W^2 ) .

collax( U , sin(U)*cos(U) , sin(2*U)* (1/2) ) .

collax( U , cos(U)^2+ -1*(sin(U)^2) , cos(2*U) ) .

collax( U , sin(U)*cos(V)+cos(U)*sin(V) , sin(U+V) ) .

collax( U&V , sin(U)*cos(V)+ -1*(cos(U)*sin(V)) , sin(U+ (-1*V)) ) .

collax( U , cos(U)*cos(V)+ -1*(sin(U)*sin(V)) , cos(U+V) ) .

collax( U , cos(U)*cos(V)+sin(U)*sin(V) , cos(U+ (-1*V)) ) .

collax( U , cos(U)^2 + sin(U)^2 , 1 ).

collax( U , log(U,X) + log(U,Y) , log(U,X*Y) ) .

collax( W , log(W,V)*log(U,W) , log(U,V) ) .

collax( U , A*log(U,X) + B*log(U,Y),log(U,X^A*Y^B) ).

collax( U, A*log(U,X) + log(U,Y),log(U,X^A*Y) ).


:- dynamic attrax/4.

attrax( U & V , U*W+V*W , (U+V)*W , []).

attrax(U&V,U*W+V*W1,(U+ -1*V)*W,[ok_number(W),ok_number(W1),eval(-W,W1)]).

attrax( U & V , W^U*W^V , W^(U+V) , [] ).

attrax( U & V , log(W,U)+log(W,V) , log(W,U*V) , [] ).

attrax( U & V , A*log(W,U)+B*log(W,V),log(W,U^A*V^B) , [] ).

attrax( U & V , A*log(W,U)+log(W,V),log(W,U^A*V) , [] ).

attrax( U & V , U*log(W,V) , log(W,V^U) , [] ).

attrax( U & V , log(W,V)*log(U,W) , log(U,V) , [] ).

attrax( U & V , U=V , U+(-1*V)=0 , [] ).

attrax( V & W , (U^V)^W , U^(V*W) , [] ).

attrax( U & V , U^(V*W) , (U^V)^W , [] ).
