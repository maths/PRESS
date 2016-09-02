/*		COLLEC.AX	  19.2.81  */
/* AXIOMS FOR COLLECTION*/
/* FIRST ARGUMENT IS THE VARIABLES COLLECTED*/
/* ALL COLLECTION AXIOMS APPLY TO TERMS DOMINATED BY + OR */

% :- public		collax/3.

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

collax( U , A*log(U,X) + B*log(U,Y),log(U,X^A*Y^B) ).

collax( U, A*log(U,X) + log(U,Y),log(U,X^A*Y) ).
