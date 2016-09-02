/*		ATTRAC.AX	  19.2.81   */
/* New axioms added 17.9.81 */
/* AXIOMS FOR ATTRACTION*/
/* FIRST ARGUMENT IS THE SET OF THE VARIABLES ATTRACTED*/

% :- public		attrax/3.

attrax( U & V , U*W+V*W , (U+V)*W ) .

attrax( U & V , W^U*W^V , W^(U+V) ) .

attrax( U & V , log(W,U)+log(W,V) , log(W,U*V) ) .

attrax( U & V , A*log(W,U)+B*log(W,V),log(W,U^A*V^B) ) .

attrax( U & V , A*log(W,U)+log(W,V),log(W,U^A*V) ) .

attrax( U & V , U*log(W,V) , log(W,V^U) ) .

attrax( U & V , log(W,V)*log(U,W) , log(U,V) ) .

attrax( U & V , U=V , U+(-1*V)=0 ) .

attrax( U & V , U>V , U+(-1*V)>0 ) .

attrax( U & V , U>=V , U+(-1*V)>=0 ) .

attrax( V & W , (U^V)^W , U^(V*W) ) .

attrax( U & V , U^(V*W) , (U^V)^W ) .

