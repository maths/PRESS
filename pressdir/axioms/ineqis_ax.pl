%   File   : INEQIS.AX
%   Author : PRESS GROUP, last modified by Bernard Silver
%   Updated: Tue Oct 29 14:29:51 1985
%   Purpose: Isolation Axioms for inequalities

/* NOTE: In Quintus Prolog (version 1.5) this file must be
   loaded AFTER ISOLAT.AX which contains the multifile declaration
   for isolat/4.  In Quintus version 1.0, the clauses in the two files
   must be combined in one file.
*/



/*ISOLATION AXIOMS FOR >= */

 % Exponentiation

isolax( 1 , U^N>=V , U>=V^N1 , true ) :- tidy(1/N, N1).

isolax( 2 , U^V>=W , V>=log(U,W) , positive(W) ) .

/* multiplication */
isolax( 1 ,U*V>=W, U>=W*V1,positive(V)) :- tidy(1/V,V1).

isolax( 1 ,U*V>=W, U =< W*V1, negative(V)) :- tidy(1/V,V1).

isolax( 2 , V*U>=W, U>=W*V1, positive(V)) :- tidy(1/V,V1).

isolax( 2 , V*U>=W, U =< W*_V1, negative(V)) :- tidy(1/V,V).

/* addition */
isolax( 1 ,U+V>=W,U>=W+(-1)*V,true).
isolax( 2 , V+U>=W, U>=W+(-1)*V, true ) .


/* cosine */
isolax(1, cos(U) >= V, U =< arccos(V), acute(U)).

/* sine */
isolax( 1 ,sin(U)>=V,U>=arcsin(V),acute(U)).

/* tangent */
isolax( 1 , tan(U) >= V, U >= arctan(V), acute(U)).

/*ISOLATION AXIOMS FOR > */

/* Exponentiation */

isolax( 1 , U^N>V , U>V^N1 , true ) :- tidy(1/N, N1).

isolax( 2 , U^V>W , V>log(U,W) , positive(W) ) .

/* multiplication */
isolax( 1 ,U*V>W, U>W*V1,positive(V)) :- tidy(1/V,V1).

isolax( 1 ,U*V>W, U < W*V1, negative(V)) :- tidy(1/V,V1).

isolax( 2 , V*U>W, U>W*V1, positive(V)) :- tidy(1/V,V1).

isolax( 2 , V*U>W, U < W*V1, negative(V)) :- tidy(1/V,V1).

/* addition */
isolax( 1 ,U+V>W,U>W+(-1)*V,true).
isolax( 2 , V+U>W, U>W+(-1)*V, true).

/* sine */
isolax( 1 ,sin(U)>V,U>arcsin(V),acute(U)).

/* tangent */
isolax( 1 , tan(U) > V, U > arctan(V), acute(U)).

/* square root */
isolax( 1 ,U^K>V,U>V^2,true) :- eval(K=:=1/2),!.


/* Isolation Axioms for < */

 % Exponentiation

isolax( 1 , U^N < V , U<V^N1 , positive(V) ) :- tidy(1/N, N1).


isolax( 2 , U^V<W , V<log(U,W) , positive(W) ) .

/* multiplication */

isolax( 1 ,U*V < W, U < W*V1, positive(V)) :- tidy(1/V,V1).

isolax( 1 ,U*V < W, U>W*V1,negative(V)) :- tidy(1/V,V1).

isolax( 2 , V*U < W, U < W*V1, positive(V)) :- tidy(1/V,V1).

isolax( 2 , V*U < W, U>W*V1, negative(V)) :- tidy(1/V,V1).

 % Why none for =< ?
 % Other functions?
