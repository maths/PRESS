%   File   : ARITH.PL
%   Author : R.A.O'Keefe
%   Updated: 12 June 1984
%   Purpose: Define the 'plus' family of arithmetic predicates.

% :- public
%         divide/4,
%         ge/2,
%         gt/2,
%         le/2,
%         lt/2,
%         plus/3,
%         succ/2,
%         times/3.

%:- mode
%        instantiation_fault_(+),
%        ge(?, ?),
%        gt(?, ?),
%        le(?, ?),
%        lt(?, ?),
%        succ(?, ?),
%        plus(?, ?, ?),
%        times(?, ?, ?),
%        times(+, +, ?, ?, ?),
%        divide(?, ?, ?, ?).

/* :- pred
        ge(integer, integer),
        gt(integer, integer),
        le(integer, integer),
        lt(integer, integer),
        succ(integer, integer),
        plus(integer, integer, integer),
        times(integer, integer, integer),
            times(integer, integer, integer, integer, integer),
        divide(integer, integer, integer, integer).
*/
/*  These predicates are now primitives in C Prolog.  My original
    reason for adding them to C-Prolog was efficiency, as the "is"
    expression interpreter (which has to handle floating point as
    well as integers) is quite complicated.  succ(X, Y) is 1.2ms
    faster than Y is X+1 on a VAX 11-750.  However, I found that
    using succ and plus were clearer, and because of the (limited)
    reversibility of these operations, lived up to Prolog's claim
    to have some relation to logic a little better than programs
    written using the strictly one-way "is".  When I started using
    Prolog I was very taken with "is" and held my nose up at IC-
    Prolog for lacking it.  I now think this was a mistake.

    Of course there are occasions when you want more than the four
    algorithms of antiquity, such as when you want to do bit-wise
    operations, or when you want to use floating-point.  And it is
    undeniable that a single arithmetic expression involving 3 or 4
    operators is a lot clearer than 3 or 4 predicate calls whose
    data flow needs careful tracing.  The style rule I have adopted
    (in addition to using 'is' when these predicates simply can't
    express what I mean) is to use 'is' whenever I can't express
    what I want with a *single* call on one of these predicates.
    Thus to calculate 3X I might use times(3,X,Ans), but to obtain
    3X+2 I would use Ans is 3*X+2.  A further style rule is that
    if a predicate uses 'is', all similar calculations in that
    predicate also use 'is' so you can see what is going on.  Thus
    if I want 3X+2 in one clause and 3X in another, I would use 'is'
    in both so that the reader can easily perceive the similarities
    and differences.  Reversibility is not then an issue.

    This Prolog code looks bulky.  It is bulky.  But don't dismiss
    these operations for such a reason.  The C code which implements
    them in C Prolog is much shorter, and it is much more efficient
    than using "is", as it knows that there is no question of walking
    down trees.  When writing a Prolog compiler, you should consider
    these operations: the compiler can benefit from knowing more
    about the arguments, and if it keeps track of the instantiation
    state of source variables it may be able to generate special-
    purpose code.  (E.g. calling plus(X,Y,Z) when Z is known to be
    unbound should generate "int_chk_push(X), int_chk_push(Y),
    add, bind_new_var(Z)" or something like that.)
*/

%   The general idea is that if there is enough information in a goal
%   to detect that it must fail (e.g. succ(X,a)) we fail, if there is
%   enough information to determine a unique solution, we yield that
%   solution, and otherwise (this can generally only happen when too
%   few arguments are instantiated) we report an instantiation fault.
%   We report a fault even when the non-determinism is bounded, e.g.
%   in times(X, X, 4) there are only two possible solutions.  This is
%   because these operations are primitives, that would be coded in
%   assembler or micro-code, and we don't want to oblige a compiler
%   to generate full frames for them.  

instantiation_fault_(Goal) :-
        nl, write('! instantiation fault in '),
        print(Goal), nl,
        break, abort.



%   {ge|gt|le|lt}(X,Y) <-> integer(X) & integer(Y) & X {>=|>|=<|<} Y
%   Note that there is no eq or ne, = and \= will do fine.

ge(X, Y) :-
        integer(X), integer(Y),
        !,
        X @>= Y.
ge(X, Y) :-
        ( var(X) ; integer(X) ),
        ( var(Y) ; integer(Y) ),
        !,
        instantiation_fault_(ge(X,Y)).


gt(X, Y) :-
        integer(X), integer(Y),
        !,
        X @> Y.
gt(X, Y) :-
        ( var(X) ; integer(X) ),
        ( var(Y) ; integer(Y) ),
        !,
        instantiation_fault_(gt(X,Y)).


le(X, Y) :-
        integer(X), integer(Y),
        !,
        X @=< Y.
le(X, Y) :-
        ( var(X) ; integer(X) ),
        ( var(Y) ; integer(Y) ),
        !,
        instantiation_fault_(le(X,Y)).


lt(X, Y) :-
        integer(X), integer(Y),
        !,
        X @< Y.
lt(X, Y) :-
        ( var(X) ; integer(X) ),
        ( var(Y) ; integer(Y) ),
        !,
        instantiation_fault_(lt(X,Y)).



%   succ(P, S) <-> integer(P) & integer(S) & P >= 0 & S = P+1
%   given either of P or S we can solve for the other.
%   If either is neither an integer nor a variable the relation
%   must be false.  But succ(P, S) with both arguments unbound
%   has infinitely many solutions.  (You can generate a bounded
%   range of integers using between/3.)

succ(Pred, Succ) :-
        integer(Pred),
        !,
        Pred >= 0,
        Succ is Pred+1.
succ(Pred, Succ) :-
        integer(Succ),
        !,
        Succ > 0,
        Pred is Succ-1.
succ(Pred, Succ) :-
        var(Pred), var(Succ),
        instantiation_fault_(succ(Pred,Succ)).



%   plus(A, B, S) <-> integer(A) & integer(B) & integer(S) & S = A+B.
%   given any two of the arguments, we can solve for the third.
%   If any argument is neither an integer nor a variable, the relation
%   must be false.  If two are variables and the other is variable or
%   integer, there are infinitely many solutions.

plus(A, B, S) :-
        integer(A), integer(B),
        !,
        S is A+B.
plus(A, B, S) :-
        integer(A), integer(S),
        !,
        B is S-A.
plus(A, B, S) :-
        integer(B), integer(S),
        !,
        A is S-B.
plus(A, B, S) :-
        ( var(A) ; integer(A) ),
        ( var(B) ; integer(B) ),
        ( var(S) ; integer(S) ),
        !,      % at most one of A,B,S is integer, the others are vars
        instantiation_fault_(plus(A,B,S)).



%   times(A, B, P) <-> integer(A) & integer(B) & integer(P) & P = A*B.
%   This is trickier than plus.  Given A and B there is a unique solution
%   for P.  Given A(B) and P there is at most one solution for B(A)
%   except in the case when P and A(B) are both 0, in which case there
%   are infinitely many solutions.  Given just A or B there are infinitely
%   many solutions.  Given P there is always a finite number of solutions,
%   but this number always exceeds 1 (even times(X,Y,1) has X,Y=1,1 or -1,-1).
%   So we report an instantiation error in that case two.  Of course if any
%   argument is instantiated to a non-integer the relation must be false.

times(A, B, P) :-
        integer(A), integer(B),
        !,
        P is A*B.
times(A, B, P) :-
        integer(A), integer(P),
        !,
        times(P, A, B, A, B).
times(A, B, P) :-
        integer(B), integer(P),
        !,
        times(P, B, A, A, B).
times(A, B, P) :-
        ( var(A) ; integer(A) ),
        ( var(B) ; integer(B) ),
        ( var(P) ; integer(P) ),
        !,      % at most one of P,A,B is integer, the others are vars
        instantiation_fault_(times(A,B,P)).

times(P, A, B, _X, _Y) :-
        A \== 0,
        !,
        0 is P mod A,
        B is P  //  A.
times(0, 0, _B, X, Y) :-
        instantiation_fault_(times(X,Y,0)).



/*  divide(A, B, Q, R)
    means A, B, Q, and R are all integers,
    A = B*Q + R,
    A*R >= 0,   (A and R have the same sign, so Q is truncated towards 0)
    0 <= |R/B| < 1      (so B is non-zero)

    This piece of Prolog is to be taken as a specification of the
    predicate, an implementation may proceed differently.
    I assume "is", and that overflow need not be checked for,
    and that X / Y and X / Y are well defined for X >= 0, Y >= 1.

    Cases:
        any one of A, B, Q, R is bound to a non-integer => FAIL
        B is bound to 0 => FAIL
        {These two failures should have associated error messages}

        A and B bound => calculate Q', R' unify Q=Q' R=R'

        A, Q, and R bound => if A*R < 0 then FAIL
                         if Q = 0 then instantiation FAULT
                         unless |Q| divides A-R then FAIL
                         calculate B from divide(A-R,Q,B,0)

        B, Q, and R bound => check conditions
                        calculate A = B*Q+R
                        check remaining conditions

        otherwise, instantiation FAULT
*/
divide(A, B, Q, R) :-
        ( nonvar(A), \+ integer(A)
        ; nonvar(B), \+ integer(B)
        ; nonvar(Q), \+ integer(Q)
        ; nonvar(R), \+ integer(R)
        ),
        !,      % bound to fail
        fail.
divide(A, B, Q, R) :-
        nonvar(A),
        nonvar(B),
        !,
        ( B > 0, A >= 0, Q1 is A//B
        ; B > 0, A <  0, Q1 is -((-A)//B)
        ; B < 0, A >= 0, Q1 is -(A//(-B))
        ; B < 0, A <  0, Q1 is (-A)//(-B)
        ), !,
        Q = Q1,
        R is A-Q1*B.
divide(A, B, Q, R) :-
        nonvar(A),
        nonvar(Q),      
        nonvar(R),
        !,
        ( A >= 0, R >= 0
        ; A =< 0, R =< 0
        ),
        ( Q = 0, !, instantiation_fault_(divide(A,B,Q,R))
        ; true
        ),
        !,
        0 is (A-R) mod Q,
        B is (A-R)  //  Q.
divide(A, B, Q, R) :-
        nonvar(B),
        nonvar(Q),
        nonvar(R),
        !,
        B \== 0,
        A is B*Q+R,
        ( R >= 0, A >= 0, (B > R ; -B > R)
        ; R =< 0, A =< 0, (B < R ; -B < R)
        ),
        !.
divide(A, B, Q, R) :-
        instantiation_fault_(divide(A,B,Q,R)).

