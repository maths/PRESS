/* SIMEQ. :         May 1981

                                                Alan Bundy
                                                Updated: 10 September 82

        Simultaneous Equations Routines      */

/*simultaneous solution with messages*/
simsolve(Eqns,Us,Ans) :-
        trace_press('Simultaneously solving : %cFor %t.\n',[Eqns,Us],1),
	flag(real,Old,off),	% Stop solve evaluating with real arith.
        simsolve_pick(Eqns,Us,Ans), 
        trace_press('\nFinal Answers are : %e', [Ans],1),
	flag(real,_,Old),	% Reset flag to old value
	real_process(Ans,_),
        !.


/* Solve conjunction of equations */
simsolve_pick(Eqns,Unks,Ans1) :-
        reorder_eqn(Unks,Eqns,NewUnks,NewEqns),
        simsolve1(NewEqns,NewUnks,Ans1).

simsolve1(EqnsA & EqnsB,[X|Unks], Ans1) :- !,
        pick_xeqn(EqnsA & EqnsB,X,XEqn,Rest),
        solve(XEqn,X,Ans),
        distribute(Ans,Rest,Eqns1),
        simsolve2(Eqns1,Unks,Ans1).



/*single equation*/
simsolve1(A=B, [U], Ans) :- !, solve(A=B,U,Ans).


/*basis case*/
simsolve1(true,[],true) :- !.

/*Pick equation to solve for x, and return the remainder */
pick_xeqn(EqnC,X,XEqn,RestC) :-  !,
        andtodot(EqnC,EqnL),
        sublist(contains(X),EqnL,XEqnL),
        subtract(EqnL,XEqnL,NonXRestL),
        select(XEqn,XEqnL,XRestL),
        append(XRestL,NonXRestL,RestL),
        dottoand(RestL,RestC).

/* Distribute Or over And */
distribute(Sub1 # Sub2, Exp, Ans1 # Ans2) :- !, % disjunction case
        distribute(Sub1,Exp,Ans1),
        distribute(Sub2,Exp,Ans2).

distribute(Sub, Exp, Sub & Ans) :- !,   % conjunction or single equation case
        subst_mesg(Sub,Exp,Ans).


/* Call simsolve1 recursively and substitute back */
simsolve2(Eqns1 # Eqns2, Unks, Ans1 # Ans2) :- !,       % Solve disjunction
        simsolve2(Eqns1,Unks,Ans1),
        simsolve2(Eqns2,Unks,Ans2).

simsolve2(X=Ans1 & Eqns, Unks, Ans3) :- !,      % Discount already solved equation
        simsolve1(Eqns, Unks, Ans2),
        trace_press('Substituting back in %t solution\n',[X],1),
        distribute(Ans2,X=Ans1,Ans3).

% Clauses for easy type-in   
simsolve(Eqns,Unks) :- simsolve(Eqns,Unks,_Ans).

simsolve(Eqns) :- simsolve(Eqns,[x,y],_Ans).

/* Problems
        2. Return particular solutions; alternates on backtracking.
        4. Reject silly answers as required by Cardan. (??)
*/
