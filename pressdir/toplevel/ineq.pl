/*              INEQ       Updated: 14 January 83
		min/3 commented out 3rd October 1987
*/

% :- public
%                 findbnd/3,
%                 givesneg/3,
%                 subst2/4.

/*******************************
        MULTIPLE INEQUALITIES
**********************************/

/*FIND MINIMUM VALUE OF X FOR WHICH EXP IS TRUE*/
%min(Exp,X,Minval) :- 
%        solveineq(Exp,X,X>=Minval),
%        trace_press('Hence minimum value of %c is %c\n',[X,Minval],1),
%        !.

/*SOLVE INEQUALITY CONJUNCTION*/

solvemax(Exp,X,Ans) :-
        process_ineq(Exp,X,Ansset),
        maximum(Ansset,Ans1), 
        tidy(Ans1,Ans),
        trace_press('%t dominates the other inequalities.\n',[Ans],1).

solvemin(Exp,X,Ans) :-
        process_ineq(Exp,X,Ansset),
        minimum(Ansset,Ans1), 
        tidy(Ans1,Ans),
        trace_press('%t dominates the other inequalities.\n',[Ans],1).

process_ineq(Exp,X,Ansset) :-
        trace_press('Trying to solve %c\n',[Exp],1),
        tidy(Exp,Exp1),
        mapand(findbnd(X),Exp1,Ansset),
        trace_press('Isolating %t on the lhs gives %c\n',[X,Ansset],1),
        trace_press('Trying to find maximum of : %c',[Ansset],3),
        !.

/*SOLVE INEQUALITY*/

findbnd(_X,true,true) :-!.

findbnd(X,Ineq,Ans) :-
        solve(Ineq,X,Ans1),
        Ans1=..[Prop,X,Bnd1],
        (intermediates_in(Bnd1,[Y]) -> findmax(Bnd1,Y,[Bnd]); Bnd1=Bnd),
        Ans=..[Prop,X,Bnd],
        !.

findbnd(X,Ineq,_Ans) :-
        trace_press('Unable to find bounds for %t in %t.\n',[X,Ineq],2), 
        !, fail.


/*GET LIST OF INTERMEDIATES IN EXP*/

intermediates_in(Exp,Inters) :-
        wordsin(Exp,Words), 
        sublist(intermediate,Words,Inters),
        !.

/*FIND MAXIMUM VALUES OF EXPRESSION*/

findmax(Exp,X,Maxvals) :-
        diffwrt(Exp,Exp2,X),
        solve(Exp2=0,X,Soln),
        collect_ans(X,Soln,Anslist),
        diffwrt(Exp2,Exp3,X),
        sublist(givesneg(X,Exp3),Anslist,Maxargs),
        maplist(subst2(X,Exp),Maxargs,Maxvals),
        !.

/*special substitution to suit maplist*/

subst2(X,Exp,Arg,Val) :- subst(X=Arg,Exp,Val1), tidy(Val1,Val), !.

/*MAKE LIST OF ALTERNATIVE ANSWERS*/

collect_ans(X,true, [X]) :- !.

collect_ans(_X,false, []) :- !.

collect_ans(X,X=Ans,[Ans]) :- !.

collect_ans(X,Exp1#Exp2,Anslist) :-
        collect_ans(X,Exp1,Anslist1), 
        collect_ans(X,Exp2,Anslist2),
        append(Anslist1,Anslist2,Anslist),
        !.

/*SUBSTITUTING ANS FOR X IN EXP GIVES NEGATIVE RESULT*/

givesneg(X,Exp,Ans) :-
        subst_mesg(X=Ans,Exp,Exp1), 
        negative(Exp1),
        !.
