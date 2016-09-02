/* ISOLAT. : 

                                                    19.2.81 
                                                 Modified 19.9.81 
                                                Updated: 7 September 82
*/

% :- public
%                 isolate/3.


/* ISOLATION ROUTINES*/

isolate([N|Posn],Exp,Ans) :-
        maneuver_sides(N,Exp,NewExp),
        isolate1(Posn,NewExp,Inter),
        tidy(Inter,Ans),
        mod_trace_press(Ans).

/*get term to be isolated on Lhs */

maneuver_sides(1,Exp,Exp) :- !.

maneuver_sides(2,Exp,NewExp) :- 
        !,
        Exp=..[Sym,Lhs,Rhs],
        invert(Sym,Sym1),
        NewExp=..[Sym1,Rhs,Lhs].

%% Perform the Isolation %%

/*trivial boolean cases*/

isolate1(_Posn,false,false).
isolate1(_Posn,true,true).

/*deal with each disjunct*/

isolate1(Posn,Eqn1#Eqn2,Ans1#Ans2) :- 
        !, 
        isolate1(Posn,Eqn1,Ans1),
        isolate1(Posn,Eqn2,Ans2).


/*expression is already isolated*/

isolate1([],Ans,Ans) :- !.

/*expression can have isolax rule applied*/

isolate1([N|Posn],Old,Ans) :- !,
        isolax(N,Old,New,Condition),
        modcall(Condition),    %Hack for non_zero
        isolate1(Posn,New,Ans).

/* Inversion of Predicates */

invert(S1,S2) :- perm2(S1,S2,S3,S4), invert1(S3,S4), !.

invert1(=,=) :- !.
invert1(>,<) :- !.
invert1(>=,=<) :- !.

/* Overcoming non_zero, etc. condition */

modcall(A&B) :- !,modcall(A),modcall(B).
modcall(non_zero(X)) :- non_zero(X),!.
modcall(non_zero(X)) :- eval(X=0),!,fail.
modcall(non_zero(X)) :- trace_press('\nAssuming %t is non-zero\n',[X],1),!.
modcall(X) :- call(X),!.

/* Output result */

mod_trace_press(false) :- !. % Hack for false case
mod_trace_press(Exp) :- trace_press('%c     (by Isolation)\n',[Exp],1),!.

