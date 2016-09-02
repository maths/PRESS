/* IDENT. :             Prove identities with PRESS
Written 1.11.1981
                                                Bernard Silver
                                                Updated: 21 March 83
*/

/* Top level X is the possible identity */
identity(X) :- 
        trace_press('\nTrying to prove that\n%t\nis an identity\n',[X],1),
        tidy(X,Y),
        cond_print(X,Y,_),
        abolish(seen_eqn,1),
        ident(Y),
        !.

/* Recursive call top level */
identity1(X) :- tidy(X,Y),cond_print(X,Y,_),ident(Y),!.

/* Base cases */
ident(false) :- !,trace_press('\nExpression is not an identity\n',1).
ident(true) :- !,trace_press('\nExpression is an identity\n',1).
ident(A=A) :-  !,trace_press('\nIdentically true\n',1).  %unifies

/* Find words in expression */
ident(X) :- wordsin(X,Words),ident1(X,Words),!.

/* No words remaining,so fail */
ident1(_,[]) :- trace_press('\nCannot show identity\n',1),!,fail.

/* Try to solve as an equation with unknown X */

ident1(X,[H|_]) :- ident2(X,H),!.

/* Try next word, if any */
ident1(X,[_|T]) :- ident1(X,T),!.

/* Put expression in weak normal form and try PRESS methods */
ident2(Old,Unk) :- weak_normal_form(Old,Unk,New),ident3(New,Unk),!.

  % Isolation

ident3(A,Unk) :- 
        occ(Unk,A,1),
        position(Unk,A,Posn),
        isolate(Posn,A,New),
        tidy(New,New1),
        cond_print(New,New1,_),
        terminate_ident(New),
        !.

% Polynomial
ident3(L=R,X) :- 
        is_poly(X,L),
        poly_solve(L=R,X,Ans,_),
        !,
        ident(Ans).

% Collection
ident3(Old=Rhs,Unk) :- 
        collect(Unk,Old,New),
        trace_press('\n%t\n',[New=Rhs],1),
        !,
        identity1(New=Rhs),
        !.

% Attraction
ident3(Old=Rhs,X) :-  
        attract(X,Old,New), 
        !,
        trace_press('%c\n',[New=Rhs],1),
        identity1(New=Rhs),
        !.

% Change Of Unknown
ident3(A=B,Unk) :- 
        occ(Unk,A,N), 
        eval(N>1),
        setof(T,good_subterm(A,Unk,N,T),Tset),
        extreme_term(Tset,>,T),
        identifier(New),
        !,
        subst_mesg(T=New,A=B,Neweq),
        identity1(Neweq),
        !.

% Trig Methods
ident3(Old,Unk) :- 
        linear_sin_cos(Old,Unk),
        trig_fac(Old,Unk,New), 
        !,
        trace_press('\n%t\n',[New],1),
        identity1(New),
        !.

% Homogenization
ident3(Old,Unk) :- 
        mult_occ(Unk,Old), 
        multiple_offenders_set(Old,Off,Unk),
        homog(Old,Unk,New,_,_,Off),
        identity1(New),
        !.

% Nas1
ident3(Eqn,X) :-  
        mult_occ(X,Eqn), 
        nas1(Eqn,X,Posn),
        isolate(Posn,Eqn,New),
        findrhs(New,List),
        checklist(freeof(X),List),
        !,
        identity1(New),
        !.

% Logmethods
ident3(Eqn,X) :- 
        logmethod(Eqn,X,New,Base), 
        trace_press('\nTaking logs, base %t, gives\n\n%t\n',[Base,New],1),
        identity1(New),
        !.

% Nasty Function Method
ident3(Eqn,X) :- nasty_method(Eqn,X,Neweq),tidy(Neweq,New),!,identity1(New),!. 

/* Examine result of isolation  */
terminate_ident(true) :- trace_press('\nExpression is identity\n',1),!.
terminate_ident(_) :- trace_press('\nExpression is not an identity\n',1),!.

