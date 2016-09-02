/* SOLVE : Top level Solve procedure

        After a chequered history       Updated: 12 May 1983
*/                      
/***********************************************
        SOLVE ONE EQUATION OR INEQUALITY 
************************************************/

:- dynamic seen_eqn/1.

/* Top Level Solve Procedure */

solve(Eqn,X,Ans,[Tidy|ProofTree]) :- 
    process_input(Eqn,X,Eqn1,Tidy),
    initialize_loop_check,
    solve_eqn(Eqn1,X,Ans1,ProofTree-[]), 
    process_answer(Ans1,Ans,[Tidy|ProofTree]),
    !.


/* Equation does not contain the unknown */

solve_eqn(Eqn,X,Soln,[evaluation|L]-L) :- 
        freeof(X,Eqn), 
        !,
        verify(Eqn,Soln).

/* Deal with disjunction */

solve_eqn(Eqn,X,Soln,Proof) :-
        disjunction(Eqn),
        !,
        disj_solve(Eqn,X,Soln,Proof).

disj_solve(Eqn,X,Soln,Proof) :-
        ortodot(Eqn,EqnList),
        disj_solve_list(EqnList,X,SolnList,Proof),
        dottoor(SolnList,Soln),
        !.

disj_solve_list([],_,[],L-L) :- !.

disj_solve_list([Eqn|E],X,[Soln|S],[Proof|P]-Diff) :-
        trace_press('\n\nSolving disjunct %t\n',[Eqn],1),
        solve_eqn(Eqn,X,Soln,Proof),
        disj_solve_list(E,X,S,P-Diff).

/* See if eqn is factorizable*/

solve_eqn(Lhs=Rhs, X, Soln, [Fact|Proof]-Diff) :-       % Always factorise when possible
        zero(Rhs),
        mulbag(Lhs),
        !,
        factorise(Lhs,X,List,Fact),
        fact_solve(List,X,SolnList,Proof-Diff),
        dottoor(SolnList,Soln).

fact_solve([],_,[],L-L) :- !.

fact_solve([Lhs|L],X,[Soln|S],Proof-Diff) :-
        trace_press('\n\nSolving factor %t = 0\n',[Lhs],1),
        solve_eqn(Lhs=0,X,Soln,Proof-Inter),
        fact_solve(L,X,S,Inter-Diff).

/* If single occurence of unknown then Isolate */

solve_eqn(Exp,X,Ans,[single_occurrence|Proof]-Proof) :-
        single_occ(X,Exp),
        !,
        position(X,Exp,Posn),
        isolate(Posn,Exp,Ans).

/* Special Polynomial Method */

solve_eqn(Lhs=Rhs,X,Ans,Proof-Diff) :-
        is_poly(X,Lhs),
        !,
        poly_solve(Lhs=Rhs,X,Ans,Proof-Diff).

/* Try to Change the unknown to simplify equation  */

solve_eqn(Eqn,X,Ans,[substitute(Term)|Proof]-Diff) :- 
        changeunknown(Eqn,X,Term),
        !,
        changevar(Term,Eqn,NewVar,NewEqn),
        solve_eqn(NewEqn,NewVar,Soln,Proof-Inter),
        subst_mesg(NewVar=Term,Soln,NewSoln),
        solve_eqn(NewSoln,X,Ans,Inter-Diff),
        !.

/* Apply Collection to reduce occurrences of unknown */

solve_eqn(Exp=Rhs,X,Ans,[collect|Proof]-Diff) :- 
        collect(X,Exp,New), 
        !,
        trace_press('\n%t = %t\n',[New,Rhs],1),
        solve_eqn(New=Rhs,X,Ans,Proof-Diff).

/* Apply Attraction to move occurrences of unknown closer together */

solve_eqn(Exp=Rhs,X,Ans,[attract|Proof]-Diff) :- 
        attract(X,Exp,New), 
        !,
        trace_press('\n%t = %t\n',[New,Rhs],1),
        solve_eqn(New=Rhs,X,Ans,Proof-Diff).

/* Trig factorization method */

solve_eqn(Eqn,X,Ans,[trig_fac|Proof]-Diff) :-
        linear_sin_cos(Eqn,X),
        trig_fac(Eqn,X,Neweqn),
        solve_eqn(Neweqn,X,Ans,Proof-Diff),
        !.

/* Try to remove dominating functor  */

solve_eqn(Eqn,X,Ans,[isolate|Proof]-Diff) :- 
        nas1(Eqn,X,Posn),
        isolate(Posn,Eqn,New),
        solve_eqn(New,X,Ans,Proof-Diff),
        !.

/* Try to take logs if equation is in suitable form  */

solve_eqn(Eqn,X,Ans,[take_logs|Proof]-Diff) :- 
        prod_exp_terms_eqn(Eqn,X,New),
        !,
        log_reduce(New,X,Base,Log),
        weak_normal_form(Log,X,NewLog),
        trace_press('\nTaking logs, base %t, gives \n\n%t\n',[Base,Log],1),
        solve_eqn(NewLog,X,Ans,Proof-Diff),!.
 
/* Try homogenization     */

solve_eqn(Eqn,X,Ans,[homog|Proof]-Diff) :-
        multiple_offenders_set(Eqn,Off_Set,X),
        homog(Eqn,X,New,Term,V,Off_Set),
        tidy(New,NewEqn),       % Hack due to poor tidying
        solve_eqn(NewEqn,V,Vans,Proof-Inter),
        subst_mesg(V=Term,Vans,Uans),
        solve_eqn(Uans,X,Ans,Inter-Diff),
        !.


/* Try to eliminate Nasty Functions */

solve_eqn(Eqn,X,Ans,[nasty|Proof]-Diff) :- 
        nasty_method(Eqn,X,Neweq),
        tidy(Neweq,Neweqn),
        solve_eqn(Neweqn,X,Ans,Proof-Diff),
        !.


/* One, two and three argument solve clauses for easy type-in and past
        compatibility                                    */

solve(Exp) :- solve(Exp,x,_A,_).
solve(Exp,Unk) :- solve(Exp,Unk,_Ans,_).
solve(Exp,Unk,Ans) :- solve(Exp,Unk,Ans,_).

% Initialize Looping Check
initialize_loop_check :- 
        abolish(seen_eqn,1),
        assert((seen_eqn(_) :- fail)).

% Tidy the input equation, putting into weak normal form (see weaknf)
% noting any significant changes

process_input(Eqn,X,Eqn2,Infer) :-
        trace_press('\nSolving %t for %t\n',[Eqn,X],1),
        tidy(Eqn,Eqn1),
        weak_normal_form(Eqn1,X,Eqn2),
        cond_print(Eqn,Eqn2,Infer),
	!.

% Tidy and print the answer in the appropriate form.

process_answer(Ans,Ans2,ProofTree) :-
        tidy(Ans,Ans1),
        simplify_ans(Ans1,Ans2),
	print_the_answer(Ans2,ProofTree),
	real_process(Ans2,ProofTree), % Calculate and print a possibly evaluated answer but
	!.				   % don't pass it to Ans2 (which goes to solve) so that sim eqns 
					   % arent passed solutions that they cant handle.
real_process(Ans2,ProofTree) :-
	process_answer_real(Ans2,Ans3),
	(Ans2 = Ans3; print_the_answer(Ans3,ProofTree)),
	!.  



simplify_ans(A#B,C#D) :- !, simplify_ans(A,C), simplify_ans(B,D).
simplify_ans(X=Expr,X=Simp) :- !, simplify(Expr,Simp).
simplify_ans(Expr,Expr).        % to handle inequalities

print_the_answer(false,_) :- !,trace_press('\nThe equation has no solution.\n',1).

print_the_answer(Ans,_) :- trace_press('\nAnswer is : %e\n',[Ans],1).

process_answer_real(Old,New1) :-
	flag(real,on,on),
	!,
	process_answer_23(Old,New),
	cond_real_print(Old,New,New1).

process_answer_real(X,X).

:- flag(real,_,on).

cond_real_print(Old,New,New) :-
	match(Old,New),
	!.

cond_real_print(_,New,NewAns) :-
	remove_bad_answers(New,New1),
	tidy(New1,NewAns),
	writef_press('\nEvaluating expression to\n\n%e\n',[NewAns]).

remove_bad_answers(A#B,New) :- !,
	remove_bad_answers(A,A1),
	remove_bad_answers(B,B1),
	tidy(A1#B1,New).

remove_bad_answers(A&B,false) :-
	((remove_bad_answer(A,A1),A \= A1) ; (remove_bad_answer(B,B1),B \= B1)),
	!.
remove_bad_answers(A&B,A&B) :- !.
remove_bad_answers(A,B) :-
	remove_bad_answer(A,B).
remove_bad_answer(invalidlog-New,false) :- !,
	writef_press('\n Answer %t\n is invalid as it contains a negative argument to a log\n\n',[New]).
remove_bad_answer(invalidasin-New,false) :- !,
	writef_press('\n Answer %t\n is invalid as it contains a argument to asin that is numerically > 1\n\n',[New]).
remove_bad_answer(invalidacos-New,false) :- !,
	writef_press('\n Answer %t\n is invalid as it contains a argument to acos that is numerically > 1\n\n',[New]).
remove_bad_answer(invalidexp-New,false) :- !,
	writef_press('\n Answer %t\n is invalid as it contains a negative base argument to an exponent\n\n',[New]).
remove_bad_answer(A,A).