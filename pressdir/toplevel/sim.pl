 %              SIM                      
 % Simplify simultaneous equations using homogenization 
 % Bernard Silver 12.9.81 
 % Updated: 10 September 82

 % Top level 
 % Find the offending terms in each unknown 

sim(Eqns1,Unks,Ans) :- tidy(Eqns1,Eqns),
        mapmodparse(Eqns,Unks,Offends),
        sim1(Eqns,Unks,Offends,Ans),
        !.

 % If all the offending sets are empty or contain only the unknown
 % use normal method (simsolve) 

sim1(Eqns,Unks,Offends,Ans) :- checktrivial_set(Unks,Offends),
        simsolve(Eqns,Unks,Ans),
        !.

 % Otherwise try to use homogenization  

sim1(Eqns,Unks,Offends,Ans) :-
        trace_press('Simultaneously solving : %c For %t.\n',[Eqns,Unks],1),
        apply_sim2(Eqns,Unks,Offends,New,Vs,Terms),
        !,
        tidy(New,New1),
        reorder_eqn(Vs,New1,NewVarsList,NewEqnList),
	flag(real,Old,off),
        simsolve1(NewEqnList,NewVarsList,Ans1),
        ortodot(Ans1,Dislist),
        listsolve(Dislist,NewVarsList,Terms,Unks,Ans2),
        dottoor_set(Ans2,Ans),
        trace_press('\nFinal Answers are : %e ',[Ans],1),
	flag(real,_,Old),
	process_answer_real(Ans,_).


 % If homogenization fails try simsolve 
sim1(Eqns,Unks,_,Ans) :- 
	flag(real,Old,off),
	simsolve1(Eqns,Unks,Ans1),
        tidy(Ans1,Ans),
        trace_press('\nFinal Answers are : %e',[Ans],1),
	flag(real,_,Old),
	process_answer_real(Ans,_),
        !.

apply_sim2(Eqns,[],[],Eqns,[],[]) :- !.
apply_sim2(Eqns,[H|T],[O1|T1],New,[V1|T2],[Term1|T3]) :-
        sim2(Eqns,H,O1,New1,V1,Term1),
        apply_sim2(New1,T,T1,New,T2,T3),
        !.

 % sim2(Eqns,Unknown,Newequation,Identifier,Reduced_Term) applies
 % homogenization to the set of equations,homogenizing in Unknown 

sim2(Eqns,X,[],Eqns,X,X) :- !. %Eqns do not contain X
sim2(Eqns,X,[X],Eqns,X,X) :- !. %Eqns is already homogeneous in X
sim2(Eqns,X,Y,Eqns,X,X) :- checklist(polytype(X),Y),!.
 %Only Polynomials

 % Change of Unknown case 
sim2(Eqns,_X,[A],New,V,A) :- identifier(V),subst_mesg(A=V,Eqns,New),!.

 % Homogenize 
sim2(Eqns,X,Off,New,V,Term) :- 
	homog1(Eqns,X,New,Term,V,Off,Hom,sim),
        trace_press('\nHomogenizing equations in %t\n gives %c\n',[X,Hom],1),
        trace_press('\nSubstituting %t = %t gives %c\n',[V,Term,New],1),
        !.

 % listsolve(ListofAns,Newunks,Reducedterms,Oldunks,Newans) 
 % ListofAns is the list of answers in the Newunks returned by simsolve1.
 % listsolve now solves the substitution equations (of the form
 % Newunk1=Ans1 & Reducedterm1=Newunk1) in terms of the Oldunks to give
 % Newans  

listsolve([],_,_,_,[]) :- !.
listsolve([A|T],X,Y,Z,[A1|T1]) :- 
	andtodot(A,A2),
        listsolve1(A2,X,Y,Z,A3),
        dottoand(A3,A4),
        tidy(A4,A1),
        listsolve(T,X,Y,Z,T1),
        !.

listsolve1([],_,_,_,[]) :- !.
listsolve1([H|T],Vs,Terms,Unks,[Ans|Tail]) :-
        wordsin(H,Words),
        correspond1(Words,Vs,Terms,Unks,Id,Term,Unk),
        subst_solve(Id,Term,H,Unk,Ans),
        listsolve(T,Vs,Terms,Unks,Tail),
        !.

 % Solve substitution equation

 %No substitution needed
subst_solve(X,X,Unk=Ans,Unk,Unk=Ans) :- !.

 % General case
subst_solve(Id,Term,H,Unk,Ans) :- 
        subst_mesg(Id=Term,H,New),
        weak_normal_form(New,Unk,New1),
	flag(real,Old,off),
        solve_eqn(New1,Unk,Ans,_),
	flag(real,_,Old),
        !.

 % The offending set is trivial,ie it is empty or contains just the unknown 
checktrivial_set(_,[]) :- !.
checktrivial_set(X,[H|T]) :- trivial_set(X,H),checktrivial_set(X,T),!.

trivial_set(_,[]) :- !.
trivial_set(Unklist,[X]) :- member(X,Unklist),!.


 %Reorder equations so nicest occurs first
reorder_eqn(OldVars,OldEqns,[GoodVar|Rest],NewEqns) :-
        find_good_eqn(OldVars,OldEqns,NewEqns,GoodVar),
        !,
        delete(OldVars,GoodVar,Rest).

reorder_eqn(OldVars,OldEqns,OldVars,OldEqns).
find_good_eqn([X|_],Old,New,X) :- try_sort(X,Old,New),!.
find_good_eqn([_|T],Old,New,X) :- find_good_eqn(T,Old,New,X),!.



 % Equation to be solved first should have only one 'easy' occurrence of X

try_sort(X,First&Rest,First&Rest) :- good_eqn(X,First),!.
try_sort(X,F&Rest,New) :- try_sort(X,Rest,New1),tidy(New1&F,New).
try_sort(X,First,First) :- good_eqn(X,First),!.

 %Occurrence is good if it is a first order polynomial
good_eqn(X,Eqn) :- 
	single_occ(X,Eqn),
        weak_normal_form(Eqn,X,Lhs=_Rhs),
        poly_norm(Lhs,X,[polyand(1,_)|_]),
        !.

 % Multilist version of correspond/4  
 % correspond1(List,L1,L2,L3,T1,T2,T3)
 % List, L1,L2,L3 are lists,T1 is a member of List that also occurs in L1.
 % T2 and T3 occur in the same position in L2 and L3 as T1 does in L1 

correspond1([],_,_,_,_,_,_) :- !,fail.
correspond1([H|_],L1,L2,L3,H,T2,T3) :-
        correspond2(H,L1,L2,L3,T2,T3),
        !.
correspond1([_|H],L1,L2,L3,T1,T2,T3) :-
        correspond1(H,L1,L2,L3,T1,T2,T3),
        !.

correspond2(H,[H|_],[H1|_],[H2|_],H1,H2) :- !.
correspond2(H,[_|T],[_|T1],[_|T2],H1,H2) :- 
        correspond2(H,T,T1,T2,H1,H2),
        !.

 % Modified parser,deals with & and =,and also reorders the expression 
mapmodparse(_,[],[]) :- !.
mapmodparse(X,[H|T],[H1|T1]) :- modparse(X,H,H1),mapmodparse(X,T,T1),!.

modparse(Exp,X,Off) :- dl_modparse(Exp,X,Off1-[]),listtoset(Off1,Off).

dl_modparse(A&B,X,L-L1) :- !,dl_modparse(A,X,L-L2),dl_modparse(B,X,L2-L1).
dl_modparse(A=B,X,L-L1) :- !,dl_modparse(A,X,L-L2),dl_modparse(B,X,L2-L1).
dl_modparse(A,X,Off) :- dl_parse(A,Off,X),!.

 % These are needed to deal with disjunctive solutions from simsolve1 
dottoor_set(List,Ans) :- listtoset(List,L1),dottoor(L1,Ans1),tidy(Ans1,Ans),!.

 % Equation doesn't need homogenization in X
polytype(X,X) :- !.
polytype(X,X^N) :- integer(N),!.

 %Clauses for easy type in

sim(Eqns) :- sim(Eqns,[x,y],_Ans).

sim(Eqns,Unks) :- sim(Eqns,Unks,_Ans).
