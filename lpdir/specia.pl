%   File   :  /usr/bs/lpdir/specia.pl
%   Author : Bernard Silver
%   Updated: Tue Oct 15 11:59:48 1985
%   Purpose: Code for special cases of top level


 % Partition steps for factorization case
 % We require that after factorization step the equations are solved in
 % order, i.e. from A*B*C...*Z=0, the next line is A = 0, (or its tidied form)
 % which is solved, then the next factor B=0 appears, and the example proceeds 
 % with the solution of this equation.

 % Disjunction is similar


factor_part([_|T],List,Ans) :- factor_part1(T,List,Ans).

factor_part1([],Steps,[Steps]).
factor_part1([H|T],Steps,[HSteps|Rest]) :-
	wordsin(H,Words),
	member(W,Words),
	unknown(W),
	mod_weak_normal_form1(H,expr,W,H1),
	factor_part2(H1,Steps,HSteps,NewSteps),
	factor_part1(T,NewSteps,Rest).


factor_part2(H1,[H2|Rest],[],[H2|Rest]) :- match(H1,H2),!.
factor_part2(H1,[H2|Rest1],[H2|L2],Rest) :-
	factor_part2(H1,Rest1,L2,Rest),
	!.

 % Failure
factor_part2(_,_,_,_) :- 
	writef('Failure to find solution of next disjunct.'),
	!.

work2([[A|Rest]|StepsList],X,
[message(ff)|T],L1,Unk,L2) :- 
	work1([A|Rest],X,T,L3,Unk,L4),
	work2r(StepsList,X,L3,L1,L4,L2),
	!.

work2r([],_,T,T,L,L) :- !.
work2r([[B|Rest]|Tail],X,[message(nf)|T],L1,Unk,L2) :-
	work1([B|Rest],X,T,L3,Unk,L4),
	work2r(Tail,X,L3,L1,L4,L2),
	!.



 % Change of Unknown
 % We require the example has a step of the form New = f(Old)
 % and, after solving the changed variable equation, a step
 % f(Old)=Ans, before the substitution equation is solved.

 % Substitution Equation is found.
chunk_part(_,Subs,[Subs1=Ans|R],[],[single(Subs=Ans)|R]) :- 
	match(Subs,Subs1),
	!.

 % Disjunctive solution is found
chunk_part(X,Subs,[H|R],[],[mult(H)|R])  :- 
	mod_dis_solution(X,H,Subs),!.

 % Not found yet, so add steps to CVsteps.
chunk_part(X,Subs,[H|R],[H|CVSteps],Substeps) :-
	chunk_part(X,Subs,R,CVSteps,Substeps),
	!.

 % Failure
chunk_part(_,_,_,_,_) :- 
	writef('Failure to find solution of substitution equation.'),
	!.

mod_dis_solution(X,Term=A,Term1) :- !,
	freeof(X,A),
	match(Term,Term1),
	!.

mod_dis_solution(X,A#B,Term) :- !,
	mod_dis_solution(X,A,Term),
	mod_dis_solution(X,B,Term).
mod_dis_solution(_,true,_) :- !.
mod_dis_solution(_,false,_) :- !.
	
work3(Old,New,_,Neweqn,CVsteps,[H|SubsSteps],[message(cve)|NewM],L1,Unks,L2) :- 
	work1([Neweqn|CVsteps],New,NewM,[message(ses)|L3],Unks,L4),
	arg(1,H,H1),
	work1([H1|SubsSteps],Old,L3,L1,L4,L2),
	!.
