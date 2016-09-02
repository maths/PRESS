%   File   : IMETH
%   Author : Bernard Silver
%   Updated: 15 October 1984
%   Purpose: Some Interpreted Method Code for LP

 % The isolate code

isolate([N|Posn],Exp,Ans) :-
	manoeuvre_sides(N,Exp,NewExp),
	isolate1(Posn,NewExp,Inter),
	tidy(Inter,Ans).


 %  get term to be isolated on Rhs 

manoeuvre_sides(1,Exp,Exp) :- !.

manoeuvre_sides(2,A=B,B=A).

 % Perform the Isolation

 % trivial boolean cases

isolate1(_,false,false).
isolate1(_,true,true).

 % deal with each disjunct

isolate1(Posn,Eqn1#Eqn2,Ans1#Ans2) :- 
	!, 
	isolate1(Posn,Eqn1,Ans1),
	isolate1(Posn,Eqn2,Ans2).


 % expression is already isolated

isolate1([],Ans,Ans) :- !.

 % expression can have isolax rule applied

isolate1([N|Posn],Old,Ans) :- !,
	isolax(N,Old,New),
	tidy_rhs(New,New1),
	isolate1(Posn,New1,Ans).

tidy_rhs(A#B,C#D) :- !,tidy_rhs(A,C),tidy_rhs(B,D).
tidy_rhs(A=B,A=C) :- tidy(B,C).
tidy_rhs(false,false) :- !.
tidy_rhs(true,true) :- !.

 % Attract and Collect applied as much as possible

recurse_collect(X,Eqn,New) :- 
	mod_collect(X,Eqn,Mid),
	tidy(Mid,Mid1),
	recurse_collect1(X,Mid1,New).

recurse_collect1(X,Old,New) :-
	mod_collect(X,Old,Mid),
	tidy(Mid,Mid1),
	recurse_collect1(X,Mid1,New).

recurse_collect1(_,Old,Old).

recurse_attract(X,Old,New) :-
	attract(X,Old,Mid),
	tidy(Mid,Mid1),
	recurse_attract1(X,Mid1,New).

recurse_attract1(X,Old,New) :-
	attract(X,Old,Mid),
	tidy(Mid,Mid1),
	recurse_attract1(X,Mid1,New).

recurse_attract1(_,Old,Old).
 % Modified Collection, call Collection after hard_combine

mod_collect(X,A+B,New) :- hard_combine(contains(X,T),A+B,T,New),!.
mod_collect(X,Exp,New) :- collect(X,Exp,New).



attract(X,Exp,New) :- old_attract(X,Exp,New),closer(X,Exp,New).
attract(X,A+B,New) :-
	hard_combine(true,A+B,_,New),
	!,
	closer(X,A+B,New).



 % In the expression A + B, A and B are products which have a common 
 % subterm Y, (which matches Y1) X is the unknown. Term1 And Term2
 % are the rest of A and B.  Keep on applying to remove other
 % stuff as well.

prepfac_terms(Cond,Exp,Y1,NewTerm,Term1,Term2) :- 
	decomp(Exp,[+|PlusBag]),
	map_mult_decomp(PlusBag,[List1|ListList]),
	member(Y1,List1),
	call(Cond),
	map_match_member(Y1,ListList,ListY2),
	!,
	delete_one(Y1,List1,New1),
	map_delete_one(ListY2,ListList,New2),
	prepfac_terms1(New1,New2,NewTerm,1,New3,New4),
	recomp(Term1,[*|New3]),
	map_recomp(ListTerm2,New4,*),
	recomp(Term2,[+|ListTerm2]),
	!.

prepfac_terms1([H|List],List2,NewTerm,Acc,Rest1,Rest2) :-
	map_match_member(H,List2,Mult2),
	!,
	map_delete_one(Mult2,List2,Temp2),
	prepfac_terms1(List,Temp2,NewTerm,Acc*H,Rest1,Rest2).

prepfac_terms1([H|List],List2,NewTerm,Acc,[H|New1],New2) :- !,
	prepfac_terms1(List,List2,NewTerm,Acc,New1,New2).

prepfac_terms1([],List,Acc,Acc,[],List) :- !.

map_mult_decomp([],[]) :- !.
map_mult_decomp([H|T],[H1|T1]) :-
	mult_decomp(H,H1),
	!,
	map_mult_decomp(T,T1).

mult_decomp(A,List) :- decomp(A,[*|List]),!. 
mult_decomp(A,[A]).

map_delete_one([],[],[]) :- !.
map_delete_one([Term|Tail],[H|T],[H1|T1]) :-
	delete_one(Term,H,H1),
	!,
	map_delete_one(Tail,T,T1).

map_match_member(_,[],[]) :- !.
map_match_member(Term,[H|T],[H1|T1]) :-
	match_member(Term,H,H1),
	!,
	map_match_member(Term,T,T1).

map_recomp([],[],_).
map_recomp([H|T],[H1|T1],Op) :-
	recomp(H,[Op|H1]),
	map_recomp(T,T1,Op).

 % Use to rewrite
hard_combine(Cond,Exp+Exp1,Mult,New) :-
	prepfac_terms(Cond,Exp+Exp1,Mult,Y,Term1,Term2),
	!,
	tidy(Mult*Y*(Term1+Term2),New).

