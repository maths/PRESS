%   File   :  /usr/bs/lpdir/constr.pl
%   Author : Bernard Silver
%   Updated: Tue Oct 15 12:00:24 1985
%   Purpose: Propogate constraints

 % Partition list into seperate solution paths 
partition_the_list(X,List,[Main|P],List1,[Main1|P1],List2,[Main2|P2],Type) :-
	sort_of_list(List,Type),
	find_first_part(Type,X,List,Main,R,List1,Main1,R1,List2,Main2,R2),
	partition_rest(Type,X,R,P,R1,P1,R2,P2).

sort_of_list(List,'Change of Unknown') :- member('Change of Unknown',List),!.
sort_of_list(List,'Factorization') :- member('Factorization',List),!.
sort_of_list(_,'General') :- !.

find_first_part('Change of Unknown',_,['Change of Unknown'|T],
	['Change of Unknown'],T,[H|T1],[H],T1,[H1|T2],[H1],T2) :- !.
find_first_part('Factorization',_,['Factorization'|T],['Factorization'],T,
[H|T1],[H],T1,[H1|T2],[H1],T2) :- !.

find_first_part(Ty,X,[H|T],Main,R,[H1|T1],[H1|Main1],R1,[H2|T2],
[H2|Main2],R2) :- 
	ignore_term(H),
	!,
	find_first_part(Ty,X,T,Main,R,T1,Main1,R1,T2,Main2,R2).

find_first_part('General',X,List,New,[],List1,New1,[],List2,New2,[]) :- !,
	p_r1('General',X,List,New,_,List1,New1,_,List2,New2,_).
find_first_part(Ty,X,[H|T],[H|Main],R,[H1|T1],[H1|Main1],R1,[H2|T2],
[H2|Main2],R2) :- 
	find_first_part(Ty,X,T,Main,R,T1,Main1,R1,T2,Main2,R2).
find_first_part(_,_,[],[],[],[],[],[],[],[],[]) :- !.


partition_rest(_,_,[],[],[],[],[],[]) :- !.
partition_rest(Type,X,List,[F|P],List1,[F1|P1],List2,[F2|P2]) :-
	p_r1(Type,X,List,F,R,List1,F1,R1,List2,F2,R2),
	!,
	partition_rest(Type,X,R,P,R1,P1,R2,P2).


p_r1(Type,X,[H|T],F,R,[H1|T1],[H1|F1],R1,[H2|T2],[H2|F2],R2) :-
	ignore_term(H),
	!,
	p_r1(Type,X,T,F,R,T1,F1,R1,T2,F2,R2).
p_r1(_,_,[H|T],[H],T,[H1|T1],[H1],T1,[X|T2],[X],T2) :- 
	dis_solution(H1,X),
	!.
p_r1('Change of Unknown',X,[cve|T],F,R,[_|T1],F1,R1,[_|T2],F2,R2) :- !,
	p_r1('Change of Unknown',X,T,F,R,T1,F1,R1,T2,F2,R2).
p_r1('Change of Unknown',X,[ses|T],F,R,[_|T1],F1,R1,[_|T2],F2,R2) :- !,
	p_r1('Change of Unknown',X,T,F,R,T1,F1,R1,T2,F2,R2).
p_r1(Type,X,[start|T],F,T1,[_|T2],F1,R1,[_|T3],F2,R2) :- !,
	p_r1(Type,X,T,F,T1,T2,F1,R1,T3,F2,R2).
p_r1(Type,X,[H|T],[H|F],R,[H1|T1],[H1|F1],R1,[H2|T2],[H2|F2],R2) :- !,
	p_r1(Type,X,T,F,R,T1,F1,R1,T2,F2,R2).

ignore_term(nd).
ignore_term(ff).
ignore_term(nf).
ignore_term(start).

find_the_purpose(_,_,[],[],[],[]) :- !.
find_the_purpose(U,Unk1,[H|T],[H1|T1],[U1|TU],[H2|T2]) :-
	find_the_purpose1(U,Unk1,H,H1,U1,H2),
	!,
	find_the_purpose(U,Unk1,T,T1,TU,T2).

find_the_purpose1(_,_,[],_,_,[]) :- !.
find_the_purpose1(Unk,_,[H],[A|_],[X|_],
	[conditions(H,sat(Sat,Unk,Eqn),unsat([finish],_,_))]) :- !,
	get_preconditions(Unk,H,Eqn,Pre),
	find_ok_preconds(Unk,X,A,Eqn,Pre,Sat).

find_the_purpose1(Unk,Unk1,[H,H1|T],[A,B|T1],[X,X|UT],
	[conditions(H,sat(Sat,Unk1,Eqn2),unsat(Unsat,Unk,Eqn1))|T2]) :-
	get_preconditions(Unk,H1,Eqn1,Pre1),
	get_preconditions(Unk1,H1,Eqn2,Pre2),
	find_missing_preconds(B,Unk,X,A,Unsat,Pre1,Eqn1),
	find_ok_preconds(Unk,X,A,Eqn2,Pre2,Sat),
	!,
	find_the_purpose1(Unk,Unk1,[H1|T],[B|T1],[X|UT],T2).


