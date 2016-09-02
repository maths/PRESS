
%   File   : CONJ
%   Author : Bernard Silver
%   Updated: 20 August 1984

:- dynamic
	difficult_once/2.



conjecture_steps([Eqn|Example1],X,Unksteps,Conjlist,UnkList,Method,Method2) :-
	make_unk_list([Eqn|Example1],X,List1,UnkList),
	find_unknown_steps([Eqn|Example1],List1,Unksteps,NewUnk),
	conjecture_the_steps(Eqn,X,NewUnk,Conjlist,Method,Method2).


  % Find unknowns for each step
make_unk_list(A,X,B,List) :- make_unk_list1(A,X,X,B,List),!.

make_unk_list1([],_,_,[],[]) :- !.

make_unk_list1([false|T],Old,Old,[su(false,Old)|T1],[Old|T2]) :- !,
	make_unk_list1(T,Old,Old,T1,T2).

make_unk_list1([true|T],Old,Old,[su(true,Old)|T1],[Old|T2]) :- !,
	make_unk_list1(T,Old,Old,T1,T2).

 % Change of Unknown
make_unk_list1([Y=Sub|T],Old,Unk,[su(Y=Sub,Y)|T1],[Y|T2]) :-
	freeof(Unk,Y),
	contains(Unk,Sub),
	make_unk_list1(T,Old,Y,T1,T2),
	!.

 % Change back
make_unk_list1([Y=X|T],Old,Unk,[su(Y=X,Old)|T1],[Old|T2]) :-
	freeof(Unk,Y),
	freeof(Old,X),
	contains(Old,Y),
	make_unk_list1(T,Old,Old,T1,T2),
	!.

make_unk_list1([H|T],Unk,X,[su(H,X)|T1],[X|T2]) :-
	contains(X,H),
	make_unk_list1(T,Unk,X,T1,T2),
	!.

make_unk_list1([H|T],Unk,X,[su(H,Unk)|T1],[Unk|T2]) :-
	contains(Unk,H),
	make_unk_list1(T,Unk,Unk,T1,T2),
	!.

 % Conjecture reasons for steps

conjecture_the_steps(_,_,[],[],L,L) :- reset,!.
conjecture_the_steps(Eqn,X,A,Conj,L,L1) :- !,
	check_no_conjectures(Eqn,X),
	((A = [_] -> Flag = '.');Flag='s.'),
	writef('\nTrying to conjecture reasons for unknown step%w\n',[Flag]),
	conj_steps(A,Flag,Conj1,Namelist),
	zap_stored_conj(Conj1,Conj,Namelist,L,L1).

conj_steps([],Flag,[],[]) :- writef('\nEnd of Conjecture%w\n',[Flag]),!.
conj_steps([H|T],Flag,[Conj1|Conjlist],NewNames) :-
	write_step(H),
	conjecture_steps1(H,Conj,Conj1,Ok,Type),
	H = st(A,X,B,_),
	confirm_conj(Ok,Conj,X,A,B,Type,Name),
	(((var(Type),var(Name)) -> NewNames = Rest);
	NewNames =[change_name(Name,A,B)|Rest]),
	conj_steps(T,Flag,Conjlist,Rest),
	!.
	


conjecture_steps1(st(A,X,B,X),C,C1,Flag,Name) :- 
	retract(p_m_r(Name,A,B)),
	retract_rest(A,B),
	(Name = method(Name1);Name=Name1),
	!,
	known_method(_,_,_,Name,Type,_,_,_),
	find_parts_of_equation(Type,A,B,A1,B1),
	Diff =.. [Name1,A1,B1],
	writef('\nStep is an application of %w\n',[Name1]),
	remake_conjecture(X,A,B,NewC,Name1),
	(NewC=diff(Old,New);NewC=re(diff(Old,New))),
	writef('\n\nConjecture that\n\n%t\n\n\t\t->\n\n%t\n',[Old,New]),
	(Name = 'Polynomial Methods' -> Flag = yes1;Flag=yes),
	!,
	process_diff(diff(Diff),A1,B1,C,C1).

conjecture_steps1(st(A=B,X,C=B,X),Conj,Conj1,yes,Type) :- 
	find_diff(X,A,C,Diff,Type,A=B,C=B), 
	process_diff(Diff,A=B,C=B,Conj,Conj1),
	!.

conjecture_steps1(st(A,X,B,X),C,C1,F,Type) :- 
	!,
	find_diff_hard(A,B,X,F,C,C1,Type,A,B).

conjecture_steps1(st(Old,X,New,Y),chunk,Conj,yes1,_) :-
	flag(method('Change of Unknown'),on,on),
	X\=Y,
	writef('\nSome sort of change of unknown seems to have happened.\n'),
	Conj = conj_mess('Change of Unknown',ch(X,Y),Old,New),
	!.

conjecture_steps1(st(Old,_,New,_),_,Conj,no,_) :- 
	writef('\nUnable to conjecture reason for this step.\n'),
	Conj = conj_mess(fail(New),_,Old,New),
	!.

 % Hack so that only top most 'possible missing rule' is considered
retract_rest(A,B) :-
	retract(p_m_r(_,A,B)),
	fail.

retract_rest(_,_).

find_parts_of_equation(all,A,B,A,B) :- !.
find_parts_of_equation(part,A=B,C=B,A,C) :- !.
find_parts_of_equation(_,A,B,A,B) :- !.


% We already know that step is done by a missing rule of a certain method.
% The remake_conjecture clauses try to produce the conjectures that can be used by the rest
% of the program

remake_conjecture(_,Old,New,diff(Old,New),Name) :-
      (Name = 'Isolation'; Name = 'Function Stripping'),
      !.

% Lets decompose the terms to get a nice conjecture.
remake_conjecture(X,A=C,B=C,Conj,_) :-
	functor(A,F,N),
	functor(B,F,N),
	decomp(A,[F|Arg1]),
	decomp(B,[F|Arg2]),	
	find_diff3(X,F,Arg1,Arg2,Conj,_,A=C,B=C),
	!.

 % Like previous case, but LHS of first term is a plus bag, and LHS of second term
 % is a member of this bag.  This extra case is needed because B doesnt appear to be a plus bag
 % (the functor test above fails)

remake_conjecture(X,A+A1=C,B=C,Conj,_) :-
	decomp(A+A1,[F|Arg1]),
	member(B,Arg1),
	find_diff3(X,F,Arg1,[B],Conj,_,A+A1=C,B=C),
	!.

remake_conjecture(X,A=C,B=D,Diff,_) :-
	C \== 0,
	tidy_expr(A-C=0,New1=0),
	tidy_expr(B-D=0,New2=0),
	find_diff1(X,New1,New2,Diff,_,A=C,B=D),
	!.

remake_conjecture(_,Old,New,diff(Old,New),_).

  % Find differences
find_diff(X,Term1,Term2,Diff,Type,Eqn1,Eqn2) :- 
	find_diff1(X,Term1,Term2,Diff,Type,Eqn1,Eqn2),
	output_diff(Diff),
	!.

find_diff1(X,Term1,Term2,Diff,Type,Eqn1,Eqn2) :- 
	functor(Term1,F,N),
	functor(Term2,F,N),
	decomp(Term1,[F|Arg1]),
	decomp(Term2,[F|Arg2]),	
	find_diff3(X,F,Arg1,Arg2,Diff,Type,Eqn1,Eqn2),
	!.

find_diff1(X,Term1,Term2,Diff,Type,Eqn1,Eqn2) :-
	find_diff2(X,Term1,Term2,Diff,Type,Eqn1,Eqn2),!.


find_diff2(X,Term1,Term2,diff(Diff),Name,Old,New) :-
	retract(p_m_r(Name,Old,New)),
	retract_rest(Old,New),
	(Name = method(Name1);Name=Name1),
	writef('\nStep is an application of %w\n',[Name1]),
	!,
	Diff =.. [Name1,Term1,Term2].

find_diff2(X,Term1,Term2,diff(T1,T2),_,Old,New) :-
	good_structure(Term1,Term2,T1a,T2a),
	!,
	find_diff2(X,T1a,T2a,diff(T1,T2),_,Old,New).


find_diff2(_,A,B,diff(A,B),_,_,_) :- !.

find_diff3(X,F,Arg1,Arg2,re(Diff),Type,Eqn1,Eqn2) :-
	associative(F),
	remove_match(Arg1,Arg2,New1,New2),
	shift_numbers(New1,New2,NewAr1,NewAr2,F),	
	remove_match1(NewAr1,NewAr2,A,B,F),
	find_diff2(X,A,B,Diff,Type,Eqn1,Eqn2),
	!.

find_diff3(_,_,Arg1,Arg2,Diff,_,_,_) :-
	only_diff(Arg1,Arg2,Diff),
	!.

good_structure(Term1^N,Term2,Term1,New2) :-
	ok_number(N),
	inverse_number(^,N,N1),
	final_tidy_expr(Term2^N1,New2),
	!.
	
good_structure(A*Term1,Term2,T1,T2) :-
	decomp(A*Term1,[*|List]),
	select(Num,List,Rest),
	ok_number(Num),
	!,
	inverse_number(*,Num,Inv),
	recomp(T1,[*|Rest]),
	g_s1(Inv,Term2,T2).

g_s1(Number,A*B,Term) :- !,
	decomp(A*B,[*|List]),
	collect_numbers_now([Number|List],Numbers,Others,[],[]),
	recomp(NewNum,[*|Numbers]),
	eval(NewNum,NN),
	recomp(Term1,[*|Others]),
	final_tidy_expr(NN*Term1,Term).

g_s1(Number,T,Term) :- 
	final_tidy_expr(Number*T,Term).

 % Args differ in only one entry
only_diff(List1,List2,Diff) :- diff_list(List1,List2,[],[Diff]),!.

diff_list([],[],X,X) :- !.
diff_list([H|T],[H|T1],Acc,Diff) :- diff_list(T,T1,Acc,Diff),!.
diff_list([H|T],[H1|T1],Acc,Diff) :- diff_list(T,T1,[diff(H,H1)|Acc],Diff),!.

remove_match([],List,[],List) :- !.
remove_match([H|T],List,New1,New2) :-
	delete_member(H,List,New),
	!,
	remove_match(T,New,New1,New2).

remove_match([H|T],List,[H|New1],New2) :-
	remove_match(T,List,New1,New2).

 % Delete term from list to get new list
delete_member(H,[H|T],T) :- !.
delete_member(H,[H1|T],[H1|T1]) :- delete_member(H,T,T1).


remove_match1([A],[B],NewT1,NewT2,_) :-
	A =.. [F|Arg1],
	B =.. [F|Arg2],
	associative(F),
	remove_match(Arg1,Arg2,Arg3,Arg4),
	recomp(New1,[F|Arg3]),
	tidy(New1,NewT1),
	recomp(New2,[F|Arg4]),
	tidy(New2,NewT2),
	!.

remove_match1(List1,List2,TTerm1,TTerm2,F) :-
	recomp(Term1,[F|List1]),
	tidy(Term1,TTerm1),
	recomp(Term2,[F|List2]),
	tidy(Term2,TTerm2),
	!.

 % Move numbers from left hand side to right hand side of rewrite rule
shift_numbers([],L,[],L,_) :- !.
shift_numbers([H|T],L,L1,[H1|L2],F) :-
	ok_number(H),
	!,
	inverse_number(F,H,H1),
	shift_numbers(T,L,L1,L2,F).

shift_numbers([H|T],L,[H|L1],L2,F) :-
	shift_numbers(T,L,L1,L2,F).

inverse_number(+,Old,Inv) :- !,eval(-Old,Inv).
inverse_number(*,Old,Inv) :- !,eval(1/Old,Inv).
inverse_number(^,Old,Inv) :- !,eval(1/Old,Inv).

 % Harder differences

 % Recognise disjunctive solution

 % Work with both sides
find_diff_hard(A=B,C=D,X,yes,Conj,Conj1,Type,E1,E2) :- !,
	find_both_sides_difference(X,A=B,C=D,Conj,Conj1,Type,E1,E2).

find_diff_hard(_=_,_,_,_,_,_,_,_,_) :-
	writef('\nSorry, I cannot handle this case'),
 writef('[equation goes to a non-equation]  I have to stop here.\n\n'),
	!,
	fail.

find_both_sides_difference(X,A=B,C=D,Conj,Conj1,Type,E1,E2) :-
	B \==0,  % to stop hidden divisions
	tidy_expr(A-B=0,New1=0),
	tidy_expr(C-D=0,New2=0),
	find_diff1(X,New1,New2,Diff,Type,E1,E2),
	Diff \= diff(_,_),
	Diff \= re(diff(_,_)),
	!,
	writef('\n\nConjecture that\n\n%t\n\n\t\t=\n\n%t\n',[New1,New2]),
	process_diff(Diff,A=B,C=D,Conj,Conj1).


 % Default
find_both_sides_difference(_,A=B,C=D,Co,Co1,_,E1,E2) :-
	writef('\n\nConjecture that\n\n%t\n\n\t\t->\n\n%t\n',[A=B,C=D]),
	process_diff(re(diff(A=B,C=D)),A=B,C=D,Co,Co1).


 % Process rule obtained from user
obtain_rule1(_,_,Rule,_,_) :-
	functor(Rule,F,_),
	F \= =>,
	!,
writef('\nYou have used the wrong connective, %t, in the rule.\n',[F]),
	(F=(=) -> output_real_rule(Rule);true),
	!,
	text_not_used,
	writef('\nPlease try again.\n'),
	fail.

output_real_rule(Rule) :-
	Rule =.. [_|Arg],
	R1 =.. [=>|Arg],
	num_writef('\n[Rule\n\n%t\n\nshould probably be\n\n%t]\n',[Rule,R1]),
	!.

obtain_rule1(X,Unk,L=>R,Cond,rule(_,Old,New)) :-
	X=Unk,
	occ(X,New,F),
	try_use_rule(Unk,Old,New1,L=>R,Cond,[],F),
	(tidy_expr(New1,New2);tidy(New1,New2)),
	match_check(New,New2),
	!.

obtain_rule1(_,_,_,_,_) :-
	writef('\n
This rule does not appear to apply to the step, please try again.\n'),
	!,
	fail.


 % Replace information in stored conjectures with rule name

zap_stored_conj(Old,Old,[],L,L) :- !.

zap_stored_conj(Old,New,[change_name(Name,T1,T2)|T],L,L1) :- 
	zap_member(OldName,A,B,T1,T2,Old,NewA,NewB,NewT1,NewT2),
	member(fail,L),
	!,
	perform_rename(OldName,NewName,NewT1,NewT2,Name),
	subst(conj_mess(OldName,rule(A,B),T1,T2) =
		conj_mess(NewName,rule(NewA,NewB),NewT1,NewT2),
	Old,Mid),
	first_subst(fail=NewName,L,L2),
	zap_stored_conj(Mid,New,T,L2,L1).

 % zap_member is a HACK!!!  Sometimes Old will contain the whole equation
 % sometimes only the LHS.  Correcting the cause would take too long.

zap_member(OldName,A,B,T1,T2,Old,A,B,T1,T2) :-
	member(conj_mess(OldName,rule(A,B),T1,T2),Old),
	!.

zap_member(OldName,A=C,B=D,T1=C,T2=D,Old,A,B,T1,T2) :-
	member(conj_mess(OldName,rule(A,B),T1,T2),Old),
	!.

perform_rename(X,X,_,_,_) :- method(X),!.
perform_rename(X,method(X),_,_,_) :- method(method(X)),!.
perform_rename(_,user_rule(Name,T1,T2),T1,T2,Name) :- !.

first_subst(_,[],[]) :- !.
first_subst(A=B,[A|T],[B|T])  :- !.
first_subst(S,[H|T],[H|T1]) :-
	first_subst(S,T,T1).
	
 % Has this problem been tried before
check_no_conjectures(Eqn,X) :- 
	difficult_once(Eqn,X),
	!,
writef('\nStill finding difficulties with this problem, entering break.\n'),
	reset,
	break,
	fail.

check_no_conjectures(Eqn,X) :- asserta(difficult_once(Eqn,X)).

collect_numbers_now([],Numbers,Rest,Numbers,Rest) :- !.


collect_numbers_now([H|T],Number,Rest,Numbers,RestAcc) :-
	ok_number(H),
	!,
	collect_numbers_now(T,Number,Rest,[H|Numbers],RestAcc).

collect_numbers_now([H|T],Number,Rest,Numbers,RestAcc) :-
	collect_numbers_now(T,Number,Rest,Numbers,[H|RestAcc]).
