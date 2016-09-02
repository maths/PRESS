%   File   : NEWRUN.PL
%   Author : Bernard Silver
%   Updated: 28 July 1985
%   Purpose: Run test questions for LP

:- d loop.
:- dynamic q/3.

 % The questions

q(A,1,T-F):- !,solve(sec(2*x) + tan(2*x) = 3,x,A,T,F),!.    
q(A,3,T-F):- !,solve(4^(2*x+1) * 5^(x-2) = 6^(1-x),x,A,T,F),!.
q(A,4,T-F):- !,solve(1 - 3*cos(x)^2 = 5*sin(x),x,A,T,F),!.
q(A,6,T-F):- !,solve(cos(x) + 2*cos(2*x) + cos(3*x) = 0,x,A,T,F),!.
q(A,7,T-F):- !,solve(2*sin(x) + cos(x) = 1,x,A,T,F),!.
q(A,8,T-F):- !,solve(2*sin(x) + cos(2*x) = 1,x,A,T,F),!.
q(A,10,T-F):- !,solve(9^(3*x^2) = 27^(15-x),x,A,T,F),!.
q(A,11,T-F):- !,solve(log(e,2*x-5) + log(e,x-3) = 2*log(e,2*x-1) - log(e,2),x,A,T,F),!. 
q(A,12,T-F):- !,solve(cos(6*x) + sin(6*x) + cos(4*x) + sin(4*x) = 0,x,A,T,F),!. 
q(A,13,T-F):- !,solve(cos(2*x) + 3*sin(x) + 1 = 0,x,A,T,F),!.
q(A,15,T-F):- !,solve(sin(2*x) + sin(3*x) + sin(5*x) = 0,x,A,T,F),!.
q(A,16,T-F):- !,solve(3*sin(x) + 4*cos(x) = 1,x,A,T,F),!.
q(A,18,T-F):- !,solve(5*cos(2*x) - 2*sin(2*x) =  2,x,A,T,F),!.
q(A,19,T-F):- !,solve(3*cos(x)^2 + 5*sin(x) - 1 = 0,x,A,T,F),!.
q(A,21,T-F):- !,solve(log(x,8) + log(8,x) = 13/6,x,A,T,F),!.
q(A,22,T-F):- !,solve(sin(x) - sin(4*x) + sin(7*x) = 0,x,A,T,F),!. 
q(A,24,T-F):- !, solve(7*sin(x) - 24*cos(x) = 15,x,A,T,F),!.
q(A,25,T-F):- !, solve(cos(x) + cos(3*x) + cos(5*x) = 0,x,A,T,F),!.
q(A,26,T-F):- !, solve(2*sec(x) + 3*sin(x) = 4*cos(x),x,A,T,F),!.
q(A,28,T-F):- !, solve(cos(5*x) = cos(2*x),x,A,T,F),!.
q(A,29,T-F):- !, solve(10^(x - 3) = 2^(10 + x),x,A,T,F),!.
q(A,30,T-F):- !, solve(cot(2*x) = 2 + cot(x),x,A,T,F),!.
q(A,31,T-F):- !, solve(cos(3*x) - 3*cos(x) = cos(2*x) + 1,x,A,T,F),!.
q(A,33,T-F):- !, solve(sin(x) + sin(2*x) = sin(3*x),x,A,T,F),!.
q(A,34,T-F):- !, solve(2*tan(x) + sec(2*x) = 2*tan(2*x),x,A,T,F),!.
q(A,35,T-F):- !, solve(log(x,45)+4*log(x,2)-(1/2)*log(x,81)-log(x,10)=3/2,x,A,T,F),!.
q(A,37,T-F):- !, solve(8*cos(x) - 15*sin(x) = 3,x,A,T,F),!.
q(A,39,T-F):- !, solve(2*sin(x) + cos(x) + 2 = 0,x,A,T,F),!.
q(A,40,T-F):- !, solve(7*sin(x)^2 - 5*sin(x) + cos(x)^2 = 0,x,A,T,F),!.
q(A,41,T-F):- !, solve(8*sin(x) + 15*cos(x) = 17/2,x,A,T,F),!.
q(A,43,T-F):- !, solve(log(2,x) + 4*log(x,2) = 5,x,A,T,F),!.
q(A,46,T-F):- !, solve(sin(2*x) = sin(x),x,A,T,F),!.
q(A,47,T-F):- !, solve(sin(x) - 7*cos(x) + 5 = 0,x,A,T,F),!.
q(A,48,T-F):- !, solve(cos(3*x) + sin(3*x) = 1,x,A,T,F),!.
q(A,49,T-F):- !,solve(sin(3*x) = sin(x)^2,x,A,T,F),!. 
q(A,50,T-F):- !,solve(4*cos(x) + sin(x) = 1,x,A,T,F),!.  
q(A,51,T-F):- !,solve(4^(3+x)/8^(10*x) = 2^(10 - 2*x)/64^(3*x),x,A,T,F),!. 
q(A,52,T-F):- !,solve(log(2,(x+4)) = 2 - log(2,x),x,A,T,F),!. 
q(A,53,T-F):- !,solve(6^(1/2)*cos(x) - 2^(1/2)*sin(x) = 2,x,A,T,F),!. 
q(A,55,T-F):- !,solve(sin(x) = cos(a),x,A,T,F),!.
q(A,56,T-F):- !,solve(e ^ (log(e,x)) + log(e,e^x) = 8,x,A,T,F),!. 
q(A,57,T-F):- !,solve(sin(2*x) + sin(x) = 0,x,A,T,F),!. 
q(A,122,T-F):- !,solve(sin(x) + cos(x)*cos(2*x)=cos(2*x)*cos(3*x),x,A,T,F),!.
q(A,666,T-F):- !,solve((x+ 3/(8*x)+ 3/(8*x))^2 + 2*(x+3/(4*x))+1= 0,x,A,T,F),!.
/*
q(A,59,T-F):- !,solve(2^(2/x) = 32,x,A,T,F),!. 
q(A,60,T-F):- !,solve(log(x,2)*log(x,3) = 5,x,A,T,F),!. 
q(A,64,T-F):- !,solve(sin(x) - cos(x) = 1,x,A,T,F),!.  % solved
q(A,65,T-F):- !,solve(cos(2*x) + 1 = sin(2*x),x,A,T,F),!. 
q(A,66,T-F):- !, solve(8*cos(x) - sin(x) = 4,x,A,T,F),!.
q(A,67,T-F):- !, solve(2*sin(x)^2 - 1 = (1 + cos(x))^2,x,A,T,F),!.
q(A,68,T-F):- !, solve(sec(x) - 1/sec(x) = sin(x),x,A,T,F),!.
q(A,69,T-F):- !, solve(3*sin(x)^2  - cos(x) - 1 = 0,x,A,T,F),!.
q(A,71,T-F):- !, solve(3*tan(3*x) - tan(x) + 2 = 0,x,A,T,F),!. 
q(A,72,T-F):- !, solve(sin(3*x) = 2*sin(x),x,A,T,F),!. 
q(A,74,T-F):- !, solve(4*cos(x) + 3*sin(x) = 2,x,A,T,F),!. 
q(A,76,T-F):- !,solve(150*cos(x) + 80*sin(x) = 51,x,A,T,F),!.
q(A,78,T-F):- !,solve(3*cos(x) + 2*sec(x) + 5 = 0,x,A,T,F),!.
q(A,79,T-F):- !,solve(sin(x) + 7*cos(x) = 5,x,A,T,F),!.
q(A,80,T-F):- !,solve(4*tan(2*x) + 3*cot(x)*sec(x)^2 = 0,x,A,T,F),!.   
q(A,81,T-F):- !,solve(sin(2*x) = cos(x),x,A,T,F),!.
q(A,82,T-F):- !,solve(sin(5*x) + sin(3*x) = 0,x,A,T,F),!.
q(A,86,T-F):- !,solve(sin(3*x) - sin(x) = cos(2*x),x,A,T,F),!.
q(A,87,T-F):- !,solve(6*sin(x) + 8*cos(x) = 5,x,A,T,F),!.
q(A,88,T-F):- !,solve(3*cos(x) + 4*sin(x) = (5/2)*(3)^(1/2),x,A,T,F),!. 
q(A,89,T-F):- !,solve(cos(x) + cos(3*x) = 0,x,A,T,F),!.  
q(A,90,T-F):- !,solve(sin(x) + 3^(1/2)*cos(x) = 1,x,A,T,F),!. 
q(A,91,T-F):- !,solve(2*cos(2*x) + sin(2*x) = 5^(1/2)/2,x,A,T,F),!.  
q(A,92,T-F):- !,solve(sin(x) + sin(3*x) = 0,x,A,T,F),!. 
q(A,93,T-F):- !,solve(2*sin(x) - 3*cos(x) = 1,x,A,T,F),!. 
q(A,95,T-F):- !,solve(5*cos(x)^2 - 12*sin(x)*cos(x) = 2,x,A,T,F),!. 
q(A,96,T-F):- !, solve(sin(3*x) = -1/2,x,A,T,F),!.
q(A,97,T-F):- !,solve(2*cos(x) - sin(x) = 1,x,A,T,F),!.  
q(A,99,T-F):- !,solve(10*cos(x)^2 + sin(x) - 7 = 0,x,A,T,F),!.  
q(A,100,T-F):- !,solve(sin(5*x) + sin(x) = 3*cos(2*x),x,A,T,F),!.
q(A,101,T-F):- !,solve(2*cos(2*x) + cos(x) - 1 = 0,x,A,T,F),!.
q(A,102,T-F):- !,solve(sin(5*x/2) - sin(3*x/2) = 0,x,A,T,F),!.
q(A,104,T-F):- !,solve(sin(3*x) - sin(x) = 1/2*cos(2*x),x,A,T,F),!.
q(A,105,T-F):- !,solve(3*cos(x)+6*sin(x)=1,x,A,T,F),!.
q(A,106,T-F):- !,solve(tan(x)=tan(4*x),x,A,T,F),!.
q(A,108,T-F):- !,solve(log(4,x)+log(2,x)=6,x,A,T,F),!.
q(A,109,T-F):- !,solve(cos(2*x)-2*sin(2*x)=2,x,A,T,F),!.
q(A,110,T-F):- !,solve(sin(x) - sin(2*x) + sin(3*x)=0,x,A,T,F),!.
q(A,111,T-F):- !,solve(3*log(8,x) - 8*log(x,2)=8,x,A,T,F),!.
q(A,112,T-F):- !,solve(6*sin(x)^2 - cos(x) = 5,x,A,T,F),!.
q(A,113,T-F):- !,solve(tan(2*x)+4*cot(x) = 0,x,A,T,F),!.
q(A,114,T-F):- !,solve(sec(x)^2 = 3+tan(x),x,A,T,F),!.
q(A,115,T-F):- !,solve(sin(2*x)=4*cos(2*x)-1,x,A,T,F),!.
q(A,116,T-F):- !,solve(log(3,(9*x-2)) = 2+2*log(3,x),x,A,T,F),!.
q(A,120,T-F):- !,solve((10/98)^x = 15/10*10^947,x,A,T,F),!.
q(A,121,T-F):- !,solve(5*sin(x)^2 + sin(x)*cos(x) = 3,x,A,T,F),!.

q(A,123,T-F):- !,solve(2*3^(1/2)*sin(x)^2 -sin(2*x) = 0,x,A,T,F),!.
q(A,124,T-F) :- solve(sin(5*x) - sin(3*x) = sin(4*x)-sin(2*x),x,A,T,F).
q(A,125,T-F):- !,solve(sin(x)-2*cos(x) = 1,x,A,T,F),!.
q(A,127,T-F):- !,solve(4*sin(2*x) + 3*cos(2*x) = 249/100,x,A,T,F),!.
q(A,128,T-F):- !,solve(cos(x)-cos(2*x)=1/2,x,A,T,F),!.
q(A,130,T-F):- !,solve(sec(x)^2 - 3*tan(x) - 5 = 0,x,A,T,F),!.
q(A,131,T-F):- !,solve(cos(4*x) + cos(2*x) - sin(4*x) + sin(2*x) = 0,x,A,T,F),!.
q(A,132,T-F):- !,solve(log(3,x)=log(x,2),x,A,T,F),!.
q(A,133,T-F):- !,solve(6^(3-4*x)*4^(x+4)=2,x,A,T,F),!.
q(A,134,T-F):- !,solve(8*sin(x) + cos(x)=4,x,A,T,F),!.
q(A,137,T-F):- !,solve(4*log(3,x) = 9*log(x,3),x,A,T,F),!.
q(A,139,T-F):- !,solve(sin(x) + 2*cos(x) = 0,x,A,T,F),!.
q(A,140,T-F):- !,solve(sin(x) + 2*cos(x) = 1,x,A,T,F),!.
q(A,142,T-F):- !,solve(2*sec(x/2)^2 - tan(x/2)^2 = 0,x,A,T,F),!.
q(A,145,T-F):- !,solve(sin(x) + cos(x) = 1/2,x,A,T,F),!.
*/
 % The tests

get_test_list(X,[1,3,4,6,7,8,10,11,13,15,16,18,19,21,22,24,25,26,28,29,30,31,
33,35,37,39,40,41,43,46,47,48,49,50,51,52,53,55,56,57,122,666]):- 
%,59,60,64,65,66,67,68,69
	(X=1;X=2;X=3),
	!.
get_test_list(X,[1,3,4,6,7,8,10,11,12,13,15,16,18,19,21,22,24,25,26,28,29,30,
31,33,35,37,39,40,41,43,46,47,48,49,50,51,52,53,55,56,57,122,666]) :- (X=4;X=5;X=6),!.

get_test_list(7,[59,60,64,65,66,67,68,69,71,72,74,76,78,79,80,
81,82,86,87,88,89,90,91,92,93,95,96,97,99,100,101,102,
104,105,106,108,109,110,111,112,113,114]).
get_test_list(8,[115,116,120,121,123,124,125,127,128,
130,131,132,133,134,137,139,140,142,145]).


extra_questions(X,[]) :- X > 3,!.
extra_questions(_,[12]).

% Running them

test(Number):-
	get_test_list(Number,List),
	!,
	ac_t(Number,File,StatsFile),
	listrun(List),
	!,
	tell(File),
	count(Number),
	told,
	dump_stats(StatsFile).


listrun(List):-
	listrun1(List,Suc,Fail),
	!,
	report_scores(Suc,Fail).

listrun1([],[],[]):- !.
listrun1([H|T],SList,FList):-
	writef('\nTrying question %t\n',[H]),
	clause(q(_,H,_),_),
	!,
	call(q(A,H,Time)),
	!,
	mark_ans(H-(Time),A,SList,FList,NewS,NewF),
	!,
	listrun1(T,NewS,NewF).
listrun1([H|T],L,L1):-
	writef('\nQuestion %t doesn''t exist!.  Skipping\n',[H]),
	!,
	listrun1(T,L,L1).

mark_ans(H,A,[H|RestS],Fail,RestS,Fail):-
	nonvar(A),
	!.
mark_ans(H-Time,_,Suc,[H-(Time-'(f)')|RestF],Suc,RestF):- !.

:- dynamic runscores/2, successes/1,failures/1.


report_scores(Suc,Fail):- !,
	length(Suc,L),
	length(Fail,L1),
	writef('\nFinished\n\nSucceed %t Fail %t\n',[L,L1]),
	assertz(runscores(L,L1)),
	assertz(successes(Suc)),
	assertz(failures(Fail)).




output_scores(List) :-
	(setof(S,successes(S),SSet);SSet=[]),
	(setof(F,failures(F),FSet);FSet=[]),
	!,
	join_scores('Successes',SSet,NewS,STime),
	join_scores('Failures',FSet,NewF,FTime),
	length(NewS,SL),
	length(NewF,FL),
	write_totals(STime,SL,FTime,FL),
	output_untried(List),
	!.

output_untried([]) :- !.
output_untried(L) :-
	(L=[_,_|_] ->W = 's were';W=' was'),
	writef('\nThe following question%w not tried: %t\n',[W,L]),
	!.

join_scores(Word,Set,New,Time):-
	mult_append(Set,New),
	(New == [] -> (Time = 0,writef('\nNo %w\n',[Word]));
	join_scores_cont(Word,New,Time)),
	!.

join_scores_cont('Successes',New,Time) :- !,
	list_times_and_scores(New),
	total_time(New,Time),
	!.
join_scores_cont(_,List,Time) :-
	remove_var_markers(List,New),
	list_times_and_scores(New),
	total_time(New,Time),
	!.

remove_var_markers([],[]).
remove_var_markers([H-((Time-X)-F)|T],[H-(Time-F)|T1]) :-
	var(X),
	!,
	remove_var_markers(T,T1).
remove_var_markers([H-((Time-X)-F)|T],[H-(Time-(X-F))|T1]) :-
	remove_var_markers(T,T1).
mult_append([],[]):- !.
mult_append([H|Old],New):- 
	mult_append(Old,H,New).
mult_append([],H,H):- !.
mult_append([H1|T],H,New):-
	append(H,H1,Mid),
	!,
	mult_append(T,Mid,New).
list_times_and_scores([]):- !.
list_times_and_scores([H-H1|T]):- 
	handle_time_output(H,H1),
	!,
	list_times_and_scores(T).

handle_time_output(Number,Time-(S-'(f)')) :- !,
	hand_out1(S,S1),
	writef('%t\t%t(f)\t%t\n',[Number,Time,S1]).
handle_time_output(Number,Time-'(f)') :- !,
	writef('%t\t%t(f)\n',[Number,Time]).
handle_time_output(A,B-C) :- !,
	hand_out1(C,C1),
	writef('%t\t%t\t%w\n',[A,B,C1]).
handle_time_output(A,B) :- !,
	writef('%w\t%w\n',[A,B]).
total_time(List,Acc):-
	total_time1(List,Times),
	sumlist(Times,0,Acc).


hand_out1(win,-) :- !.
hand_out1(new_flag(A,B),New) :- !,
	hand_out2(A,B,New).
hand_out1(failed(X),New) :- !,
	method_number_from_name(X,f,New),
	!.

hand_out1(X,X).	% Shouldnt happen

hand_out2(_,good(X),New) :- !,
	method_number_from_name(X,s,New).

hand_out2(Term,bad,New) :- !,
	method_number_from_name(Term,f,New).
hand_out2(A,B,cant_handle(A,B)) :- !.

method_number_from_name(Term,Type,Ans) :-
	recurse_to_atomic(Term,Arg),
	name(Arg,Name),
	append("auto",X,Name),
	name(Type,TName),
	TName = [Char],
	append(X,[40,Char,41],NewX),
	name(Ans,NewX),
	!.


recurse_to_atomic(X,X) :-
	atomic(X),
	!.
recurse_to_atomic(Term,Ans) :-
	arg(1,Term,Arg),
	!,
	recurse_to_atomic(Arg,Ans).

total_time1([],[]):- !.
total_time1([_-H|T],[H1|T1]):-
	(H = H1-_;H=H1),
	!,
	total_time1(T,T1).

count(Number) :-
	(bagof(Suc,Z^(runscores(Suc,Z)),SSet);SSet=[]),
	(bagof(Fail,Y^(runscores(Y,Fail)),FSet);FSet = []),
	!,
	sumlist(SSet,0,SSum),
	sumlist(FSet,0,FSum),
	extra_questions(Number,List),
	length(List,Untried),
	NewFail is FSum+Untried,
	!,
	Per is 100*SSum/(SSum+NewFail),
	writef('Succ: %t Fail: %t\n	%t \n',[SSum,NewFail,Per]),
	output_scores(List),
	clean_up.

clean_up :-
	(retract(successes(_));
	retract(failures(_));
	retract(runscores(_,_))),
	fail.
clean_up.

	


sumlist([],Tot,Tot):- !.
sumlist([H|T],Acc,Tot):-
	NewAcc is Acc + H,
	!,
	sumlist(T,NewAcc,Tot).




write_totals(STime,SL,FTime,FL):-
	do_division(succ,STime,SL),
	do_division(fail,FTime,FL),
	Average is (STime+FTime)/(SL+FL),
	writef('Av:   %t\n',[Average]).

do_division(Type,_,0):- !,
	writef('[No %w]\n',[Type]).
do_division(Type,Sum,Number):-
	Av is Sum/Number,
	writef('%w:   %t\n',[Type,Av]).


ac_t(1,File,SF):- !,
	gensym(t1scr,File),
	gensym('t1scr.sts',SF).
ac_t(Number,File,StatsFile):- !,
	name(Number,NName),
	append([116|NName],"scr",New),
	name(File1,New),
	gensym(File1,File),
	append(New,".sts",SFile),
	name(StatsFile1,SFile),
	gensym(StatsFile1,StatsFile),
	append("/usr/bs/lpdir/test/dump",NName,DumpFileName),
	name(DumpFile,DumpFileName),
	no_style_check(single_var),
	maybe_consult(DumpFile),
	style_check(all).

maybe_consult(_):-
	(known_method_schema(_,_,_,_,_,_,_,_,_);known_method_auto(_,_,_,_,_ ,_,_);user_rule(_,_,_,_,_,_,_)),
	!.
maybe_consult(File):- consult(File).




