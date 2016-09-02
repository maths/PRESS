
%   File   : CHAR
%   Author : Bernard Silver
%   Updated: 15 March 1984
%   Purpose: Character Reading for LP

:- dynamic temp_file/1.
/*
:- public
	convert_chars/2,
	read_in_conditions/1,
	read_in_rule_conds/1,
	read_in_line/1,
	read_in_rule/1,
	read_in_unknown/1,
	read_name/1,
	syntax_check/1.

:- mode
	convert_chars(+,-),
	make_reply_from_chars(+,+,+,-),
	map_put(+),
	mod_know_conditions(+),
	read_conditions(_),
	read_unknown(-),
	read_in_conditions(-),
	read_in_line(-),
	read_in_line1(+,-),
	read_in_rule(-),
	read_in_rule_conds(-),
	read_in_rule_conds_cont(+,-),
	read_in_unknown(-),
	read_name(-),
	read_rule(-),
	read_rest(+,+,?),
	read_rest_cond(+,?),
	read_rest_init_check(+,-),
	read_rest_init_check1(+,+,-),
	read_rest_unk(+,?),
	read_rest_unk1(+,?),
	syntax_check(+),
	syntax_check(+,+,+).
*/
read_in_rule_conds(Reply) :-
	read_in_rule(Rule),
	read_in_rule_conds_cont(Rule,Reply),
	!.

 % List starts "rule(", remainder is Name)
read_in_rule_conds_cont([114,117,108,101,40|L],Reply) :-
	append(NameChars,")",L),
	name(Name,NameChars),
	((call(rule(Name,Unk,Rule,Cond)),Reply=[Unk,Rule,Cond]);
(writef('\nRule %t not found, please try again.\n\n',[Name]),!,fail)),
	!.

read_in_rule_conds_cont(Rule,Reply) :-
	read_in_unknown(Unk),
	read_in_conditions(Cond),
	!,
	make_reply_from_chars(Rule,Unk,Cond,Reply).

read_in_rule(Rule) :-
	repeat,
	writef('\nEnter Rule\n'),
	prompt(_,'Rule:'),
	read_rule(Rule),
	syntax_check(Rule),
	!.

read_in_unknown(Unk) :-
	repeat,
	writef('\nEnter Unknown\n'),
	call(rule_text2a),
	prompt(_,'Unknown:'),
	read_unknown(Unk),
	append("u(",Unk,Mid),
	append(Mid,")",New),
	syntax_check(New),
	!.

read_in_conditions(Cond) :-
	repeat,
	writef('\nEnter List of conditions\n'),
	call(rule_text2),
	prompt(_,'Conditions:'),
	read_conditions(Cond),
	syntax_check(Cond),
	mod_know_conditions(Cond),
	!.

read_rule(Rule) :-
	get0(C),
	read_rest_init_check(C,Rule),
	!.
read_conditions(Cond) :-
	get0(C),
	(C \= 10; C=10,writef('\n[No conditions]\n')),
	read_rest_cond(C,Cond),
	!.
read_in_line(Rule) :-
	get0(C),
	read_in_line1(C,Rule),
	!.

read_in_line1(10,end) :- !.


read_in_line1(C,Rule) :- 
	read_rest_init_check(C,Rule1),
	syntax_check(Rule1),
	convert_chars(Rule1,Rule),
	!.
read_name(Name) :-
	get0(C),
	read_rest_unk1(C,List),
	syntax_check(List),
	convert_chars(List,Name),
	!.

read_unknown(Unk) :- 
	get0(C),
	(C\=10;C=10,writef('\n[No distinguished unknown]\n')),
	read_rest_unk(C,Unk),
	!.

read_rest_init_check(C,Line) :-
	get0(C1),
	read_rest_init_check1(C,C1,Line).

read_rest_init_check1(97,10,_) :- !,	% a followed by <CR>
	writef('\n[Aborting]\n'),
	call(abort).
read_rest_init_check1(98,10,_) :- !,	% b followed by <CR>
	writef('\n[Entering break]\n'),
	call(break),
	writef('\n[Leaving break]\n'),
	fail.
read_rest_init_check1(C,C1,Ans) :-
	read_rest(C,C1,Ans).

read_rest(46,10,[]) :- !.  % Delimited by .<cr>
read_rest(46,10,[]) :- !.  % Delimited by .<space>
read_rest(C,C1,[C|L]) :-
	get0(C2),
	read_rest(C1,C2,L).


read_rest_unk(10,"_") :- !.
read_rest_unk(C,Rest) :- read_rest_unk1(C,Rest),!.

read_rest_cond(10,"[]") :- !.
read_rest_cond(C,Rest) :- read_rest_unk1(C,Rest),!.

read_rest_unk1(10,[]) :- !. % delimited by <cr> 
read_rest_unk1(46,[]) :- skip(10),!. % delimited  .
read_rest_unk1(C,[C|L1]) :-
	get0(C1),
	!,
	read_rest_unk1(C1,L1).

syntax_check(Input) :-
	seeing(OldSee),
	telling(OldTell),
	syntax_check(Input,OldSee,OldTell).

syntax_check([10],OldSee,OldTell) :- !,
	fileerrors,
	seen,
	see(OldSee),
	told,
	tell(OldTell).

syntax_check(Input,OldSee,OldTell) :-
	get_temp_file(File),
	tell(File),
	map_put(Input),
	put("."),
	nl,
	told,
	tell(OldTell),
	nofileerrors,
	see(File),
	read(Term),
	Term \= 'end_of_file',
	!,
	seen,
	fileerrors,
	file_delete(File),
	see(OldSee),
	!.

syntax_check(_,OldSee,OldTell) :-
	seen,
	see(OldSee),
	told,
	tell(OldTell),
	fileerrors,
	fail.

map_put([]) :- !.
map_put([H|T]) :-
	put(H),
	!,
	map_put(T).

make_reply_from_chars(Rule,Unk,Cond,Reply) :-
	get_temp_file(File),
	telling(OldTell),
	tell(File),
	put("["),
	map_put(Unk),
	put(","),
	map_put(Rule),
	put(","),
	map_put(Cond),
	put("]"),
	put("."),
	nl,
	told,
	tell(OldTell),
	seeing(OldSee),
	see(File),
	read(Reply),
	seen,
	file_delete(File),
	see(OldSee),
	!.

mod_know_conditions("[]") :- !.
mod_know_conditions(Cond) :-
	convert_chars(Cond,Cond1),
	know_conditions(Cond1),
	!.

convert_chars(List,Structure) :-
	seeing(OS),
	telling(OT),
	get_temp_file(File),
	tell(File),
	map_put(List),
	put("."),
	nl,
	told,
	tell(OT),
	see(File),
	read(Structure),
	seen,
	file_delete(File),
	see(OS),
	!.


get_temp_file(File) :-
	call(temp_file(File)),
	!.

get_temp_file('temp.tmp') :-
%	plsys(jobno(JN)),
%	name(JN,List),
%	append([83,67,82,65,58,48|List],"LP.TMP",NewList),
%	name(File,NewList),
	asserta(temp_file('~/temp.tmp')),
	!.

% Turn non-atomic things into lists of numbers
atom_break(A&B,L-L1) :- !,
	atom_break(A,L-[38|L2]), % "&" -> [38]
	atom_break(B,L2-L1).
atom_break(A#B,L-L1) :- !,
	atom_break(A,L-[35|L2]), % "#" -> [35]
	atom_break(B,L2-L1).
atom_break(A=B,L-L1) :- !,
	atom_break(A,L-[61|L2]),   % "=" -> [61]
	atom_break(B,L2-L1).
atom_break(A*B,L-L1) :- !,
	atom_break(A,L-[42|L2]),	% "*" -> [42]
	atom_break(B,L2-L1).
atom_break(A+B,L-L1) :- !,
	atom_break(A,L-[43|L2]),	% "+" -> [43]
	atom_break(B,L2-L1).
atom_break(A^B,L-L1) :- !,
	atom_break(A,L-[94|L2]), % "^" -> [94]
	atom_break(B,L2-L1).
atom_break(A/B,L-L1) :- !,
	atom_break(A,L-[47|L2]), % "/" -> [47]
	atom_break(B,L2-L1).
atom_break(A-B,L-L1) :-
	atom_break(A,L-[45|L2]),  % "-" -> [45]
	atom_break(B,L2-L1).
atom_break(number(A,B,C),Ans) :- !,
	number(number(A,B,C),10000,S,N,D),
	number_handle(S,N,D,Ans).
atom_break(log(A,B),[108,111,103,40|L]-L1) :- !, %"log(" -> [108,111,103,40]
	atom_break(A,L-[44|L2]), % "," -> [44]
	atom_break(B,L2-[41|L1]). % ")" -> [41]

atom_break(A,New-L) :- 
	atomic(A),
	!,
	name(A,Ans),
	append(Ans,L,New).

atom_break(Term,LName-L1) :-
	functor(Term,F,1),
	!,
	name(F,FName),
	append(FName,[40|L],LName),
	arg(1,Term,Arg),
	atom_break(Arg,L-[41|L1]).


		% Adapted from LONG.PL
number_handle(+, N, [1],Ans) :- !,         % +N/1 = a +ve integer
	break_putn(N,Ans).
number_handle(-, N, [1],[45|Ans]) :- !,         % -N/1 = a -ve integer
	break_putn(N,Ans).
number_handle(+, N,  D,[40|L]-L1) :- !,         % +N/D = a +ve rational
	break_putn(N,L-[47|L2]),
	break_putn(D,L2-[41|L1]).


number_handle(-, N,  D,[40,45|L]-L1 ) :- !,         % -N/D = a -ve rational
	break_putn(N,L-[47|L2]),
	break_putn(D,L2-[41|L1]).

break_putn([],[48|L]-L   ) :- !.
break_putn([D],New-L) :- !,
	name(D,Ans),
	append(Ans,L,New),
	!.
break_putn([D|T],L-L1) :- 
	break_putn(T,L-[D4,D3,D2,D1,D0|L1]),
	D4 is (D//10000)       +48,     % D4*10^4 +
	D3 is (D//1000) mod 10 +48,     % D3*10^3 +
	D2 is (D//100) mod 10  +48,     % D2*10^2 +
	D1 is (D//10) mod 10   +48,     % D1*10^1 +
	D0 is (D) mod 10       +48.     % D0*10^0 = D.

