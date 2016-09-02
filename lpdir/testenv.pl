%   File TESTENV.PL
%   Author : Bernard Silver
%   Updated: Wed Nov 20 11:59:51 1985
%   Purpose: Find out if LP is running under EMACS
%   and/or Suntools, and adjust accordingly



test :-
	test_env_emacs,
	test_env_st,
	halting_message,
	standard_header.	

% Suntools ?
test_env_st :-
	unix(system('ps -c | grep -s suntools')),
	flag(st,_,yes),
	!.
test_env_st :-
	flag(st,_,no),
	!.

% Emacs
test_env_emacs :-
	unix(system('ps -c | grep -s emacs')),
	!,
	writef('
 [As you are running LP within Emacs, your screen will not be cleared
 at the start of a problem.]\n'),
	flag(emacs,_,yes),
	flag(clearscreen,_,no).

test_env_emacs :-
	writef('\n
 [Your screen will be cleared at the start of a problem. To change this type
 "disable clearscreen." to the Prolog prompt]\n'),
	flag(emacs,_,no),
	flag(clearscreen,_,yes),
	!.

halting_message :-
	text_for_halting_message(Text),
	writef('\n[Type "stop." to exit%w]\n',[Text]),
	!.

text_for_halting_message(', "close." to close the window.') :-
	ok_for_tricks,
	!.
text_for_halting_message('').


put_string([]).
put_string([H|T]) :-
	put(H+256),
	!,
	put_string(T).

put_out_header(X) :-
	ok_for_tricks,
	!,
	append("]l",X,Mid),
	append(Mid,"\",Message),
	put_string(Message),
	!.

put_out_header(_).


standard_header :-
	put_out_header("                LP Learning PRESS").

restore_header :-
	ok_for_tricks,
	unix(system('echo s_t\(\"$SHELL\"\). > $HOME/s.tmp')),
	open('~/s.tmp',read,Stream),
	read(Stream,s_t(ShellString)),
	close(Stream),
	unix(system('rm $HOME/s.tmp')),
	append("Shell Tool 2.0: ",ShellString,New),
	put_out_header(New).

restore_header.

ok_for_tricks :-
	flag(st,yes,yes),
	flag(emacs,no,no),
	!.


equation_string(Word,Eqn) :-
	ok_for_tricks,
	!,
	s_w(Word,NW-EqnName),
	atom_break(Eqn,EqnName-[]),
	put_out_header(NW).
equation_string(_,_).

% "Solving equation "

s_w(solving,
[83,111,108,118,105,110,103,32,101,113,117,97,116,105,111,110,32|L]-L) :- !.

%"Solved equation "

s_w(solved,[83,111,108,118,101,100,32,101,113,117,97,116,105,111,110,32|L]-L)
	:- !.

%"Failed to solve "

s_w(fails,
[70,97,105,108,101,100,32,116,111,32,115,111,108,118,101,32|L]-L) :- !.

%"Working on "

s_w(work,[87,111,114,107,105,110,103,32,111,110,32|L]-L) :- !.

%"Finished example "

s_w(done,
[70,105,110,105,115,104,101,100,32,101,120,97,109,112,108,101,32|L]-L) :-!.

%"Failed on "

s_w(failed,[70,97,105,108,101,100,32,111,110,32|L]-L) :- !.

% Close window into LP icon
close :-
	ok_for_tricks,
	!,
	icon_file(File),
	name(File,IName),
	put_string("]I"),
	put_string(IName),
	put_string("\"),
	put_string("]L\"),
	put_string("[2t").
close.

stop :-
	writef('\nExiting\n'),
	restore_header,
	halt.


