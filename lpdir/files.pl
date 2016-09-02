% Files.PL
%   Author : Lawrence Byrd + Richard A. O'Keefe
%   Updated: 21 August 1984
%   Purpose: Routines for playing with files.

file_exists(File) :-
        atom(File),
        seeing(OldSee),
        (   nofileerrors, see(File), !, fileerrors, seen, see(OldSee)
        ;   fileerrors, fail
        ).



file_delete(File) :-
	name(File,FileChars),
	append("rm ",FileChars,New),
	name(Atom,New),
	unix(system(Atom)).

file_rename(Old,New) :-
	name(Old,Chars),
	name(New,NewChars),
	append("mv ",Chars,Mid),
	append(Mid,[" "|NewChars],List),
	!,
	name(Atom,List),
	unix(system(Atom)).
