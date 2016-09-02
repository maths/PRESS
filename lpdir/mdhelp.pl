%   File   : MDHELP.PL
%   Author : Bernard Silver
%   Updated: 23 February 1984
%   Purpose: LP version of Helper.pl

:- no_style_check(single_var).
:- public give_help/1, give_help/2.

:- mode
    give_help(+),			%  list of topics in an area.
    give_help(+, +),			%  help on a specific topic.
	find_help(+, +),		%  find and type a topic of list
	    read_after_delimiter(+),	%  find "#" or return end_of_file
	    find_help(+, +, +),	    	%  check a list of topics
		among(+, +),		%  member on commas instead of dots
		type_until_delimiter(+). %  display body of a Note.



give_help(Area) :-
	write('The topics for which help is available are:'), nl,
	give_help(Area, Topic),
	write('Call ''lphelp Topic'' for help about a specific topic.'), nl.

give_help(Area, Topic) :-
	(   help_file(Area, HelpName, Delimiter),
		call(Delim is Delimiter)
	|   atom(Area),
		Delim is "#", HelpName = Area
	),
	seeing(Old),
	(   see(HelpName), !,
		find_help(Delim, Topic),
		seen,
		see(Old)
	|    write('** No help is available on '), writeq(Topic),
		write(' in '), writeq(Area), nl
	),  !.


	find_help(Delimiter, Topic) :-
		var(Topic), !,
		read_after_delimiter(Delimiter, Topics),
		(   Topics = end_of_file
		|   tab(4), write(Topics), nl, fail
		).
	find_help(Delimiter, Topic) :-
		read_after_delimiter(Delimiter, Topics),
		find_help(Topics, Topic, Delimiter).

		find_help(end_of_file, Topic, _) :- !,
			seeing(HelpFile),
			write('** No help is available on '), writeq(Topic),
			write(' in help file '), nl.
		find_help(Topics, Topic, Delimiter) :-
			among(Topic, Topics),
			type_until_delimiter(Delimiter).


		among(Topic, (Topics1,Topics2)) :- !,
			(   among(Topic, Topics1)
			|   among(Topic, Topics2)
			).
		among(Topic, (Topics1;Topics2)) :- !,
			(   among(Topic, Topics1)
			|   among(Topic, Topics2)
			).
		among(Topic, Topic).


		read_after_delimiter(Delimiter, Topics) :-
			repeat,
				get0(Character),
				(   Character = -1, !, Topics = end_of_file
				|   Character = Delimiter, read(Topics)
				).


		type_until_delimiter(Delimiter) :-
			get0(C),
			C =\= -1, C =\= Delimiter, 
			put(C), !,
			type_until_delimiter(Delimiter).
		type_until_delimiter(Delimiter).
	


:- public help/1.
:- mode help(+).

lphelp(Topic) :- give_help(lp,Topic).


help_file(lp,File,36) :-
	helper_file(File).


:- style_check(all).
