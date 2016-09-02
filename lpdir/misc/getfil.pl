%   File   : GETFIL.PL
%   Author : Richard A. O'Keefe
%   Updated: 1 June 84
%   Purpose: read a file name from the terminal

:- public
        getfile/1,
        read_a_line/2.

/*  This is for use in tools for analysing Prolog (or other) files.
    getfile(File) prompts Next file: and reads a file name without
    any extraneous punctuation.  The file name is returned as an
    atom.  System-dependent processing should be done here.  The
    current Dec-10 version does that wrongly.  If you have a "search
    list", e.g. something like the $PATH environment variable for
    UNIX programs, this is a good place to put it in.

    read_a_line is similar to the read_line predicate in read_sent,
    but (a) it prompts, and (b) most of the tools that load this
    file don't wany anything else from read_sent.  But if your
    system wants do do case conversion on file names, you'll find
    the predicate you want there.  Oh well.
*/

:- mode
        getfile(-),
        read_a_line(+, -),
        rest_of_a_line(+, -).


getfile(File) :-
        read_a_line('Next file: ', Chars),
        name(File, Chars).


read_a_line(Prompt, Chars) :-
        write(Prompt), ttyflush,
        get0(Char),
        rest_of_a_line(Char, Chars).
                        

rest_of_a_line(10, []) :- !.    %  <NL>   = <CR><LF>
rest_of_a_line(27, []) :- !.    %  <ESC>
rest_of_a_line(Ch, [Ch|Rest]) :-
        get0(C2),
        rest_of_a_line(C2, Rest).


