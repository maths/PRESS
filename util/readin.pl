%   File   : READIN.PL
%   Author : Lawrence Byrd
%   Updated: 15 November 1983
%   Purpose: Read in a sentence as a list of words.

% :- public
%         read_in/1.
% 
% :- mode
%         read_in(?),
%         ri_rest(+, -),
%         ri_word(-, ?, ?),
%         ri_words(-, ?, ?),
%         ri_letdig(-, ?, ?),
%         ri_space(?, ?),
%         ri_digits(-, ?, ?),
%         ri_digit(+),
%         ri_lcase(+, -).



%   read_in(Words)
%   reads characters until it finds a period (. ? or !) at the end of a
%   line, that is, followed by any number of spaces but nothing else.
%   It then parses this list of characters using grammar rules.  Words
%   are sequences of letters (in either case, forced to lower case),
%   or strings of digits, or punctuation marks.  Other characters act
%   as word separators, and are otherwise ignored.  The subroutines in
%   this file all start with ri_, to avoid a collision with a user's
%   predicates.


read_in(Words) :-
        get(C1),
        get0(C2),
        ri_rest(C2, Chars), !,
        ri_words(Found, [C1,C2|Chars], []), !,
        Words = Found.

ri_rest(C, Chars) :-                            %  check for period NL
        (C=33 ; C=46 ; C=63),                   %  ! . ?
        repeat, get0(C1), C1 =\= 32, !,         %  next non-blank
        (   C1 = 10, !, Chars = []              %  was end of line
        ;   Chars = [C1|Chars1], ri_rest(C1, Chars1)
        ).                                      %  false alarm
ri_rest(C, [C1|Chars]) :-                       %  all control chars
        C =< 32, !,                             %  act as spaces.
        get(C1),                                %  skip other spaces
        ri_rest(C1, Chars).
ri_rest(_C, [C1|Chars]) :-
        get0(C1),
        ri_rest(C1, Chars).


ri_words([Word|Words]) -->
        ri_word(Word), !,
        ri_space, ri_words(Words).
ri_words([]) --> [].

ri_word(Word) -->
        [C], {ri_letter(C, D)}, !,              %  either case letter
        ri_letdig(Chars),                       %  parse rest of word
        {name(Word, [D|Chars])}.
ri_word(Word) -->
        [C], {ri_digit(C)}, !,                  %  digit
        ri_digits(Chars),                       %  parse rest of number
        {name(Word, [C|Chars])}.
ri_word(Word) -->
        [C],                                    %  can't a space, so it
        {name(Word, [C])}.                      %  is punctuation

ri_letdig([D|Chars]) -->
        [C], {ri_letter(C, D)}, !,
        ri_letdig(Chars).
ri_letdig([C|Chars]) -->
        [C], {ri_digit(C)}, !,
        ri_letdig(Chars).
ri_letdig([]) --> [].

ri_digits([C|Chars]) -->
        [C], {ri_digit(C)}, !,
        ri_digits(Chars).
ri_digits([]) --> [].

ri_space -->
        [C], {C=<32}, !,
        ri_space.
ri_space --> [].

ri_digit(C) :-
        C >= 48, C =< 57.       %  0..9

ri_letter(C, C) :-
        C >= 97, C =< 122, !.   %  a..z
ri_letter(C, D) :-
        C >= 65, C =< 90,       %  A..Z
        D is C+32.              %  32 is "a"-"A"

