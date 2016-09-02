%   File   : IMISCE.PL
%   Author : Lawrence Byrd, L. Hardman
%   Updated: 4 April 1984
%   Purpose: Miscellaneous routines (interpreted)

%   The contents of this module should be redistributed.  The only thing
%   which has to be interpreted rather than compiled is subgoal, and that
%   is incomplete and obsolete.  gcc needs once from INVOCA.PL.

%:- public
%        (\=)/2,
%        casserta/1,
%        cassertz/1,
%        clean/0,
%        continue/0.


continue.                       %  This is one of the actions for error/3


% \=(X, X) :- !, fail.

%\=(_X, _Y) :- !.


casserta(X) :-
        clause(X, true),
        !.
casserta(X) :-
        asserta(X).


cassertz(X) :-
        clause(X, true),
        !.
cassertz(X) :-
        assertz(X).


clean :-
        seeing(OldInput),
        see('prolog.log'),
        file_delete('prolog.log'),
        seen,
        see(OldInput).


diff(X, X) :- !,
        fail.
diff(_X, _Y).


gcc(Goal) :-
        once_press(Goal),
        asserta('$gcc'(Goal)),
        fail.
gcc(Goal) :-
        retract('$gcc'(Answer)),
        !,              %  This cut is needed for nested 
        Goal = Answer.


l(X) :- listing(X).                     % Make listing easier.


subgoal(exact, L) :-
        \+ \+ (numbervars(L, 0, _), subgoal_of(L) ).
