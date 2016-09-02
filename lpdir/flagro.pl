%   File   : FLAGRO.PL
%   Author : Lawrence Byrd + R.A.O'Keefe.
%   Updated: 31 October 1983
%   Purpose: Flag (global variable) handling.
%   Needs  : no other files.

:- public
        flag/2,                 %  initialise a flag.
        flag/3.                 %  change a flag.

:- mode
        check_valid_flag_name(+),
        flag(+, +),
        flag(+, ?, ?).

:- dynamic flag/2.


/*  Flags are stored in the data base keyed under the Flag itself with
    the information packaged into a compound term as follows:

        Flag -->                '$flag'(Flag, CurrentValue)

    If you only access flags through these routines there will be at
    most one such record per flag.  The flag/2 predicate will clear
    out any records it may find.  The flag/3 predicate maintains the
    flags returning the previous value as Old and updating the flag
    to New.  The code actually checks to see if this updating really
    has to change the data base.  For compatibility with old code, if
    you call flag/3 on a flag which has no record, an old value of 0
    is assumed.  For compatibility with C-Prolog, flags may not be
    integers, but only atoms or compound terms.
*/

check_valid_flag_name(Flag) :-
        nonvar(Flag),
        functor(Flag, Atom, _),
        atom(Atom).
%   There should be a clause to print an error message here.


flag(Flag, InitialValue) :-
        check_valid_flag_name(Flag),
        ( recorded(Flag, '$flag'(Flag,_), Ref), erase(Ref), fail ; true ),
        recorda(Flag, '$flag'(Flag,InitialValue), _).


flag(Flag, OldValue, NewValue) :-
        check_valid_flag_name(Flag),
        (   recorded(Flag, '$flag'(Flag, Old), Ref)  ;  Old = 0   ),
        !,                              %   there should be only one record
        OldValue = Old,                 %   pattern match, may fail
        (   OldValue == NewValue        %   no change needed
        ;   (   var(Ref)  ;  erase(Ref)   ),
            recorda(Flag, '$flag'(Flag,NewValue), _)
        ),  !.
