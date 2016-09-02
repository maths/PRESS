%   File   : METUTL.PL
%   Author : R.A.O'Keefe
%   Updated: 15 September 1984
%   Purpose: meta-logical operations as described in my note

% :- public
%         compound_press/1,
%         copy/2,
%         ground_press/1,
%         occurs_check/2,
%         occurs_in/2,
%         simple/1,
%         subsumes/2,
%         subsumes_chk/2,
%         subterm/2,
%         unify/2,
%         variables_of/2,
%         variant/2,
%         var_member_chk/2.
%  
% :- mode
%         copy(+, ?),
%         ground_press(+),
%             ground_press(+, +),
%         occurs_check(+, ?),
%             occurs_check(+, +, ?),
%         occurs_in(+, +),
%             occurs_in(+, +, +),
%         subterm(+, ?),
%             subterm(+, +, ?),
%         subsumes(+, +),
%             subsumes(+, +, +),
%                 subsumes(+, +, +, +),
%         subsumes_chk(+, +),
%         unify(+, +),
%             unify(+, +, +),
%         var_member_chk(+, +),
%         variables_of(+, -),
%             variables_of(+, +, -),
%                 variables_of(+, +, +, -),
%         variant(+, +).

:- dynamic copy/1.


compound_press(Term) :-
        nonvar(Term),           %  not a variable
        functor(Term, _, Arity),
        Arity > 0.              %  not atomic
 

simple(Term) :-
        var(Term), !.                   %  is a variable
simple(Term) :-                         %  -or-
        functor(Term, Term, 0), !.      %  is atomic
simple(Term) :-                         %  rationals should be atomic
        ok_number(Term).                   %  but aren't so we need this hack.

ground_press(Term) :-
        nonvar(Term),
        functor(Term, _, N),
        ground_press(N, Term).
 
ground_press(0, _) :-
        !.
ground_press(N, Term) :-
        arg(N, Term, Arg),
        ground_press(Arg),
        M is N-1, !,
        ground_press(M, Term).
 

occurs_in(Var, Term) :-
        var(Term),
        !,
        Var == Term.
occurs_in(Var, Term) :-
        functor(Term, _, N),
        occurs_in(N, Var, Term).
 
occurs_in(N, Var, Term) :-
        arg(N, Term, Arg),
        occurs_in(Var, Arg),
        !.
occurs_in(N, Var, Term) :-
        N > 1,
        M is N-1,
        occurs_in(M, Var, Term).
 

subterm(Term, Term).
subterm(SubTerm, Term) :-
        nonvar(Term),
        functor(Term, _, N),
        subterm(N, SubTerm, Term).
 
subterm(N, SubTerm, Term) :-
        arg(N, Term, Arg),
        subterm(SubTerm, Arg).
subterm(N, SubTerm, Term) :-
        N > 1,
        M is N-1,
        subterm(M, SubTerm, Term).


copy(Old, New) :-
        asserta(copy(Old)),
        retract(copy(Mid)), !,
        New = Mid.

occurs_check(Term, Var) :-
        var(Term), !,
        Term \== Var.
occurs_check(Term, Var) :-
        functor(Term, _, Arity),
        occurs_check(Arity, Term, Var).

occurs_check(0, _, _) :- !.
occurs_check(N, Term, Var) :-
        arg(N, Term, Arg),
        occurs_check(Arg, Var),
        M is N-1, !,
        occurs_check(M, Term, Var).

unify(X, Y) :-
        var(X), var(Y),
        !,
        X = Y.          %  want unify(X,X)
unify(X, Y) :-
        var(X),
        !,
        occurs_check(Y, X),             %  X is not in Y
        X = Y.
unify(X, Y) :-
        var(Y),
        !,
        occurs_check(X, Y),             %  Y is not in X
        X = Y.
unify(X, Y) :-
        atomic(X),
        !,
        X = Y.
unify(X, Y) :-
        functor(X, F, N),
        functor(Y, F, N),
        unify(N, X, Y).
        
unify(0, _X, _Y) :- !.
unify(N, X, Y) :-
        arg(N, X, Xn),
        arg(N, Y, Yn),
        unify(Xn, Yn),
        M is N-1, !,
        unify(M, X, Y).


subsumes_chk(General, Specific) :-
        \+  (   numbervars(Specific, 0, _),
                \+ General = Specific
            ).

var_member_chk(Var, [Head|_]) :-
        Head == Var,
        !.
var_member_chk(Var, [_|Tail]) :-
        var_member_chk(Var, Tail).


variables_of(Term, Vars) :-
        variables_of(Term, [], Vars).

variables_of(Term, Sofar, Sofar) :-
        var(Term),
        var_member_chk(Term, Sofar),
        !.
variables_of(Term, Sofar, [Term|Sofar]) :-
        var(Term),
        !.
variables_of(Term, Sofar, Vars) :-
        functor(Term, _, N),
        variables_of(N, Term, Sofar, Vars).

variables_of(0, _, Vars, Vars) :- !.
variables_of(N, Term, Sofar, Vars) :-
        arg(N, Term, Arg),
        variables_of(Arg, Sofar, Mid),
        M is N-1, !,
        variables_of(M, Term, Mid, Vars).


subsumes(General, Specific) :-
        variables_of(Specific, Vars),
        subsumes(General, Specific, Vars).

subsumes(General, Specific, Vars) :-
        var(General),
        var_member_chk(General, Vars),
        !,
        General == Specific.
subsumes(General, Specific, _Vars) :-
        var(General),
        !,
        General = Specific.     %  binds
subsumes(General, Specific, Vars) :-
        nonvar(Specific),       %  mustn't bind it
        functor(General,  FunctionSymbol, Arity),
        functor(Specific, FunctionSymbol, Arity),
        subsumes(Arity, General, Specific, Vars).

subsumes(0, _, _, _) :- !.
subsumes(N, General, Specific, Vars) :-
        arg(N, General,  GenArg),
        arg(N, Specific, SpeArg),
        subsumes(GenArg, SpeArg, Vars),
        M is N-1, !,
        subsumes(M, General, Specific, Vars).


variant(A, B) :-
        subsumes_chk(A, B),
        subsumes_chk(B, A).

