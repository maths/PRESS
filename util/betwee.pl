%   File   : BETWEEN.PL
%   Author : R.A.O'Keefe
%   Updated: 4 October 1984
%   Purpose: Generate integers.

% :- public
%         between/3,
%         gen_arg/3,
%         gen_int/1,
%         gen_nat/1,
%         repeat/1.
% 
% :- mode
%         between(+, +, ?),
%         between1(+, +, -),
%         gen_arg(?, +, ?),
%         gen_int(?),
%         gen_nat(?),
%         gen_nat(+, -),
%         repeat(+).


between(L, U, N) :-
        nonvar(N),
        !,
        integer(L), integer(U), integer(N),
        L =< N, N =< U.
between(L, U, N) :-
        integer(L), integer(U), L =< U,
        between1(L, U, N).


between1(L, _, L).
between1(L, U, N) :-
        L < U,
        M is L+1,
        between1(M, U, N).



%   gen_arg(N, Term, Arg)
%   is exactly like arg(N, Term, Arg), except that it will generate
%   solutions for N by backtracking (will work when N is a variable).

gen_arg(N, Term, Arg) :-
        functor(Term, _, Arity),
        between(1, Arity, N),
        arg(N, Term, Arg).



gen_nat(N) :-                   % gen-erate nat-ural
        nonvar(N),              % if we aren't to generate it
        !,                      % demand that it is an integer
        integer(N), N >= 0.     % and non-negative.
gen_nat(N) :-                   % otherwise, generate an
        gen_nat(0, N).          % integer >= 0
 
 
gen_nat(L, L).
gen_nat(L, N) :-                % generate natural > L
        M is L+1,
        gen_nat(M, N).          % generate natural >= M
 
 

gen_int(I) :-                   % gen-erate int-eger
        nonvar(I),              % if we aren't to generate it
        !,                      % demand that it is an integer.
        integer(I).
gen_int(0).                     % generate 0
gen_int(I) :-                   % generate +/- N for N > 0
        gen_nat(1, N),
        (   I = N
        ;   I is -N
        ).



repeat(N) :-
        telling(Old), tell(user),
        write('It is pointlessly stupid to use repeat/1.'), nl,
        write('Why dont you use between/3 instead, fathead?'), nl,
        tell(Old),
        between(1, N, _).

