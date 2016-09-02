%   File: Factor.       Authors: Leon+RAOK      Updated: 23 November 82

/*  Method: factorising equations F1*F2*...*Fk = 0

    factorise assumes that the right hand side of the equation is zero,
    and the left hand side is a product.  The lhs is split into factors,
    and factors independent of x which are (or may be assumed to be)
    non-zero are "divided through".
*/

% :- public
%         factorise/4.
% 
% :- mode
%         factorise(+, +, -, -),
%         separate_factors(+, +, -, -).

factorise(Lhs, X, Factors, Proof) :-
        decomp(Lhs, [*|Terms]),
        sort(Terms, UniqueTerms), !,    %  remove duplicates
        separate_factors(UniqueTerms, X, Factors, Proof).

%   separate factors sorts the factors into those which depend on X
%   and those which do not.

separate_factors([Term|Rest], X, Factors, [div(Term)|Proof]) :-
        freeof(X, Term),                %   Term is independent of X
        modcall(non_zero(Term)),        %   is or assumed to be non-zero
        trace_press('\nDividing through by %t', [Term], 1), !,
        separate_factors(Rest, X, Factors, Proof).
separate_factors([Term|Rest], X, [Term|Factors], Proof) :- !,
        separate_factors(Rest, X, Factors, Proof).
separate_factors([], _, [], []).
