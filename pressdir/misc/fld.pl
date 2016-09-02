%   File   : FLD
%   Author : Richard O'Keefe
%   Updated: 20 July 1983
%   Purpose: Find a least dominating subterm

/*  We are given a kernel X and an expression Exp, and want to find
    a subterm Ldom of Exp which is a least dominating term for X,
    and the path to that term.
    This is done by finding the unique argument which contains all
    the occurrences of X, and if there is more than one such argument,
    returning the current term.
*/

%:- public
%	find_least_dom/4,	%   ker x expr -> expr x path
%	least_closeness/3.	%   ker x expr -> integer
%
%:- mode
%	find_least_dom(+, +, -, -),
%	least_closeness(+, +, -),
%	unique_argument(+, +, -),
%	unique_argument(+, +, +, +, -).


find_least_dom(X, Exp, Ldom, [ArgNo|Path]) :-
	unique_argument(X, Exp, ArgNo),
	arg(ArgNo, Exp, Arg),
	!,
	find_least_dom(X, Arg, Ldom, Path).
find_least_dom(_X, Exp, Exp, []).


unique_argument(X, Exp, ArgNo) :-
	functor(Exp, _, N),
	unique_argument(N, X, Exp, 0, ArgNo).

unique_argument(0, _, _, ArgNo, ArgNo) :- !.
unique_argument(N, X, Exp, SoFar, ArgNo) :-
	arg(N, Exp, Arg),
	contains(X, Arg),
	!,
	SoFar = 0,
	M is N-1,
	unique_argument(M, X, Exp, N, ArgNo).
unique_argument(N, X, Exp, SoFar, ArgNo) :-
	M is N-1,
	unique_argument(M, X, Exp, SoFar, ArgNo).


/*  least_closeness(X, Exp, Dist)
    finds the closeness of the occurrences of the kernel X in the expression
    Exp, which is not guaranteed to be a least dominating term for X.  The
    older predicate 'closeness' is only correct when Exp >is< a least
    dominating term, so this code finds the correct subterm first.  Some of
    the uses of 'closeness' in press:attrac should be changed to use this.
*/
least_closeness(X, Exp, Dist) :-
	find_least_dom(X, Exp, Ldom, _),
	closeness(X, Ldom, Dist).



