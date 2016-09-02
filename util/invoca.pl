%   Author : Lawrence
%   Updated: 20 July 1983
%   Purpose: Fancy control structures ("invocation" routines).


%   Most of these predicates are best forgotten.
%   The exceptions are &/2, forall/2, once_press/1, not/1.

	%%%  Run this module interpreted
	%%%  INVOCA requires no other modules


:- dynamic '$bind'/1.

%:- public			% /--- begin junk
%	(\\)/2,			% |    a\\b = once_press((a;b))
%	nobt/1,			% |    nobt(X) = once_press(X)
%	(thnot)/1,		% |    thnot(X) = not(X) = \+ X
%	any/1,			% |    any([G1,...,Gn]) = G1;....;Gn
%	binding/2,		% |    nobody knows
%	for/2,			% |    for(N,G) = G,...,G (with N Gs)
%	findall_press/3,		% \--- end junk
%	(&)/2,
%	forall/2,
%	once_press/1,
%	(not)/1.
                               
&(A, B) :-
	call(A),
	call(B).

/*		

\\(A, B) :-
	(call(A) ; call(B)), !.

*/

any([]) :- !,
	fail.			%   catch lists with unbound tails
any([Goal|_Goals]) :-
	call(Goal).
any([_|Goals]) :-
	any(Goals).



binding(N, Goal) :-
	asserta('$bind'(N)),
	N > 0,
	call(Goal),
	'$retr'(N2),
	N3 is N2-1,
	(   N3 =< 0
	;   asserta('$bind'(N3)), fail
	),  !.
binding(_, _) :-
	'$retr'(_),
	fail.


'$retr'(N) :-
	retract('$bind'(N)), !.



			% Findall Xs such that P.  This is a funny version
			%  which finds the FIRST solution that bagof would
			%  find, but returns the empty list if there are no
			%  solutions. (Logically dubious.)  A better version
			%  of this is available elsewhere.

findall_press(X, P, List) :-
	bagof(X, P, List), !.
findall_press(_X, _P, []).



for(0, _Goal) :- !.
for(N, Goal) :-
	N > 0,
	call(Goal),
	M is N-1,
	for(M, Goal),
	!.			%   this cuts Goal as well



forall(Generator, Test) :-
	Generator,
	\+ Test,
	!, fail.
forall(_, _).



once_press(Goal) :-
	call(Goal), !.


nobt(Goal) :-
	call(Goal), !.



not(Goal) :- call(Goal), !, fail.
not(_Goal).



thnot(Goal) :- call(Goal), !, fail.
thnot(_Goal).




