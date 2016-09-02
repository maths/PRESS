%   File   :  /usr/bs/lpdir/bag.pl
%   Author : Richard O'Keefe
%   Updated: Tue Oct 15 11:46:41 1985
%   Purpose: Stuff from BAGUTL

list_to_bag(L, B) :-
        addkeys(L, K),
        keysort(K, S),
        bagform(S, B).


addkeys([], []).
addkeys([Head|Tail], [Head-1|Rest]) :-
                addkeys(Tail, Rest).

bagform([], bag) :- !.
bagform(List, bag(E,M,B)) :-
                bagform(E, List, Rest, 0, M), !,
                bagform(Rest, B).

bagform(Head, [Head-N|Tail], Rest, K, M) :-!,
                        L is K+N,
                        bagform(Head, Tail, Rest, L, M).
bagform(Head, Rest, Rest, M, M).


