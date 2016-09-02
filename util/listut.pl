%   File   : LISTUT.PL
%   Author : Bob Welham, Lawrence Byrd, and R.A.O'Keefe
%   Updated: 1 October 1984
%   Purpose: list processing utilities

%   This module requires
%       select/3        (from SetUtl.Pl) for perm/2
%       listtoset/2     (from SetUtl.Pl) for remove_dups/2
%   If you don't want those routines, it can be used on its own.
%   I am not sure how much of the original code was by Bob Welham
%   and how much by Lawrence Byrd.  The layout and comments are by
%   R.A.O'Keefe, as are nth*, same_length, shorter_list, and subseq*.
%   Keys_and_values has moved to PROJEC.PL.

% :- public
%         append/3,                       %   List x List -> List
%         correspond/4,                   %   Elem <- List x List -> Elem
%         delete/3,                       %   List x Elem -> List
%         last/2,                         %   List -> Elem
%         nextto/3,                       %   Elem, Elem <- List
%         nmember/3,                      %   Elem <- Set -> Integer
%         nth0/3,                         %   Integer x List -> Elem
%         nth0/4,                         %   Integer x List -> Elem x List
%         nth1/3,                         %   Integer x List -> Elem
%         nth1/4,                         %   Integer x List -> Elem x List
%         numlist/3,                      %   Integer x Integer -> List
%         perm/2,                         %   List -> List
%         perm2/4,                        %   Elem x Elem -> Elem x Elem
%         remove_dups/2,                  %   List -> Set
%         rev/2,                          %   List -> List
%         reverse/2,                      %   List -> List
%         same_length/2,                  %   List x List ->
%         select/4,                       %   Elem x List x Elem -> List
%         shorter_list/2,                 %   List x List ->
%         subseq/3,                       %   List -> List x List
%         subseq0/2,                      %   List -> List
%         subseq1/2,                      %   List -> List
%         sumlist/2.                      %   List -> Integer
% 
% :- mode
%         append(?, ?, ?),
%         correspond(?, +, +, ?),
%         delete(+, +, -),
%         last(?, ?),
%         nextto(?, ?, ?),
%         nmember(?, +, ?),
%         nth0(+, +, ?),
%         nth0(+, ?, ?, ?),
%         nth1(+, +, ?),
%         nth1(+, ?, ?, ?),
%         numlist(+, +, ?),
%         perm(?, ?),
%         perm2(?,?, ?,?),
%         remove_dups(+, ?),
%         rev(?, ?),
%         reverse(?, ?),
%         reverse(?, +, ?),
%         same_length(?, ?),
%         select(?, ?, ?, ?),
%         shorter_list(?, +),
%         subseq(?, ?, ?),
%         subseq0(+, ?),
%         subseq1(+, ?),
%         sumlist(+, ?),
%         sumlist(+, +, ?).


%   append(Prefix, Suffix, Combined)
%   is true when all three arguments are lists, and the members of Combined|yy
%   are the members of Prefix followed by the members of Suffix.  It may be
%   used to form Combined from a given Prefix and Suffix, or to take a given
%   Combined apart.  E.g. we could define member/2 (from SetUtl.Pl) as
%       member(X, L) :- append(_, [X|_], L).

append([], L, L).
append([H|T], L, [H|R]) :-
        append(T, L, R).



%   correspond(X, Xlist, Ylist, Y)
%   is true when Xlist and Ylist are lists, X is an element of Xlist, Y is
%   an element of Ylist, and X and Y are in similar places in their lists.

correspond(X, [X|_], [Y|_], Y) :- !.
correspond(X, [_|T], [_|U], Y) :-
        correspond(X, T, U, Y).



%   delete(List, Elem, Residue)
%   is true when List is a list, in which Elem may or may not occur, and
%   Residue is a copy of List with all elements equal to Elem deleted.

delete([], _, []) :- !.
delete([Kill|Tail], Kill, Rest) :- !,
        delete(Tail, Kill, Rest).
delete([Head|Tail], Kill, [Head|Rest]) :- !,
        delete(Tail, Kill, Rest).



%   last(Last, List)
%   is true when List is a List and Last is its last element.  This could
%   be defined as last(X,L) :- append(_, [X], L).

last(Last, [Last]) :- !.
last(Last, [_|List]) :-
        last(Last, List).



%   nextto(X, Y, List)
%   is true when X and Y appear side-by-side in List.  It could be written as
%       nextto(X, Y, List) :- append(_, [X,Y], List).
%   It may be used to enumerate successive pairs from the list.

nextto(X,Y, [X,Y|_]).
nextto(X,Y, [_|List]) :-
        nextto(X,Y, List).



%   nmember(Elem, List, Index)
%   is true when Elem is the Indexth member of List.  Could be written as
%       nmember(X, L, N) :- append(B, [X|_], L), length(B, M), N is M+1.
%   It may be used to select a particular element, or to find where some
%   given element occurs, or toy enumerate the elements and indices together.

nmember(Elem, [Elem|_], 1).
nmember(Elem, [_|List], N) :-
        nmember(Elem, List, M),
        N is M+1.



%   nth0(N, List, Elem) is true when Elem is the Nth member of List,
%   counting the first as element 0.  (That is, throw away the first
%   N elements and unify Elem with the next.)  It can only be used to
%   select a particular element given the list and index.  For that
%   task it is more efficient than nmember.
%   nth1(N, List, Elem) is the same as nth0, except that it counts from
%   1, that is nth(1, [H|_], H).

nth0(0, [Head|_Tail], Head) :- !.
nth0(N, [_Head|Tail], Elem) :-
        M is N-1,                       % should be succ(M, N)
        nth0(M, Tail, Elem).


nth1(1, [Head|_Tail], Head) :- !.
nth1(N, [_Head|Tail], Elem) :-
        M is N-1,                       % should be succ(M, N)
        nth1(M, Tail, Elem).



%   nth0(N, List, Elem, Rest) unifies Elem with the Nth element of List,
%   counting from 0, and Rest with the other elements.  It can be used
%   to select the Nth element of List (yielding Elem and Rest), or to 
%   insert Elem before the Nth (counting from 1) element of Rest, when
%   it yields List, e.g. nth0(2, List, c, [a,b,d,e]) unifies List with
%   [a,b,c,d,e].  nth1 is the same except that it counts from 1.  nth1
%   can be used to insert Elem after the Nth element of Rest.

nth0(0, [Head|Tail], Head, Tail) :- !.
nth0(N, [Head|Tail], Elem, [Head|Rest]) :-
        M is N-1,               % succ(M, N); should fail if N < 1
        nth0(M, Tail, Elem, Rest).


nth1(1, [Head|Tail], Head, Tail) :- !.
nth1(N, [Head|Tail], Elem, [Head|Rest]) :-
        M is N-1,               % succ(M, N); should fail if N < 1
        nth1(M, Tail, Elem, Rest).



%   numlist(Lower, Upper, List)
%   is true when List is [Lower, ..., Upper]
%   Note that Lower and Upper must be integers, not expressions, and
%   that if Upper < Lower numlist will FAIL rather than producing an
%   empty list.

numlist(Upper, Upper, [Upper]) :- !.
numlist(Lower, Upper, [Lower|Rest]) :-
        Lower < Upper,
        Next is Lower+1,
        numlist(Next, Upper, Rest).



%   perm(List, Perm)
%   is true when List and Perm are permutations of each other.  Of course,
%   if you just want to test that, the best way is to keysort/2 the two
%   lists and see if the results are the same.  Or you could use list_to_bag
%   (from BagUtl.Pl) to see if they convert to the same bag.  The point of
%   perm is to generate permutations.  The arguments may be either way round,
%   the only effect will be the order in which the permutations are tried.
%   Be careful: this is quite efficient, but the number of permutations of an
%   N-element list is N!, even for a 7-element list that is 5040.

perm([], []).
perm(List, [First|Perm]) :-
        select(First, List, Rest),      %  tries each List element in turn
        perm(Rest, Perm).



%   perm2(A,B, C,D)
%   is true when {A,B} = {C,D}.  It is very useful for writing pattern
%   matchers over commutative operators.  It is used more than perm is.

perm2(X,Y, X,Y).
perm2(X,Y, Y,X).



%   remove_dups(List, Pruned)
%   removes duplicated elements from List.  Beware: if the List has
%   non-ground elements, the result may surprise you.

remove_dups(List, Pruned) :-
        sort(List, Pruned).



%   reverse(List, Reversed)
%   is true when List and Reversed are lists with the same elements
%   but in opposite orders.  rev/2 is a synonym for reverse/2.

rev(List, Reversed) :-
        reverse(List, [], Reversed).

reverse(List, Reversed) :-
        reverse(List, [], Reversed).

reverse([], Reversed, Reversed).
reverse([Head|Tail], Sofar, Reversed) :-
        reverse(Tail, [Head|Sofar], Reversed).



%   same_length(List1, List2)
%   is true when List1 and List2 are both lists and have the same number
%   of elements.  No relation between the values of their elements is
%   implied.  It may be used to generate either list given the other,
%   or indeed to generate two lists of the same length, in which case
%   the arguments will be bound to lists of length 0, 1, 2, ... 

same_length([], []).
same_length([_|List1], [_|List2]) :-
        same_length(List1, List2).



%   select(X, Xlist, Y, Ylist)
%   is true when X is the Kth member of Xlist and Y the Kth element of Ylist
%   for some K, and apart from that Xlist and Ylist are the same.  You can
%   use it to replace X by Y or vice versa.

select(X, [X|Tail], Y, [Y|Tail]).
select(X, [Head|Xlist], Y, [Head|Ylist]) :-
        select(X, Xlist, Y, Ylist).



%   shorter_list(Short, Long)
%   is true when Short is a list is strictly shorter than Long.  Long
%   doesn't have to be a proper list provided it is long enough.  This
%   can be used to generate lists shorter than Long, lengths 0, 1, 2...
%   will be tried, but backtracking will terminate with a list that is
%   one element shorter than Long.  It cannot be used to generate lists
%   longer than Short, because it doesn't look at all the elements of the
%   longer list.

shorter_list([], [_|_]).
shorter_list([_|Short], [_|Long]) :-
        shorter_list(Short, Long).
        


%   subseq(Sequence, SubSequence, Complement)
%   is true when SubSequence and Complement are both subsequences of the
%   list Sequence (the order of corresponding elements being preserved)
%   and every element of Sequence which is not in SubSequence is in the
%   Complement and vice versa.  That is,
%   length(Sequence) = length(SubSequence)+length(Complement), e.g.
%   subseq([1,2,3,4], [1,3,4], [2]).  This was written to generate subsets
%   and their complements together, but can also be used to interleave two
%   lists in all possible ways.  Note that if S1 is a subset of S2, it will
%   be generated *before S2 as a SubSequence and *after it as a Complement.

subseq([], [], []).
subseq([Head|Tail], Sbsq, [Head|Cmpl]) :-
        subseq(Tail, Sbsq, Cmpl).
subseq([Head|Tail], [Head|Sbsq], Cmpl) :-
        subseq(Tail, Sbsq, Cmpl).



%   subseq0(Sequence, SubSequence)
%   is true when SubSequence is a subsequence of Sequence, but may
%   be Sequence itself.   Thus subseq0([a,b], [a,b]) is true as well
%   as subseq0([a,b], [a]).

%   subseq1(Sequence, SubSequence)
%   is true when SubSequence is a proper subsequence of Sequence,
%   that is it contains at least one element less.

%   ?- setof(X, subseq0([a,b,c],X), Xs).
%   Xs = [[],[a],[a,b],[a,b,c],[a,c],[b],[b,c],[c]] 
%   ?- bagof(X, subseq0([a,b,c,d],X), Xs).
%   Xs = [[a,b,c,d],[b,c,d],[c,d],[d],[],[c],[b,d],[b],[b,c],[a,c,d],
%         [a,d],[a],[a,c],[a,b,d],[a,b],[a,b,c]] 

subseq0(List, List).
subseq0(List, Rest) :-
        subseq1(List, Rest).


subseq1([_Head|Tail], Rest) :-
        subseq0(Tail, Rest).
subseq1([Head|Tail], [Head|Rest]) :-
        subseq1(Tail, Rest).



%   sumlist(Numbers, Total)
%   is true when Numbers is a list of integers, and Total is their sum.
%   Note that in Dec-10 compiled Prolog this will only work as stated;
%   interpreters will almost certainly accept integer expressions.  Also
%   note here as elsewhere in Prolog arithmetic that machine arithmetic
%   wraps round on the Dec-10: (2^17 - 1)+1 = -2^17 .

sumlist(Numbers, Total) :-
        sumlist(Numbers, 0, Total).

sumlist([], Total, Total).
sumlist([Head|Tail], Sofar, Total) :-
        Next is Sofar+Head,
        sumlist(Tail, Next, Total).


