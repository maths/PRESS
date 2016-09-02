%	WORDS					Updated: 21-Apr-81.

% :- public wordsin/2, frequent_words/2.
% 
% :- mode
%     wordsin(+, -),
%     frequent_words(+, -),
% 	scan_term(+, ?, -),
% 	    insert_word(?, +, -),
% 	    scan_list(+, ?, -),
% 	tree_list(?, +, +, -),
% 	strip_num(+, -).

%   wordsin(Term, List)
% finds all the words (atom) which occur at least once in Term, and returns
% them in List.  Furthermore, the words are in descending order of frequency.
% E.g. wordsin(x*x+x*y+y^2+z^7, [x,y,z]).
% The order is supposed to be heuristic.

wordsin(Term, List) :-
	scan_term(Term, _Some, Tree),
	tree_list(Tree, 1, [], Pairs),
	keysort(Pairs, Inorder),
	strip_num(Inorder, List).

%   frequent_words(Term, List)
% finds all the words (atoms) which occur more than once in Term, and returns
% them in List.  Furthermore, the words are in descending order of frequency.
% E.g. frequent_words(x*x+x*y+y^2+z^7, [x,y]).

frequent_words(Term, List) :-
	scan_term(Term, _Some, Tree),
	tree_list(Tree, 2, [], Pairs),
	keysort(Pairs, Inorder),
	strip_num(Inorder, List).

	scan_term(Simp, Old_Tree, Old_Tree) :-
		var(Simp), !.
	scan_term(Simp, Old_Tree, Old_Tree) :-
	ok_number(Simp), !.	%  was integer(Simp)
	scan_term(Atom, Old_Tree, New_Tree) :-
		atom(Atom), !,
		insert_word(Old_Tree, Atom, New_Tree).
	scan_term(List, Old_Tree, New_Tree) :-
		List = [_|_], !,
		scan_list(List, Old_Tree, New_Tree).
	scan_term(Term, Old_Tree, New_Tree) :-
		Term =.. [_Functor|Args], !,
		scan_list(Args, Old_Tree, New_Tree).

		insert_word(t(C, W, L, R), W, t(D, W, L, R)) :- !,
			(   var(C), D = 1
			;   integer(C), D is C+1
			),  !.
		insert_word(t(C, X, L, R), W, t(C, X, M, R)) :-
			W @< X, !,
			insert_word(L, W, M).
		insert_word(t(C, X, L, R), W, t(C, X, L, S)) :-
			W @> X, !,
			insert_word(R, W, S).

		scan_list([Head|Tail], Old_Tree, New_Tree) :-
			scan_term(Head, Old_Tree, Mid_Tree), !,
			scan_list(Tail, Mid_Tree, New_Tree).
		scan_list([],          Old_Tree, Old_Tree).

	tree_list(Tree,		 _Thresh, Accum, Accum) :-
		var(Tree), !.
	tree_list(t(N, _X, L, R), Thresh, Accum, Answer) :-
		N < Thresh, 
		tree_list(L, Thresh, Accum, Sofar), !,
		tree_list(R, Thresh, Sofar, Answer).
	tree_list(t(C, W, L, R), Thresh, Accum, Answer) :-
		tree_list(L, Thresh, Accum, Sofar),
		Key is -C, !,
		tree_list(R, Thresh, [Key-W|Sofar], Answer).

	strip_num([_Key-Word|Rest], [Word|More]) :- !,
		strip_num(Rest, More).
	strip_num([],		   []).

