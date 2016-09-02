%   File   :  /usr/bs/lpdir/routin.pl
%   Author : Written by Lawrence & Richard
%   Updated: Tue Oct 15 11:48:00 1985
%   Purpose: Old UTIL compiled code

 /* EXPORT */

  :- public 
	    append/3,
	    apply/2,		
	    concat/3,
	    gensym/2,
	    last/2,
	    listtoset/2,
	    maplist/3,
	    member/2,
	    memberchk/2,
	    nmember/3,
	    occurs_in/2,
	    pairfrom/4,
	    perm/2,
	    perm2/4,
	    rev/2,
	    select/3,
	    some/2,
	    union/3.



  /* MODES */

:- mode append(?,?,?),
	apply(+,+),	
	concat(+,+,?),
	gensym(+,?),
	last(?,?),
	listtoset(?,?),
	maplist(+,?,?),
	member(?,?),
	memberchk(?,?),
	nmember(?,+,?),
	occurs_in(+, +),
	occurs_in(+, +, +),
	pairfrom(+,?,?,?),
	perm(?,?),
	perm2(?,?,?,?),
	rev(?,?),
	revconc(?,+,?),
	select(?,?,?),
	some(+,?),
	union(?,?,?).



  member(X,[X|_]).

  member(X,[_|TL]) :- member(X,TL).


  memberchk(X,[X|_]) :- !.

  memberchk(X,[_|TL]) :- memberchk(X,TL).



			% X is the N'th member of List
nmember(El,List,N) :-
	nmember(El,List,1,N).

nmember(X,[X|_],N,N).

nmember(X,[_|L],Acc,N) :-
	New is Acc+1,
	nmember(X,L,New,N).

  union([],Ys,Ys).

  union([X|Xs],Ys,Zs)
	:- member(X,Ys),
	   !,
	   union(Xs,Ys,Zs).

  union([X|Xs],Ys,[X|Zs]) :- union(Xs,Ys,Zs).

  append([],L,L).

  append([HD|TL],L,[HD|LL]) :- append(TL,L,LL).

  last(X,[X]) :- !.

  last(X,[_|TL]) :- last(X,TL).


  listtoset([],[]).

  listtoset([HD|TL],Ans)
	:- member(HD,TL),
	   !,
	   listtoset(TL,Ans).

  listtoset([HD|TL],[HD|Ans]) :- listtoset(TL,Ans).



  perm([],[]).

  perm(L,[X|Xs])
	:- select(X,L,R),
	   perm(R,Xs).


  perm2(X,Y,X,Y).

  perm2(X,Y,Y,X).


  rev(L1,L2) :- revconc(L1,[],L2).


  revconc([],L,L).

  revconc([X|L1],L2,L3) :- revconc(L1,[X|L2],L3).



  select(X,[X|TL],TL).

  select(X,[Y|TL1],[Y|TL2]) :- select(X,TL1,TL2).



			% Get a pair of elements from a list, also
			%  return the rest. Pairs are only returned
			%  once (not twice different ways round)

pairfrom([X|T],X,Y,R) :- select(Y,T,R).

pairfrom([H|S],X,Y,[H|T]) :- pairfrom(S,X,Y,T).

  concat(N1,N2,N3)
	:- name(N1,Ls1),
	   name(N2,Ls2),
	   append(Ls1,Ls2,Ls3),
	   name(N3,Ls3).



  gensym(Prefix,V)
	:- var(V),
	   atom(Prefix),
	   flag(gensym(Prefix),N,N),
	   N2 is N + 1,
	   flag(gensym(Prefix),_,N2),
	   concat(Prefix,N2,V),
	   !.

  apply(P,Eargs)
	:- ( atom(P),
	     NP =.. [P|Eargs]	;   P =.. [Pred|Oargs],
				     append(Oargs,Eargs,Nargs),
				     NP =.. [Pred|Nargs]
	   ),
	   !,
	   NP.


  maplist(_,[],[]) :- !.

  maplist(P,[X|Xs],[Y|Ys])
	:- !,
	   apply(P,[X,Y]),
	   maplist(P,Xs,Ys).


  some(_,[]) :- !, fail.

  some(P,[X|_]) :- apply(P,[X]).

  some(P,[_|Xs]) :- !, some(P,Xs).

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



