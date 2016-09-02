%   File   :  /usr/bs/lpdir/func.pl
%   Author : Bernard Silver
%   Updated: Tue Oct 15 12:00:58 1985
%   Purpose: Check LP knows the functions in the problems

:- dynamic 
	new_functor/2,
	k_functor/2.
	


find_functions(List,X,Flag) :-
	find_functions1(List,X,Flag,_-[]),
	mod_abolish(new_functor,2),
	!.

find_functions1([],_,_,L-L).
find_functions1([H|T],X,F,L-L1)  :-
	function_parse(H,H1,X),
	check_ok_set(H1,X,F,L-L2),
	!,
	find_functions1(T,X,F,L2-L1).


function_parse(Eqn,Set,Unk) :- dl_parsef(Eqn,Set1-[],Unk),listtoset(Set1,Set).

dl_parsef(A#B,L-L1,Unk) :- !,dl_parsef(A,L-L2,Unk),dl_parsef(B,L2-L1,Unk).
dl_parsef(A=B,L-L1,Unk) :- !,dl_parsef(A,L-L2,Unk),dl_parsef(B,L2-L1,Unk).
dl_parsef(A+B,L-L1,Unk) :- !,dl_parsef(A,L-L2,Unk),dl_parsef(B,L2-L1,Unk).
dl_parsef(A*B,L-L1,Unk) :- !,dl_parsef(A,L-L2,Unk),dl_parsef(B,L2-L1,Unk).
dl_parsef(A^B,L-L1,Unk) :- !,dl_parsef(A,L-L2,Unk),dl_parsef(B,L2-L1,Unk).
dl_parsef(Unk,L-L,Unk) :- !.
dl_parsef(A,L-L,Unk) :- freeof(Unk,A),!.
dl_parsef(A,[A|L]-L,_) :- !.

check_ok_set([],_,_,L-L) :- !.
check_ok_set([H|T],X,F,L-L1) :-
	check_ok_term(H,X,F,L-L2),
	!,
	check_ok_set(T,X,F,L2-L1).

check_ok_term(Term,X,_,L-L) :- freeof(X,Term),!.
check_ok_term(Term,X,F,[Func|L]-L1) :-
	known_functor(Term,N,Func),
	!,
	check_ok_args(Term,N,X,F,L-L1).


check_ok_term(Term,X,check,[F|L]-L1) :-
	functor(Term,F,N),
	(new_functor(F,N);
	(writef('\n[**Warning, LP has no rules for functor %t/%t**]\n',[F,N]),
	asserta(new_functor(F,N)))),
	check_ok_args(Term,N,X,check,L-L1),
	!.

check_ok_term(Term,_,add,_) :-
	functor(Term,F,N),
	writef('\n[Adding functor %t/%t to list of known functors]\n',[F,N]),
	mod_assert(k_functor(F,N)),
	!.

known_functor(Term,N,F) :-
	functor(Term,F,N),
	k_functor(F,N),
	!.

k_functor(+,2).
k_functor(*,2).
k_functor(^,2).

k_functor(log,2).

k_functor(sin,1).
k_functor(cos,1).
k_functor(tan,1).
k_functor(cot,1).
k_functor(sec,1).
k_functor(cosec,1).

k_functor(arcsin,1).
k_functor(arccos,1).
k_functor(arctan,1).
k_functor(arccot,1).
k_functor(arcsec,1).
k_functor(arccosec,1).

/*
k_functor(sinh,1).
k_functor(cosh,1).
k_functor(tanh,1).
k_functor(coth,1).
k_functor(sech,1).
k_functor(cosech,1).


k_functor(arcsinh,1).
k_functor(arccosh,1).
k_functor(arctanh,1).
k_functor(arccoth,1).
k_functor(arcsech,1).
k_functor(arccosech,1).

*/

check_ok_args(Term,N,Unk,F,L) :-
	c_f_a(N,0,Unk,Term,F,L).

c_f_a(N,N,_,_,_,L-L) :- !.
c_f_a(N,M,Unk,Term,F,L-L1)  :-
	K is M + 1,
	arg(K,Term,Arg),
	c_ok_arg1(Unk,Arg,F,L-L2),
	!,
	c_f_a(N,K,Unk,Term,F,L2-L1).

c_ok_arg1(Unk,Arg,_,L-L) :- freeof(Unk,Arg),!.


c_ok_arg1(Unk,Arg,F,L) :-
	function_parse(Arg,S,Unk),
	check_ok_set(S,Unk,F,L),
	!.
