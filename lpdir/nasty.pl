%   File   :  /usr/bs/lpdir/nasty.pl
%   Author : Bernard Silver
%   Updated: Tue Oct 15 11:42:27 1985
%   Purpose: Nasty Function code for LP

nasty_method(Eqn,X,Ans) :- 
	tidy(Eqn,Eqn1),
	try_nasty_method(Eqn1,X,Ans),
	!.

try_nasty_method(Eqn,X,Neweqn) :- 
	parse4(Eqn,X,U,other),
	subnasty(X,U,V),
	find_symbols(Eqn,V,Symbols,Posns),
	nasty_act(Symbols,Posns,Eqn,X,Neweq),
	tidy(Neweq,Neweqn),
	!.

try_nasty_method(Eqn,X,Neweqn) :- 
	parse4(Eqn,X,U,neg),
	exp_nasty_list(X,U,V),
	remove_subsumed(V,Termlist),
	multiply_through(Eqn,Termlist,Neweqn,X),
	tidy(Neweqn,New),
	!.

nasty_act(Symbols,[Posn|_],Eqn,X,New) :- 
	nice(Symbols),
	append(Posn,[1],Posn1),
	position(Term,Eqn,Posn1),
	try_isolate(Posn1,Eqn,New),
	!.


try_isolate(Posn,Eqn,New) :- isolate(Posn,Eqn,New),!.
try_isolate(_,_,_) :- !,fail.

nasty_act(Symbols,Posns,Eqn,X,New) :- 
	find_attract_list(Symbols,N,L,Type),
	nmember(Posn,Posns,N),
	strip(Posn,L,Newp),
	position(Term,Eqn,Newp),
	nas_rule(Term,Nterm,Type),
	subst(Term=Nterm,Eqn,New1),
	tidy(New1,New),
	!.

parse4(A,Unk,Bag,Type) :- dl_parse4(A,Unk,Bag-[],Type).

dl_parse4(A,Unk,L-L,_) :- freeof(Unk,A),!.
dl_parse4(A=B,Unk,L-L1,T) :- !,
	dl_parse4(A,Unk,L-L2,T),
	dl_parse4(B,Unk,L2-L1,T).

dl_parse4(A*B,Unk,L-L1,T) :- !,
	dl_parse4(A,Unk,L-L2,T),
	dl_parse4(B,Unk,L2-L1,T).

dl_parse4(A+B,Unk,L-L1,T) :- !,
	dl_parse4(A,Unk,L-L2,T),
	dl_parse4(B,Unk,L2-L1,T).

dl_parse4(A^B,Unk,X,other) :- integer(B),B > 0,!,dl_parse4(A,Unk,X,other).
dl_parse4(A,_,[A|L]-L,_) :- !.

nasty(X,Y) :- root_nasty(X,Y),!.
nasty(X,Y) :- exp_nasty(X,Y),!.

root_nasty(X,U^N) :- contains(X,U),ok_number(N),not integer(N),eval(N>0),!.

exp_nasty(X,U^N) :- contains(X,U),ok_number(N),eval(N<0),diff(X,U),!.

exp_nasty_list(_,[],[]) :- !.
exp_nasty_list(X,[H|Rest],[H|RestV]) :-
	exp_nasty(X,H),
	!,
	exp_nasty_list(X,Rest,RestV).

exp_nasty_list(X,[_|Rest],RestV) :-
	exp_nasty_list(X,Rest,RestV).


find_symbols(_,[],[],[]) :- !.
find_symbols(E,[H|T],[H1|T1],[H2|T2]) :- 
	find_symbols1(E,H,H1,H2),
	find_symbols(E,T,T1,T2),
	!.

find_symbols1(Eqn,X,Y,B) :- 
	posl(X,Eqn,A,B),
	expon(X,P),
	append(A,[P],Y),
	!.	

posl(X,X,[],[]) :- !.
posl(X,E,[Op|L],[N|Pos]) :- 
	E=..[Op1,Arg|Args],
	get_ops(Op,Op1,E),
	nmember(T,[Arg|Args],N),
	posl(X,T,L,Pos),
	!.

get_ops(exp(Arg1),_,E) :- E=..[^,_,Arg1|_],!.
get_ops(Op1,Op1,_) :- !.

expon(U^N,exp(N)) :- ok_number(N),!.

remove_subsumed([],_) :- !, fail.
remove_subsumed(V,Termlist) :-
	listtoset(V,List),		% Cheap test
	rem_sub(List,Termlist,[]).

rem_sub([],Termlist,Termlist) :- !.
rem_sub([H|Rest],Termlist,Acc) :-
	member_match(H,Acc,NewAcc),
	!,
	rem_sub(Rest,Termlist,NewAcc).
rem_sub([H|Rest],Termlist,Acc) :-
	match(H,U^N),
	ok_number(N),
	rem_sub(Rest,Termlist,[U^N|Acc]).

member_match(H,[],_) :- !, fail.
member_match(H,[U^N|Rest],[U^K|Rest]) :-
	match(H,U^M),
	!,
	least(N,M,K).
member_match(H,[Term|Rest],[Term|NewRest]) :-
	member_match(H,Rest,NewRest).

least(N,M,N) :- eval(N=<M), !.
least(N,M,M).

nice([]) :- !.
nice([List|Rest]) :-
	nice_list(List),
	!,
	nice(Rest).

nice_list([]) :- !.
nice_list([Fun|Rest]) :-
	good_fun(Fun),
	!,
	nice_list(Rest).

good_fun(+) :- !.
good_fun(=) :- !.
good_fun(*) :- !.
good_fun(exp(N)) :- ok_number(N),not integer(N),eval(numer(N)=1),!.

find_attract_list([],_,_,_) :- !,fail.
find_attract_list([H|T],1,M,Type) :- attract_list(H,M,Type),!.
find_attract_list([_|T],N,M,Type) :- 
	find_attract_list(T,N1,M,Type),
	N is N1+1,
	!.

attract_list([exp(N)|T],K,Type) :- 
	integer(N),
	last(exp(M),T),
	get_nasty_type(M,N,Type),
	append(T1,[exp(M)],T),
	checkpt(T1),
	length(T,K),
	!.


attract_list([_|T],M,Type) :- attract_list(T,M,Type),!.


get_nasty_type(M,N,root(M)) :- eval(1/N,M),!.
get_nasty_type(M,N,negroot(M)) :- eval(1/N,-1*M),!.
get_nasty_type(M,N,neg(M)) :- eval(M<0),!.

pt(*) :- !.
pt(+) :- !.

nas_rule(A^2 ,Exp,root(N)) :- dist(A,A1),tidy(A1,A2),expon_exp(A2^2,N,Exp),!.
nas_rule(A^2,Exp,negroot(N)) :- 
	dist(A,A1),
	tidy(A1,A2),
	expon_inv_exp(A2^2,N,Exp),
	!.

nas_rule(A^2,Exp,neg(N)) :- neg_exp(A^2,N,Exp),!.

expon_exp(Old,N,New) :- eval(N=(1/2)),expon_exp1(Old,N,New),!.

expon_inv_exp(Old,N,New) :- eval(N=(-1/2)),expon_inv_exp1(Old,N,New),!.

expon_exp1(A^2,N,C^2 + 2*C*D^N + D) :- match(A,D^N+C),!.
expon_exp1(A^2,N,C^2 + 2*C*E*D^N + D*E^2) :- match(A,C+E*D^N),!.
expon_exp1(A^2,N,C^2*D) :- match(A,C*D^N),!.

expon_inv_exp1(A^2,N,C^2 + 2*C*D^N + D^(-1)) :- match(A,D^N+C),!.
expon_inv_exp1(A^2,N,C^2 + 2*C*E*D^N + D^(-1)*E^2) :- match(A,C+E*D^N),!.
expon_inv_exp1(A^2,N,C^2*D^(-1)) :- match(A,C*D^N),!.

neg_exp(A^2,N,A^2) :- wordsin(A,L),L=[],!.
neg_exp(A^2,N,X*Y) :- match(A,B*C),!,neg_exp(B^2,N,X),neg_exp(C^2,N,Y).
neg_exp(A^2,N,B^E+2*C*B^N +C^2) :- 
	match(A,B1+C),
	neg_exp_match(B1,F,B,N), 
	eval(2*N,E),
	!.

neg_exp(A^2,_,A^2) :- !.


neg_exp_match(Exp,1,B,N) :- match(Exp,B^N),!.
neg_exp_match(Exp,F,B,N) :- match(Exp,F*B^N),!.

strip(L,N,L1) :- append(L1,List,L),length(List,N),!.

multiply_through(Lhs=Rhs,List,New,X) :- 
	dist(Lhs,Exp),
	decomp(Exp,[+|L]),
	mult(List,L,NewL),
	recomp(NewLhs,[+|NewL]),
	free_mult(List,Rhs,NewRhs),
	weak_normal_form(NewLhs=NewRhs,X,Left=Right),
	tidy(Left=Right,New),
	!.

dist(Old,New) :- prepd(Old,New1),dist1(New1,New),!.

dist1(A+B,C+D) :- !,dist1(A,C),dist1(B,D),!.	
dist1((A+B)*C,Y + Z) :- !,dist1(A*C,Y),dist1(B*C,Z).
dist1(C*(A+B),Y+Z) :- !,dist1(A*C,Y),dist1(B*C,Z).
dist1(C*(A+B)*D,Y+Z) :- !,dist1(C*D*A,Y),dist1(C*D*B,Z).
dist1(X,X) :- !.

prepd(X,Y) :- decomp(X,[*|L]), prepd1(L,Y),!.
prepd(X,X) :- !.

prepd1(L,Y) :- get_dist(L,Mult,[],Plus),re_dist(Mult,Plus,Y),!.

get_dist([],_,_,_) :- !,fail.
get_dist([A+B|T],Prod,Acc,A+B) :- !,append(T,Acc,Prod1),recomp(Prod,[*|Prod1]).
get_dist([H|T],Ans,Acc,Plus) :- !,append([H],Acc,Newacc),
get_dist(T,Ans,Newacc,Plus).
	

re_dist(M1,P+Q,X+Y) :- prepd(M1*P,X),prepd(M1*Q,Y),!.


mult(Termlist,[],[]) :- !.
mult(Termlist,[H|Rest],[NewH|NewRest]) :-
	domult(Termlist,H,NewH),
	mult(Termlist,Rest,NewRest).

domult(Termlist,H,NewH) :-
	mulbag_to_list(H,Mullist),
	domult(Termlist,Mullist,NewH,1).

domult([],Args,Term*Acc,Acc) :-
	!,
	recomp(Term,[*|Args]).
domult([U^N|Rest],Args,Prod,Acc) :-
	exp_member(U,Args,NewArgs,K),
	eval(K-N,M),
	domult(Rest,NewArgs,Prod,U^M*Acc),
	!.

mulbag_to_list(H,Mullist) :- decomp(H,[*|Mullist]), !.
mulbag_to_list(H,[H]).

exp_member(U,[],[],0) :- !.
exp_member(U,[H|Rest],Rest,1) :- match(H,U), !.
exp_member(U,[H|Rest],Rest,K) :- match(H,U^K),eval(K<0), !. %fix???
exp_member(U,[H|Rest],[H|NewRest],K) :-
	exp_member(U,Rest,NewRest,K).

free_mult(List,0,0) :- !.
free_mult([],Term,Term) :- !.
free_mult([U^N|Rest],Term,NewTerm) :-
	eval(-N,M),
	free_mult(Rest,U^M*Term,NewTerm).


subnasty(_,[],[]) :- !.
subnasty(X,[H|T],[H|T1]) :- nasty(X,H),!,subnasty(X,T,T1).
subnasty(X,[_|T],T1) :- subnasty(X,T,T1),!.

subintegral([],[]) :- !.
subintegral([H|T],[H|T1]) :- integral(H),!,subintegral(T,T1).
subintegral([_|T],T1) :- subintegral(T,T1),!.

checkpt([]) :- !.
checkpt([H|T]) :- pt(H),checkpt(T),!.
