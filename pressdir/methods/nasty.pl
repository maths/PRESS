/*              NASTY                   14.9.81   
                                        Updated: 10 September 82
*/
%declarations%

% :- public
%                 findbase/2,
%                 good_fun/1,
%                 invert_exp/2,
%                 nasty/2,
%                 nasty2/2,
%                 nasty_method/3,
%                 nice_at/1,
%                 pt/1,
%                 pta/1.
%end%


/*              CODE                                    */
/* Nasty in the context of the code and comments means a term u^x where
x is a rational non-integer and u is anything.Here offending term means
the same as it does in homogenization  */


/* Has equation been seen before */
nasty_method(Eqn,X,NewEqn) :- 
        looping(Eqn,X),
        tidy(Eqn,Eqn1),
        try_nasty_method(Eqn1,X,Eqn2),
        weak_normal_form(Eqn2,X,NewEqn),
        !.

/* Try to deal with non-rational nasty functions  */
try_nasty_method(Eqn,X,Neweqn) :- 
        parse4(Eqn,X,U,other),
        subnasty(X,U,V),
        find_symbols(Eqn,V,Symbols,Posns),
        nasty_act(Symbols,Posns,Eqn,X,Neweq),
        tidy(Neweq,Neweqn),
        !.

/* Clear rational functions */
try_nasty_method(Eqn,X,Neweqn) :- 
        parse4(Eqn,X,U,neg),
        exp_nasty_list(X,U,V),
        remove_subsumed(V,Termlist),
        multiply_through(Eqn,Termlist,Neweqn,X),
        tidy(Neweqn,New),
        trace_press('\nClearing of rational functions\n\n%t\n',[New],1),
        !.


/* The isolate case   */

nasty_act(Symbols,[Posn|_],Eqn,_X,New) :- 
        nice(Symbols),
        append(Posn,[1],Posn1),
        position(Term,Eqn,Posn1),
        trace_press('\nTrying to isolate %t\n in %t\n',[Term,Eqn],1),
        try_isolate(Posn1,Eqn,New),
        !.


try_isolate(Posn,Eqn,New) :- isolate(Posn,Eqn,New),!.
try_isolate(_,_,_) :-   writef_press('\nFailed to isolate\n'),!,fail.

/* The cancelling pair case   */
/* Left to tidy at present  */
/* Eventually we will need rules to cancel sin(arcsin(x)) etc  */

/* Attraction case  */

nasty_act(Symbols,Posns,Eqn,_X,New) :- 
        find_attract_list(Symbols,N,L,Type),
        nmember(Posn,Posns,N),
        strip(Posn,L,Newp),
        position(Term,Eqn,Newp),
        nas_rule(Term,Nterm,Type),
        subst(Term=Nterm,Eqn,New1),
        tidy(New1,New),
        trace_press('\nAttracting nasty functions\n%t\n',[New],1),
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

/* See if any of the terms found are nasty rather than offending  */

nasty(X,Y) :- root_nasty(X,Y),!.
nasty(X,Y) :- exp_nasty(X,Y),!.
nasty(X,Y) :- trig_nasty(X,Y),!.

/* Root type nasty */
root_nasty(X,U^N) :- contains(X,U),ok_number(N),\+ integer(N),eval(N>0),!.

/* Negative exponent nasty */
exp_nasty(X,U^N) :- contains(X,U),ok_number(N),eval(N<0),diff(X,U),!.

exp_nasty_list(_,[],[]) :- !.
exp_nasty_list(X,[H|Rest],[H|RestV]) :-
        exp_nasty(X,H),
        !,
        exp_nasty_list(X,Rest,RestV).
exp_nasty_list(X,[_|Rest],RestV) :-
        exp_nasty_list(X,Rest,RestV).

trig_nasty(X,Y) :- (arctrigf(Y);trigf(Y)),contains(X,Y),!.

/* Find the functions dominating,and the positions of,the nasty functions */
find_symbols(_,[],[],[]) :- !.

        find_symbols(E,[H|T],[H1|T1],[H2|T2]) :- find_symbols1(E,H,H1,H2),
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

expon(_U^N,exp(N)) :- ok_number(N),!.
expon(X,X) :- arctrigf(X),!.
expon(X,X) :- trigf(X),!.

/* Remove terms form list if they are subsumed by others,ie
if U^-N and U^-M,M>N both occur keep only U^-M    */
remove_subsumed([],_) :- !, fail.
remove_subsumed(V,Termlist) :-
        listtoset(V,List),              % Cheap test
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

member_match(_H,[],_) :- !, fail.
member_match(H,[U^N|Rest],[U^K|Rest]) :-
        match(H,U^M),
        !,
        least(N,M,K).
member_match(H,[Term|Rest],[Term|NewRest]) :-
        member_match(H,Rest,NewRest).

least(N,M,N) :- eval(N=<M), !.
least(_N,M,M).

/* Is the function dominating list nice,ie can isolation be used  */
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

/* Isolatable functions (need to add arctrig etc) */
good_fun(+) :- !.
good_fun(=) :- !.
good_fun(*) :- !.
good_fun(X) :- arctrigf(X),!.
good_fun(exp(N)) :- ok_number(N),\+ integer(N),eval(numer(N)=1),!.

/* Is the function dominating list attractable */
find_attract_list([],_,_,_) :- !,fail.
find_attract_list([H|_T],1,M,Type) :- attract_list(H,M,Type),!.
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
attract_list([X|T],K,trig) :- trigf(X),checkpta(T),length(T,K),!.

attract_list([_|T],M,Type) :- attract_list(T,M,Type),!.


get_nasty_type(M,N,root(M)) :- eval(1/N,M),!.
get_nasty_type(M,N,negroot(M)) :- eval(1/N,-1*M),!.
get_nasty_type(M,_N,neg(M)) :- eval(M<0),!.

pt(*) :- !.
pt(+) :- !.

pta(X) :- pt(X),!.
pta(X) :- arctrigf(X),!.

arctrigf(X) :- member(X,[arcsin(_),arccos(_),arctan(_)]),!.
/* Attraction Rules (many to be added)  */

nas_rule(A^2 ,Exp,root(N)) :- dist(A,A1),tidy(A1,A2),expon_exp(A2^2,N,Exp),!.
nas_rule(A^2,Exp,negroot(N)) :- 
        dist(A,A1),
        tidy(A1,A2),
        expon_inv_exp(A2^2,N,Exp),
        !.
nas_rule(A^2,Exp,neg(N)) :- neg_exp(A^2,N,Exp),!.
nas_rule(sin(X),Exp,trig) :- sinatt(X,Exp),!.
nas_rule(cos(X),Exp,trig) :- cosatt(X,Exp),!.
nas_rule(tan(X),Exp,trig) :- tanatt(X,Exp),!.

expon_exp(Old,N,New) :- eval(N=(1/2)),expon_exp1(Old,N,New),!.

expon_inv_exp(Old,N,New) :- eval(N=(-1/2)),expon_inv_exp1(Old,N,New),!.

expon_exp1(A^2,N,C^2 + 2*C*D^N + D) :- match(A,D^N+C),!.
expon_exp1(A^2,N,C^2 + 2*C*E*D^N + D*E^2) :- match(A,C+E*D^N),!.
expon_exp1(A^2,N,C^2*D) :- match(A,C*D^N),!.

expon_inv_exp1(A^2,N,C^2 + 2*C*D^N + D^(-1)) :- match(A,D^N+C),!.
expon_inv_exp1(A^2,N,C^2 + 2*C*E*D^N + D^(-1)*E^2) :- match(A,C+E*D^N),!.
expon_inv_exp1(A^2,N,C^2*D^(-1)) :- match(A,C*D^N),!.

neg_exp(A^2,_N,A^2) :- wordsin(A,L),L=[],!.
neg_exp(A^2,N,X*Y) :- match(A,B*C),!,neg_exp(B^2,N,X),neg_exp(C^2,N,Y).
neg_exp(A^2,N,B^E+2*C*B^N +C^2) :- 
        match(A,B1+C),
        neg_exp_match(B1,_F,B,N), 
        eval(2*N,E),
        !.
neg_exp(A^2,_,A^2) :- !.


neg_exp_match(Exp,1,B,N) :- match(Exp,B^N),!.
neg_exp_match(Exp,F,B,N) :- match(Exp,F*B^N),!.

sinatt(X,Exp) :- match(X,(-1)*Y),sinatt(Y,E1),tidy((-1)*E1,Exp),!.
sinatt(A+B,Exp) :- 
        trig_inv(sin(A),X,F1),
        trig_inv(cos(A),Y,F2),
        trig_inv(sin(B),Z,F3),
        trig_inv(cos(B),W,F4),
        member(cancel,[F1,F2,F3,F4]),
        merge(X*W,X1),
        merge(Y*Z,X2),
        tidy(X1 + X2,Exp),
        !.

cosatt(X,Exp) :- match(X,Y*(-1)),cosatt(Y,Exp),!.
cosatt(A+B,Exp) :- 
        trig_inv(sin(A),X,F1),
        trig_inv(cos(A),Y,F2),
        trig_inv(sin(B),Z,F3),
        trig_inv(cos(B),W,F4),
        member(cancel,[F1,F2,F3,F4]),
        merge(W*Y,X1),
        merge(Z*X,X2),
        tidy(X1 - X2,Exp),
        !.

tanatt(X,Exp) :- match(X,Y*(-1)),cosatt(Y,Exp1),tidy((-1)*Exp1,Exp),!.
tanatt(A+B,Exp) :- 
        trig_inv(tan(A),X,F1),
        trig_inv(tan(B),Y,F2),
        member(cancel,[F1,F2]),
        merge(X*Y,Z),
        tidy((X+Y)/(1-Z),Exp),
        !.

trig_inv(sin(X),Y,F) :- match(X,(-1)*Z),trig_inv(sin(Z),W,F),tidy((-1)*W,Y),!.
trig_inv(sin(arcsin(X)),X,cancel) :- !.
trig_inv(sin(arccos(X)),Y,cancel) :- tidy((1-X^2)^(1/2),Y),!.
trig_inv(sin(X),sin(X),no) :- !.

trig_inv(cos(X),Y,F) :- match(X,(-1)*Z),trig_inv(cos(Z),Y,F),!.
trig_inv(cos(arccos(X)),X,cancel) :- !.
trig_inv(cos(arcsin(X)),Y,cancel) :- tidy((1-X^2)^(1/2),Y),!.
trig_inv(cos(X),cos(X),no) :- !.

trig_inv(tan(X),Y,F) :- match(X,(-1)*Z),trig_inv(tan(Z),W,F),tidy((-1)*W,Y),!.
trig_inv(tan(arctan(X)),X,cancel) :- !.
trig_inv(tan(X),tan(X),no) :- !.

/* strip(L,M,L1) holds when removing the last M elements from list L
gives list L1 */
strip(L,N,L1) :- append(L1,List,L),length(List,N),!.

/* Do the multiplication to rationalize  */
multiply_through(Lhs=Rhs,List,New,_X) :- 
        dist(Lhs,Exp),
        decomp(Exp,[+|L]),
        mult(List,L,NewL),
        recomp(NewLhs,[+|NewL]),
        free_mult(List,Rhs,NewRhs),
        tidy(NewLhs=NewRhs,New),
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


mult(_Termlist,[],[]) :- !.
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

exp_member(_U,[],[],0) :- !.
exp_member(U,[H|Rest],Rest,1) :- match(H,U), !.
exp_member(U,[H|Rest],Rest,K) :- match(H,U^K),eval(K<0), !. %fix???
exp_member(U,[H|Rest],[H|NewRest],K) :-
        exp_member(U,Rest,NewRest,K).

free_mult(_List,0,0) :- !.
free_mult([],Term,Term) :- !.
free_mult([U^N|Rest],Term,NewTerm) :-
        eval(-N,M),
        free_mult(Rest,U^M*Term,NewTerm).

/* Looping Check */
looping(Eqn,X) :- normstore(Eqn,X,Eqn1),looping1(Eqn1),!.

looping1(Eqn1)  :- seen_eqn(Eqn1),
        !,
        trace_press('\n*****LOOPING*****\nI have seen equation before\n',1),
        trace_press('\nTracing\n',1),
        cond_trace_press,
        fail.

looping1(Eqn1) :- asserta(seen_eqn(Eqn1)),!.

normstore(Eqn,X,Eq) :- subst(X  = unk,Eqn,Eqn1),
        !,
        remove_arbs(Eqn1,Eqn2),
        tidy(Eqn2,Eq).


/* Remove arbitrary integers */

remove_arbs(Eqn1,Eqn2) :- 
        wordsin(Eqn1,Words),
        subintegral(Words,Word),
        remove_arbs1(Eqn1,Word,Eqn2),
        !.

remove_arbs1(X,[],X) :- !.
remove_arbs1(X,H,Y) :- make_arblist(H,Z),make_subl(H,Z,Y1),subs1(X,Y1,Y),!.

make_arblist(H,Z) :- make_arblist1(H,Z,1),!.

make_arblist1([],[],_) :- !.
make_arblist1([_H|T],[arb(N)|T1],N) :- M is N+1,make_arblist1(T,T1,M),!.

cond_trace_press :- flag(tflag,N,N),N>0,trace_press,!.
cond_trace_press :- !.

/*
seen_eqn(_) :- fail.

integral(_) :- fail.
*/

/* Merge roots in products      */
merge(A,X) :- eval(1/2,N),match(A,B^N*C^N),tidy(B*C,Y),tidy(Y^N,X),!.
merge(A,X) :- eval(1/2,N),match(A,Z*B^N*C^N),tidy(B*C,Y),tidy(Z*Y^N,X),!.
merge(A,A) :- !.

 % Converted sublists etc   

subnasty(_,[],[]) :- !.
subnasty(X,[H|T],[H|T1]) :- nasty(X,H),!,subnasty(X,T,T1).
subnasty(X,[_|T],T1) :- subnasty(X,T,T1),!.

subintegral([],[]) :- !.
subintegral([H|T],[H|T1]) :- integral(H),!,subintegral(T,T1).
subintegral([_|T],T1) :- subintegral(T,T1),!.

checkpt([]) :- !.
checkpt([H|T]) :- pt(H),checkpt(T),!.

checkpta([]) :- !.
checkpta([H|T]) :- pta(H),checkpta(T),!.
