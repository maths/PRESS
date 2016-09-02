/* SPRINT : Symbolic Integration
						David Skinner
	made compatible with current PRESS by Leon  Updated: 17 February 83
*/

integrate(Term,Term*X,X):- freeof(X,Term),!.

integrate(Term,Soln,X) :-
	tidy(Term,NTerm),
	integrate1(NTerm,Ans,X),
        tidy(Ans,Soln).


integrate1(Y+Z,YInt+ZInt,X) :-
 	!,
        integrate(Y,YInt,X),
        integrate(Z,ZInt,X).

integrate1(Term,Soln,X) :-
	derivdivides(Term,Soln,X), 
        !.



derivdivides(Term,Soln,X) :-
        mult_decomp(Term,[*|Elements]),
        ddivides(Elements,[],Soln,X).

derivdivides(Term,Const*(1/2)*(Fn^2),X) :-
       match(Term,Fn*Rest),
       mult_decomp(Fn,[*|Elements]),
       mult_decomp(Rest,[*|RestBag]),
       check(Fn,RestBag,Const,X).



ddivides([El|Rest],Scanned,Const*Ans,X)
        :-lookup(El,Arg,Ans),
          append(Rest,Scanned,Restofterm),
          check(Arg,Restofterm,Const,X).

ddivides([El|Rest],Scanned,Ans,X)
        :-ddivides(Rest,[El|Scanned],Ans,X).


check(Ux,Remainder,Const,X)
        :-diffwrt(Ux,DUx,X),
          tidy(DUx,NewDUx),
          mult_decomp(NewDUx,[*|DUxBag]),
          ksame(DUxBag,Remainder,Constbag,X),
          recomp(Const,[*|Constbag]).

mult_decomp(A,[*|B]) :- decomp(A,[*|B]).
mult_decomp(A,[*,A]).

ksame(L1,L2,Const,X)
y        :-freeof(X,L1),
          freeof(X,L2),
          specapp(L1,L2,Const),
          !.

ksame([FL1|RestL1],L2,Const,X)
        :-select(FL1,L2,NewL2),
          ksame(RestL1,NewL2,Const,X),
          !.

ksame([FL1|RestL1],L2,Const,X)
        :-freeof(X,FL1),
          not freeof(X,RestL1),
          append(RestL1,[FL1],NewL1),
          ksame(NewL1,L2,Const,X),
          !.

specapp(L1,L2,Ans)
        :-inverse(L1,NewL1),
          append(NewL1,L2,Ans).


inverse([],[]).

inverse([FList|RList],[FList^(-1)|RNeg])
          :-inverse(RList,RNeg).



lookup(cos(Arg),Arg,sin(Arg)).
lookup(sin(Arg),Arg,-cos(Arg)).
lookup(tan(Arg),Arg,log(sec(Arg))).
lookup(cot(Arg),Arg,log(sin(Arg))).
lookup(sec(Arg),Arg,log(sec(Arg)+tan(Arg))).
lookup(artan(Arg),Arg,Arg*artan(Arg)-log(1+Arg^2)*2^(-1)).
lookup(arsin(Arg),Arg,Arg*arsin(Arg)+(1-Arg^2)^2^(-1)).
lookup(log(Arg),Arg,Arg*log(Arg)-Arg).
lookup(Arg^(-1),Arg,log(Arg)).
lookup(Arg^D,Arg,(D+1)^(-1)*Arg^(D+1)):-freeof(x,D),
                                       D\==(-1).
lookup(D^Arg,Arg,(log(D)^(-1))*D^Arg):-freeof(x,D),
                                       D\==(-1).



tsimpax(-U,(-1)*U).
tsimpax(log(e),1).


