/* HOMOG.TOP : 

                                                Bernard Silver
                                                Updated: 9 September 82
*/                              



                                % HOMOGENIZATION  ROUTINE         
                                % NOTE: Requires equation is in 
                                % weak normal form 
                                


% Solve case of Homogenization with messages

homog(Eqn,X,New,Term,V,Off) :- 
        homog1(Eqn,X,New,Term,V,Off,Homeqn,solve),
        trace_press('\nRewriting equation in terms of %t\ngives %t\n',[Term,Homeqn],1),
        trace_press('\nSubstituting   %t for %t gives\n %t\n',[V,Term,New],1).

% Top Level of Homogenization proper 

homog1(Eqn,Unk,Neweqn,Term,V,Offend,Homeqn,Flag) :- 
        findtype(Type,Offend),
        trace_press('\nOffending set is %t\n',[Offend],2),
        anaz(Type,Eqn,Unk,Offend,Term,Flag),
        trace_press('\nReduced term is %t\n',[Term],2),
        perform_rewrites(Eqn,Term,Offend,Homeqn,Unk,Type),
        change_the_variable(Term,V,Unk,Homeqn,Neweqn).

 % Equation can have Homogenization applied to it
multiple_offenders_set(Eqn=_Rhs,Off,X) :-
        parse(Eqn,Off,X),
        length(Off,N),
        !,
        N>1.


 % Rewrite the offenders set and obtain new Homogenized equation
perform_rewrites(Eqn,Term,Offend,Homeqn,Unk,Type) :-
        rew(Term,Offend,Sub,Unk,Type),
        subs1(Eqn,Sub,Homeqn).

 % Now change the variable, reporting substitutions if neccesary

change_the_variable(Term,V,Unk,Homeqn,New) :-
        report_subs(Unk,_Sub),
        identifier(V),
        subst(Term=V,Homeqn,Neweqn),
        tidy(Neweqn,New).                               

% Find the offenders set (ie the terms which prevent the parsing 
% of Eqn as a rational equation ) (Assumes Eqn has been tidied 
% so no / or - occurs) 

parse(Eqn,Set,Unk) :- dl_parse(Eqn,Set1-[],Unk),listtoset(Set1,Set).

dl_parse(A+B,L-L1,Unk) :- !,dl_parse(A,L-L2,Unk),dl_parse(B,L2-L1,Unk).
dl_parse(A*B,L-L1,Unk) :- !,dl_parse(A,L-L2,Unk),dl_parse(B,L2-L1,Unk).
dl_parse(Unk^N,[Unk^N|L]-L,Unk) :- ok_number(N),!.
dl_parse(A^B,L,Unk) :- ok_number(B), !,dl_parse(A,L,Unk).
dl_parse(Unk,[Unk|L]-L,Unk) :- !.
dl_parse(A,L-L,Unk) :- freeof(Unk,A),!.
dl_parse(A,[A|L]-L,_Unk) :- !.

% Find the type of the offending set  

findtype(trig,L) :- checklist(trigf,L),!.
findtype(log(_),L) :- checklist(logf,L),!.
findtype(genpol,L) :- maplist(genpoly,L,L1),rational_gcd_list(L1,N),!,N \= 1.
findtype(exp,L) :- checklist(expp,L),!.
findtype(hyper,L) :- checklist(hyperf,L),!. %Just hyperbolics
findtype(hyper_exp,L) :- checklist(hypexp,L),!. %Hyperbolics and exponentials
findtype(mixed,_) :- !.

 % Recognizers for each type

trigf(X) :- member(X,[sin(_),cos(_),tan(_),sec(_),cosec(_),cot(_)]),!.
logf(X) :- member(X,[log(_,_)]),!.
hyperf(X) :- member(X,[sinh(_),cosh(_),sech(_),tanh(_),coth(_),cosech(_)]),!.
expp(_^_) :- !.
expp1(e^_) :- !.
hypexp(X) :- (expp1(X);hyperf(X)),!.
genpoly(X,1) :- atom(X),!.
genpoly(X^N,N) :- atom(X),ok_number(N),!.


%  Find which terms are hyperbolic and which are exponential in hyper_exp case
split_case(L,Exp,Hyp) :- split_case1(L,Exp,Hyp),non_trivial([Exp,Hyp]),!.

split_case1([],[],[]) :- !.
split_case1([H|T],[H|A],B) :- expp1(H),!,split_case1(T,A,B).
split_case1([H|T],A,[H|B]) :- hyperf(H),!,split_case1(T,A,B).

%  Check that both occur in this case

non_trivial([]) :- !.
non_trivial([[]|_]) :- !,fail.
non_trivial([_|T]) :- !,non_trivial(T).
 
% Try to choose reduced term . Arguments of anaz are
% Type of offenders set, Equation, the Unknown, the offenders set
% the reduced term, and a flag to show if the problem is a sim or solve one

 % Trig case, find the gcd of all angles that occur, then choose functor

anaz(trig,Eqn,Unk,Offend,Term,_) :- 
        findangle(Unk,Offend,Angle),!,
        anaz1(Eqn,Angle,Offend,Term,Unk,_).

%  Exponential case where terms are of the form a^f(x) where a is the same
%  for all members of the offending set. We find the 'gcd' of the f(x)

anaz(exp,_,Unk,Offend,Base^Power,_) :- 
        maplist(expcase1(Base,Rest,Unk),Offend,NewList),
        form(Rest,NewList,Power),!.

%  Other exponential case where terms are of the form a^(c*x+d).

anaz(exp,_,Unk,Offend,Base^Power,_) :- 
        maplist(expcase2(Unk),Offend,NewList),
        coeff_exp(NewList,Base,Rest),
        form1(Unk,Rest,Power),
        !.

%  Normal log case dealing with terms like log(x,4) and log(2,x) in the 
%  offenders set.

anaz(log(_),_,Unk,Offend,_Term,_) :- 
        maplist(laura(Arg2,Unk),Offend,NewList),
        onetest(NewList,Arg1),
        logocc(Arg1,Arg2,_X,Offend).


%  Other log case where the logs are converted to base 10.

anaz(log(10),_,Unk,Offend,log(10,Term),_) :- 
        checklist(laura1(Unk,Term),Offend),
        !.

 % The generalized polynomial case

anaz(genpol,_,Unk,Offend,U,_) :- 
        maplist(genpolcase(Unk),Offend,List1),
        signed(List1,P),
        rational_gcd_list(List1,N),
        eval(P*N,N1),
        form4(Unk,N1,U),
        !.

 % Hyperbolics.  Find the gcd of all the 'angles' as in trig case

anaz(hyper,Eqn,Unk,Offend,Term,Flag) :- 
        findangle(Unk,Offend,Angle),
        hyper_find(Eqn,Unk,Offend,Term,Angle,Flag),!.

 % Both exponentials and hyperbolics, find gcd of all angles and powers.

anaz(hyper_exp,_,Unk,Offend,e^Term,_) :- 
        split_case(Offend,Exp,Hyper),
        maplist(angle_size(Unk,Rest),Hyper,Angle),
        maplist(expcase1(e,Rest,Unk),Exp,NewList),
        append(Angle,NewList,Newlist1),
        rational_gcd_list(Newlist1,Gcd),
        form1(Rest,Gcd,Term),
        !.

% Choose reduced_term  using simplicity metric
  
anaz(_,_,Unk,Offend,T,_) :- 
        trace_press('Choosing reduced term via simplicity metric',2),
         reduced_term(Offend,Unk,T).

