%   File   :  /usr/bs/pressdir/methods/trig.fac
%   Author : Bernard Silver
%   Updated: Wed Nov 20 09:53:30 1985
%   Purpose: Trigonometric Factorization for Press


 % Try to solve trig equations of the form A=0, where A contains only
 % sin and cos and terms in linear form  

 % Also solves a*cos(x) + b*sin(x) =c ,see  comments on derive/7 


 % Top Level 
trig_fac(A=C,X,New) :- 
        trig_normal_form(X,A,List),
        trigmethod(X,List,Type),
        trigsolve(X,List,C,Type,New),
        !.

linear_sin_cos(A=_B,X) :- !,linear_sin_cos(A,X).

linear_sin_cos(A+B,X) :- !,linear_sin_cos(A,X),linear_sin_cos(B,X).

linear_sin_cos(A*B,X) :- !,
        decomp(A*B,[*|List]),
        sublist(contains(X),List,[New]),
        linear_sin_cos(New,X).

linear_sin_cos(Z,X) :- freeof(X,Z),!.

linear_sin_cos(sin(_),_) :- !.

linear_sin_cos(cos(_),_) :- !.


trig_normal_form(X,A,List) :-
        unattract_distribute(X,A,New),
        decomp(New,[+|NewList]),
        maptrigtype(X,NewList,List).

unattract_distribute(X,A,New) :-
        decomp(A,[*|List]),
        !,
        collect_multipliers(X,List,1,Mults,[],[Rest]),
        tidy(Mults,NewMult),
        dist_multiply(NewMult,Rest,New).

unattract_distribute(X,A,New) :- 
        decomp(A,[+|New1]),
        !,
        mapunattract_distribute(X,New1,New2),
        recomp(New,[+|New2]).

unattract_distribute(_,A,A).

mapunattract_distribute(_,[],[]).

mapunattract_distribute(X,[H|New1],[H1|New2]) :-
        unattract_distribute(X,H,H1),
        mapunattract_distribute(X,New1,New2).

collect_multipliers(_,[],Acc,Acc,Acc1,Acc1) :- !.

collect_multipliers(X,[H|T],Acc,Ans,Acc1,Ans1) :-
        freeof(X,H),
        !,
        collect_multipliers(X,T,Acc*H,Ans,Acc1,Ans1).

collect_multipliers(X,[H|T],Acc,Ans,Acc1,Ans1) :- 
        collect_multipliers(X,T,Acc,Ans,[H|Acc1],Ans1).


dist_multiply(A,B+C,D+E) :- !,dist_multiply(A,B,D),dist_multiply(A,C,E).

dist_multiply(A,B,C) :- tidy(A*B,C),!.


 % Put each trig term into the form tf(Fun,Mult,Ang,Rest,plus(Coeff,Add))
 % where Fun is the functor,Ang the angle.Ang is of the form
 % Coeff*Rest + Add, where Rest contains the unknown,and Coeff is a number
 % Mult is the coeff of the trig term.eg 2*sin(3*a*x) becomes
 % tf(sin,2,3*a*x,a*x,plus(3,0))   

maptrigtype(_,[],[]) :- !.
maptrigtype(X,[H|T],[H1|T1]) :- trigtype(X,H,H1),maptrigtype(X,T,T1),!.

trigtype(Unk,X,tf(Fun,1,Ang,Rest,Coeff)) :- 
        trigf(X),
        functor(X,Fun,1),
        arg(1,X,Ang),
        mod_angsize(Unk,Rest,X,Coeff),!.

trigtype(Unk,A,tf(Fun,Y,Ang,Rest,Coeff)) :- 
        match(A,X*Y),
        trigf(X),
        freeof(Unk,Y),
        functor(X,Fun,1),
        arg(1,X,Ang),
        mod_angsize(Unk,Rest,X,Coeff),!.


 % Classify the equation into three types: Does it contain only two terms
 % or does it contain only sin (or cos) terms whose angles are in A.P.,or
 % is it a mixture of sines and cosines  
trigmethod(_,List,two(norm)) :- length(List,2),!.
trigmethod(X,List,ap(A,D)) :-  
        checklist(sincos(_Type),List),
        apcheck(X,List,A,D),
        !.

trigmethod(_,List,mixed(Sins,Cos)) :- 
        sublist(sincos(sin),List,Sins),
        sublist(sincos(cos),List,Cos),
        length(Cos,M),
        M>0,
        length(Sins,N),
        N>0,
        M+N>2,
        !.


 % Eqn is A=0 where A is C*sin(X) + C*sin(Y),or C*cos(X) + C*cos(Y),
 % or C*sin(X)-C*sin(Y),or C*cos(X)-C*cos(Y)       

trigsolve(_,[tf(Y,N,Ang1,R1,Co1),tf(Y,M,Ang2,R2,Co2)],0,two(X),Newform=0) :-
        (M=N;eval(-M,N)),
        add_angle(Y,N,M,Ang1,Ang2,R1,R2,Co1,Co2,Newform),
        (X=norm ->
        trace_press('\nUsing trigonometric addition\n %t = 0\n',[Newform],1);
        true),
        !.

 % Both terms have the same angle  
trigsolve(_,[tf(Y,N,Ang,_,_),tf(Z,M,Ang,_,_)],C,two(norm),Newform) :-
        derive(C,Y,Z,M,N,Ang,Newform),
        trace_press('\n%t \n',[Newform],1),
        !.

 % Terms have angles of form M.X+C and N.X + D where  M= -N or M = N, and
 % C, D, M and N are free of x.
 % The equation is expanded to the form SC*sin(M1*X) + CC*cos(M1*X) = C
 % where M1 is the positive one of M and N.  If SC and CC are both non-zero
 % the equation is kept in normal form, and solved as for the normal two term
 % case, giving tan is C = 0, otherwise using the R*sin(M1*X+B) form.
 % If one of SC or CC is 0 the intermediate result is passed back to the
 % solve procedure of PRESS

trigsolve(X,[tf(Y,M,_,R,plus(Co1,Add1)),
tf(Z,N,_,R,plus(Co2,Add2))],C,two(norm),New) :-
        (eval(-Co1,Co2),Flag= -1;Co1=Co2,Flag=1),
        ((eval(Co1>0) -> Coeff=Co1,
        expand_and_collect(Flag,Y,M,Z,N,Coeff,Add1,Add2,R,C,Mid,CC,SC));
        (Coeff=Co2,
        expand_and_collect(Flag,Z,N,Y,M,Coeff,Add2,Add1,R,C,Mid,CC,SC))),
        trace_press('\nExpanding and collecting terms gives\n\n%t\n',[Mid],1),
        tidy(Coeff*R,Ang),
        ((eval(CC\=0),eval(SC\=0) ->
        trigsolve(X,[tf(sin,SC,Ang,Rest,Coeff),tf(cos,CC,Ang,Rest,Coeff)],
C,two(norm),New));
        New=Mid),
        !.


 % The terms have different functors and angles,but same coeff 
trigsolve(X,[tf(Y,M,Ang1,R1,Co1),tf(Z,N,Ang2,_R2,_Co2)],0,two(norm),Newform) :-
        (M=N;eval(-M,N)),
        ((Y=sin,Z=cos) -> M1=M,N1=N;
        Y=cos,Z=sin,N1=M,M1=N),
        convert_functor(X,M1,N1,Ang1,Ang2,R1,Co1,Newform),
        !.

 % AP case  
trigsolve(_,List,C,ap(A,D),New1) :- 
        length(List,N),
        odd(N),
        checkpairs(List,A,D,N,Term1+Term2),
        tidy(Term1+Term2=C,New),
        trace_press('\nAdding in pairs\n%t\n',[New],1),
        try_factorize(apcase(C),Term1,Term2,New1),
        !.

 % Mixed case 
trigsolve(X,_List,0,mixed(Sin,[A]),Final) :-
        trigsolve(X,Sin,0,two(mixed),New1=0),
        inv_trigtype(A,Term),
        tidy(New1 +Term =0,New),
        trace_press('\nAdding sin terms \n%t \n',[New],1),
        try_factorize(addone,New1+Term,Final),
        !.

trigsolve(X,_List,0,mixed([A],Cos),Final) :-
        trigsolve(X,Cos,0,two(mixed),New1=0),
        inv_trigtype(A,Term),
        tidy(New1 +Term =0,New),
        trace_press('\nAdding cosine terms \n%t \n',[New],1),
        try_factorize(addone,New1+Term,Final),
        !.

trigsolve(X,_List,0,mixed(Sin,Cos),Final) :-
        trigsolve(X,Sin,0,two(mixed),New1=0),
        trigsolve(X,Cos,0,two(mixed),New2=0),
        tidy(New1 + New2 =0,New),
        trace_press('\nAdding sin terms and cos terms\n%t \n',[New],1),
        try_factorize(addboth,New1+New2,Final),
        !.

  % Do some factorization, to take the load off collection
  % This is hacky, should be done using tf/5 representation.

try_factorize(apcase(Rhs),Term1,Term2,New) :-
        match(Term1,Fac*B),
        \+ atomic(Fac),
        match(Term2,Fac*C),
        tidy(Fac*(B+C)=Rhs,New),
        trace_press('\n%t\n',[New],1),
        !.

try_factorize(apcase,Term1,Term2,New) :-
        match(Term1,Term2*_B),
        \+ atomic(Term2),
        tidy(Term2*(Term1+1)=0,New),
        trace_press('\n%t\n',[New],1),
        !.

try_factorize(apcase,_,_) :- !,fail.

try_factorize(addone,A+B*G,F1=0) :-
        match(B*G,C*D),
        \+ atomic(C),
        match(A,C*E),
        tidy(C*(D+E),F1),
        trace_press('\n%t\n',[F1=0],1),
        !.

try_factorize(addone,A+B,F1=0) :-
        match(A,C*B),
        \+ atomic(B),
        tidy(B*(C+1),F1),
        trace_press('\n%t\n',[F1=0],1),
        !.

try_factorize(addboth,A+B*G,F1=0) :-
        match(B*G,C*D),
        \+ atomic(C),
        match(A,C*E),
        tidy(C*(D+E),F1),
        trace_press('\n%t\n',[F1=0],1),
        !.

try_factorize(_,Old,Old=0) :- !.

 % Sum of two sines case  
add_angle(sin,N,N,A1,A2,R1,R2,Coeff1,Coeff2,New) :-
        eval(N*2,N1),
        sumdiff(Coeff1,Coeff2,A1,A2,R1,R2,Sum,Diff,Sum1,Diff1),
        correct_sin(Sum,Sum1,Newsum,Fac,R1,R2),
        correct_cos(Diff,Diff1,Newdiff,R1,R2),
        tidy(Fac*N1*sin(Newsum)*cos(Newdiff),New),
        !.

 % Difference of two sines cases 
add_angle(sin,N,_M,A1,A2,R1,R2,Coeff1,Coeff2,New) :-
        eval(N>0),
        eval(N*2,N1),
        sumdiff(Coeff1,Coeff2,A1,A2,R1,R2,Sum,Diff,Sum1,Diff1),
        correct_sin(Diff,Diff1,Newdiff,Fac,R1,R2),
        correct_cos(Sum,Sum1,Newsum,R1,R2),
        tidy(Fac*N1*sin(Newdiff)*cos(Newsum),New),
        !.

add_angle(sin,_N,M,A1,A2,R1,R2,Coeff1,Coeff2,New) :-
        eval(M>0),
        eval(M*2,N1),
        sumdiff(Coeff2,Coeff1,A2,A1,R2,R1,Sum,Diff,Sum1,Diff1),
        correct_sin(Diff,Diff1,Newdiff,Fac,R1,R2),
        correct_cos(Sum,Sum1,Newsum,R1,R2),
        tidy(Fac*N1*sin(Newdiff)*cos(Newsum),New),
        !.

 % Sum of two cosines 
add_angle(cos,M,M,A1,A2,R1,R2,Coeff1,Coeff2,New) :-
        eval(M*2,N1),
        sumdiff(Coeff1,Coeff2,A1,A2,R1,R2,Sum,Diff,Sum1,Diff1),
        correct_cos(Sum,Sum1,Newsum,R1,R2),
        correct_cos(Diff,Diff1,Newdiff,R1,R2),
        tidy(N1*cos(Newsum)*cos(Newdiff),New),
        !.

 % Difference of two cosines 
add_angle(cos,M,_N,A1,A2,R1,R2,Coeff1,Coeff2,New) :-
        eval(M>0),
        eval(M*2,N1),
        sumdiff(Coeff2,Coeff1,A2,A1,R2,R1,Sum,Diff,Sum1,Diff1),
        correct_sin(Sum,Sum1,Newsum,Fac1,R1,R2),
        correct_sin(Diff,Diff1,Newdiff,Fac2,R1,R2),
        tidy(N1*Fac1*Fac2*sin(Newsum)*sin(Newdiff),New),
        !.

add_angle(cos,_M,N,A1,A2,R1,R2,Coeff1,Coeff2,New) :-
        eval(N>0),
        eval(N*2,N1),
        sumdiff(Coeff1,Coeff2,A1,A2,R1,R2,Sum,Diff,Sum1,Diff1),
        correct_sin(Sum,Sum1,Newsum,Fac1,R1,R2),
        correct_sin(Diff,Diff1,Newdiff,Fac2,R1,R2),
        tidy(N1*Fac1*Fac2*sin(Newsum)*sin(Newdiff),New),
        !.



 % Find the half_sum and half_difference of two angles 

 % Angles are of the form A*R and B*R,A and B are numbers 
sumdiff(plus(A,0),plus(B,0),_,_,R,R,Sum,Diff,Sum1,Diff1) :- 
        eval((A+B)/2,Sum1),
        eval((A-B)/2,Diff1),
        tidy(Sum1*R,Sum),
        tidy(Diff1*R,Diff),
        !.

 % General case 
sumdiff(_,_,A1,A2,_,_,Sum,Diff,Sum,Diff) :- 
        simplify_tidy((A1+A2)/2,Sum),
        simplify_tidy((A1-A2)/2,Diff),
        !.

 % Equation is M*sin(Ang)+N*cos(Ang) =0,so tan(Ang)=-N/M  
derive(0,sin,cos,M,N,Ang,tan(Ang) = K) :- eval((-N)/M,K),!. 
derive(0,cos,sin,N,M,Ang,tan(Ang) = K) :- eval((-N)/M,K),!.

 % Equation is M*sin(Ang)+N*cos(Ang) = C,so (M^2+N^2)sin(Ang+Beta) = C,
 % where beta is arctan(N/M)  

 % At present this is the best place for this rule as:
 % Should only be used as a collection rule when there are only 2 terms
 % If homogenization is used on the general case,the simplify routines
 % get overloaded. 

derive(C,sin,cos,M,N,Ang,New = C) :- 
        eval((M^2+N^2)^(1/2),R),
        eval(arctan(N/M),Beta),
        tidy(R*sin(Ang+Beta),New),
        !.

derive(C,cos,sin,N,M,Ang,New = C) :- 
        eval((M^2+N^2)^(1/2),R),
        eval(arctan(N/M),Beta),
        tidy(R*sin(Ang+Beta),New),
        !.

 % Convert cos(X) to sin(90-X)  
convert_functor(X,M,M,Ang1,Ang2,R1,Co1,NewE) :-
        tidy((90-Ang2),Newang),
        tidy(M*(sin(Ang1) + sin(Newang))=0,New),
        trace_press('\nRewriting (cos(X) = sin(90-X))\n%t\n',[New],1),
        mod_angsize1(X,NR,Newang,NC),
        trigsolve1(X,[tf(sin,M,Ang1,R1,Co1),tf(sin,M,Newang,NR,NC)],Co1,NC,NewE),
        !.

convert_functor(X,M,N,Ang1,Ang2,R1,Co1,NewE) :-
        eval(M>0),
        tidy((90-Ang2),Newang),
        tidy(M*(sin(Ang1) - sin(Newang))=0,New),
        trace_press('\nRewriting (cos(X) = sin(90-X)\n%t\n',[New],1),
        mod_angsize1(X,NR,Newang,NC),
        trigsolve1(X,[tf(sin,M,Ang1,R1,Co1),tf(sin,N,Newang,NR,NC)],Co1,NC,NewE),
        !.

convert_functor(X,N,M,Ang1,Ang2,R1,Co1,NewE) :-
        eval(M>0),
        tidy((90-Ang2),Newang),
        tidy(M*(sin(Newang) - sin(Ang1))=0,New),
        trace_press('\nRewriting (cos(X) = sin(90-X)\n%t\n',[New],1),
        mod_angsize1(X,NR,Newang,NC),
        trigsolve1(X,[tf(sin,M,Newang,NR,NC),tf(sin,N,Ang1,R1,Co1)],Co1,NC,NewE),
        !.

 % Check equation has not become trivial
trigsolve1(_X,_List,Coeff1,Coeff2,true)  :-
        tidy(Coeff1,NewC),
        tidy(Coeff2,NewC),
        !,
        trace_press('\nEquation collapses to 0 = 0\n',1).

trigsolve1(X,List,_,_,Ans) :- trigsolve(X,List,0,two(norm),Ans).

 % Find the coefficient and remainder of the angle
mod_angsize(X,U,T,Ans) :- arg(1,T,Z),mod_angsize1(X,U,Z,Ans),!.
        
mod_angsize1(X,U,Z,plus(N,B1)) :- 
        match(Z,A+B),
        contains(X,A),
        freeof(X,B),
        mod_angsize1(X,U,A,plus(N,C)),
        tidy(B+C,B1),
        !.

mod_angsize1(X,U,Z,plus(N,0)) :- 
        match(Z,N*U),
        ok_number(N),
        contains(X,U),
        !.

mod_angsize1(X,Z,Z,plus(1,0)) :- \+ ok_number(Z),contains(X,Z),!.

sincos(sin,tf(sin,_,_,_,_)) :- !.
sincos(cos,tf(cos,_,_,_,_)) :- !.

checksin_cos1([]) :- !.
checksin_cos1([sin(_)|T]) :- checksin_cos1(T),!.
checksin_cos1([cos(_)|T]) :- checksin_cos1(T),!.

 % AP case  
 
apcheck(_X,List,A,D) :- 
        maplist(get_coeff,List,Newlist),
        apcheck1(Newlist,A,D),
        trace_press('\nAngles are in arithmetic progression \n',1),
        !.

get_coeff(tf(_,_,_,_,C),C) :- !.

apcheck1(L,plus(A,0),diff(D,0)) :- 
        non_add(L,L1),
        sort(L1,[A|S]),
        apcheck2([A|S],D),
        !.

apcheck1(L,plus(X,A),diff(0,D)) :- 
        additive_angles(L,L1,X),
        sort(L1,[A|S]),
        apcheck2([A|S],D),
        !.

apcheck2([],_) :- !.
apcheck2([_N],_) :- !.
apcheck2([H,H1|T],D) :- simplify_tidy(H1-H,D),apcheck2([H1|T],D),!.

non_add([],[]) :- !.
non_add([plus(X,0)|S],[X|T]) :- non_add(S,T),!.

additive_angles([],[],_) :- !.
additive_angles([plus(X,A)],[A],X) :- !.
additive_angles([plus(X,A),plus(X,B)|S],[A|T],X) :- 
        additive_angles([plus(X,B)|S],T,X),
        !.

checkpairs(List,A,_,1,Term) :-
        member(tf(T,C,Z,Y,A),List),
        inv_trigtype(tf(T,C,Z,Y,A),Term),
        !.

checkpairs(List,plus(A,0),diff(D,0),N,New1+Tail) :-
        member(tf(T,C,Z,Y,plus(A,0)),List),
        simplify_tidy(A+(N-1)*D,X),
        member(tf(T,C,Z1,Y,plus(X,0)),List),
        add_angle(T,C,C,Z,Z1,Y,Y,plus(A,0),plus(X,0),New1),
        simplify_tidy(A+D,V),
        eval(N-2,N1),
        checkpairs(List,plus(V,0),diff(D,0),N1,Tail),
        !.

checkpairs(List,plus(A,A1),diff(0,D),N,New1+Tail) :-
        member(tf(T,C,Z,Y,plus(A,A1)),List),
        simplify_tidy(A1+(N-1)*D,X),
        member(tf(T,C,Z1,Y1,plus(A,X)),List),
        add_angle(T,C,C,Z,Z1,Y,Y1,plus(A,A1),plus(A,X),New1),
        simplify_tidy(A1+D,V),
        eval(N-2,N1),
        checkpairs(List,plus(A,V),diff(0,D),N1,Tail),
        !.

inv_trigtype(tf(sin,C,Z1,_,_),C*sin(Z1)) :- !.
inv_trigtype(tf(cos,C,Z1,_,_),C*cos(Z1)) :- !.



 % Equation is M*sin(Coeff*R+Add1)+N*sin(Coeff1*R+Add2) = C

 % If Coeff = Flag*Coeff1, where Flag is 1 or -1, this expands as
 % (sin(Coeff*R)*(M*cos(Add1) + Flag*N*cos(Add2)) + 
 %      cos(Coeff*R)*(M*sin(Add1)+N*sin(Add2))) = C


expand_and_collect(Flag,sin,M,sin,N,Coeff,Add1,Add2,R,C,Mid,CC,SC) :-
        simplify_tidy(M*cos(Add1) + Flag*N*cos(Add2),SC),
        simplify_tidy(M*sin(Add1) + N*sin(Add2),CC),
        simplify_tidy(sin(Coeff*R)*SC+cos(Coeff*R)*CC=C,Mid),
        !.

 % Same for two cosines
 % Expands as cos(Coeff*R)*(M*cos(Add1) - Flag*N*cos(Add2)) +
 % sin(Coeff*R)*(N*sin(Add2)-M*sin(Add1)) = C

expand_and_collect(Flag,cos,M,cos,N,Coeff,Add1,Add2,R,C,Mid,CC,SC) :-
        simplify_tidy(M*cos(Add1) - Flag*N*cos(Add2),SC),
        simplify_tidy(N*sin(Add2) - M*sin(Add1),CC),
        simplify_tidy(sin(Coeff*R)*SC+cos(Coeff*R)*CC=C,Mid),
        !.

 % One sin and one cos
expand_and_collect(Flag,cos,M,sin,N,Coeff,Add1,Add2,R,C,Mid,CC,SC) :-
        simplify_tidy(M*cos(Add1) + N*sin(Add2),CC),
        simplify_tidy(M*sin(Add1) -  N*Flag*cos(Add2),SC),
        simplify_tidy(cos(Coeff*R)*CC - sin(Coeff*R)*SC=C,Mid),
         !.

 % Note reversal of order of M, N etc!

expand_and_collect(Flag,sin,N,cos,M,Coeff,Add2,Add1,R,C,Mid,CC,SC) :-
        simplify_tidy(M*cos(Add1) + N*Flag*sin(Add2),CC),
        simplify_tidy(M*sin(Add1) + N*cos(Add2),SC),
        simplify_tidy(cos(Coeff*R)*CC - sin(Coeff*R)*SC=C,Mid),
        !.

 % Get signs right
correct_sin(_,Sum,X,F,Unk,Unk) :- ok_number(Sum),correct_sin1(Sum,F,X,Unk),!.
correct_sin(_,Sum,Sum,1,_,_) :- !.


correct_cos(_,Sum,X,Unk,Unk) :- ok_number(Sum),correct_cos1(Sum,X,Unk),!.
correct_cos(_,Sum,Sum,_,_) :- !.

correct_sin1(Sum,1,New,Unk) :- eval(Sum>0),tidy(Sum*Unk,New),!.
correct_sin1(Sum,(-1),New,Unk) :- tidy(-Sum*Unk,New),!.

correct_cos1(Sum,New,Unk) :- eval(Sum>0),tidy(Sum*Unk,New),!.
correct_cos1(Sum,New,Unk) :- tidy(-Sum*Unk,New),!.

simplify_tidy(Old,New) :-
        tidy(Old,Mid),
        simplify(Mid,New).
