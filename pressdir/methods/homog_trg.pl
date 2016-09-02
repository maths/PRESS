/* HOMOG.TRG : 

                                                Bernard Silver
                                                Updated: 2 October 82
*/
% :- public
%                 anaz1/6,
%                 angle_size/4,
%                 cc/2,
%                 cch/2,
%                 cosecfind/1,
%                 cosechp/3,
%                 cosecp/3,
%                 cosfind/1,
%                 coshp/3,
%                 cosp/3,
%                 cothp/3,
%                 cs/2,
%                 csh/2,
%                 expcc/4,
%                 expcs/4,
%                 expsc/4,
%                 expss/4,
%                 exptt/4,
%                 findangle/3,
%                 hyper_find/6,
%                 secfind/1,
%                 sechp/3,
%                 secp/3,
%                 sinfind/1,
%                 sinhp/3,
%                 st/2,
%                 sth/2,
%                 tanhp/3.

 %   Find gcd of angles in offending set 
findangle(Unk,Offend,Angle) :- 
        maplist(angle_size(Unk,Rest),Offend,List),
        form(Rest,List,Angle),
        !.

angle_size(Unk,Rest,Term,Coeff) :- 
        arg(1,Term,Arg),
        angle_size1(Unk,Rest,Arg,Coeff),
        !.

angle_size1(Unk,Rest,Arg,Coeff) :- 
        match(Arg,A+B),
        contains(Unk,A),
        freeof(Unk,B),
        angle_size1(Unk,Rest,A,Coeff),
        !.

angle_size1(Unk,Rest,Arg,Coeff) :- 
        match(Arg,Coeff*Rest),
        ok_number(Coeff),
        contains(Unk,Rest),
        !.

angle_size1(Unk,Rest,Other,1) :- 
        \+ ok_number(Other),
        contains(Unk,Other),
        match(Other,Rest),
        !.

 % Find the reduced term
 % First,see if offending set contains only cos & sin,or sec & tan,
 % or cot & cosec.If so eliminate (ie choose the other as reduced term) 
 % the one that occurs to only even powers,if this happens  
 % Flag indicates whether sim or solve is the top level 

anaz1(Eqn,Ang,Offend,R,X,Flag) :- 
        findtype_trig(Type,Offend),
        action(Type,R,Eqn,Ang,X,Flag),
        !.

 % Same case for hyperbolic functions  

hyper_find(Eqn,Unk,Offend,Term,A,Flag) :- 
        findtype_hyper(Type,Offend),
        action(Type,Term,Eqn,A,Unk,Flag),
        !.

hyper_find(_Eqn,_,_,e^A,A,_) :- !. %If first clause fails use e^A as reduced term

 % See if equation needs tan(R) as a reduced term because equation contains
 % the correct functions.

anaz1(Eqn,Ang,Offend,tan(Ang),X,_) :- tantype(Offend,Ang),taneqn(Eqn,X,Ang),!.

 % Otherwise,choose as reduced term the term that occurs most often 

anaz1(Eqn,Ang,Offend,R,_,_) :- 
        find_common(Offend,Eqn,R1,Ang),
        !,
        makenice(R1,R).

 % If no term occurs more than once,choose according to an order of niceness 

anaz1(_,Ang,Offend,R,_,_) :- anaz2(Ang,Offend,R),(R=tan(Ang) -> ! ;true).

 % If resulting equation can't be solved try tan(half_angle) method,when  
 % this method is applicable  

anaz1(_,Ang,Offend,tan(R),X,_) :- 
        maplist(angle_size(X,Rest),Offend,L1),
        ((match(Ang,M*Rest),ok_number(M));M=1),
        half_angle(M,L1,Ang,R,Rest),
        trace_press('\nTrying tan half-angle method\n',1),
        !.

% Check to see if tan(x/2) method might work  

half_angle(M,List,Angle,Angle,_) :- 
        eval(2*M,N),
        member(N,List),
        check_half_angle_check1(M,List),
        !.

half_angle(M,List,_,A1,Rest) :- 
        check_half_angle_check2(M,List),
        form2(M,Rest,A1),
        !.

 % Check to see if a term occurs more than once in the equation  

find_common(L1,Eqn,R,_Ang) :-  
        maplist(nocc(Eqn),L1,L2),
        great_el(L2,Ans),
        Ans>1,
        correspond(R,L1,L2,Ans),
        arg(1,R,x),
        !.

 % Check for sin_cos etc pairs    

findtype_trig(sin_cos,Offend) :- 
        memberchk_press(cos(X),Offend),
        memberchk_press(sin(X),Offend),
        check_cs(X,Offend),
        !.

findtype_trig(cosec_cot,Offend) :- 
        memberchk_press(cosec(X),Offend),
        memberchk_press(cot(X),Offend),
        check_cc(X,Offend),
        !.

findtype_trig(sec_tan,Offend) :- 
        memberchk_press(sec(X),Offend),
        memberchk_press(tan(X),Offend),
        check_st(X,Offend),
        !.

 % Hyperbolic cases 

findtype_hyper(sinh_cosh,Offend) :- 
        memberchk_press(cosh(X),Offend),
        memberchk_press(sinh(X),Offend),
        check_csh(X,Offend),
        !.

findtype_hyper(cosech_coth,Offend) :- 
        memberchk_press(cosech(X),Offend),
        memberchk_press(coth(X),Offend),
        check_cch(X,Offend),
        !.

findtype_hyper(sech_tanh,Offend) :- 
        memberchk_press(sech(X),Offend),
        memberchk_press(tanh(X),Offend),
        check_sth(X,Offend),
        !.

action(Type,R,Eqn,Ang,X,Flag) :- 
        parse2(Eqn,X,Offend),
        action1(Type,R,Offend,Ang,Flag),
        !.

 % If one of pair occurs only to even powers eliminate it    
action1(sin_cos,sin(A),Offend,A,_) :- 
        maplist(cosp(A),Offend,L1),
        check_even(L1),
        !.

action1(sin_cos,cos(A),_Offend,A,_) :-  !.

action1(sec_tan,tan(A),Offend,A,_) :- 
        maplist(secp(A),Offend,L1),
        check_even(L1),
        !.

action1(sec_tan,sec(A),_Offend,A,_) :- !.

action1(cosec_cot,cot(A),Offend,A,_) :- 
        maplist(cosecp(A),Offend,L1),
        check_even(L1),
        !.

action1(cosec_cot,cosec(A),_Offend,A,_) :- !.

 % Hyperbolic cases  
action1(sinh_cosh,sinh(A),Offend,A,_) :- 
        maplist(coshp(A),Offend,L1),
        check_even(L1),
        !.

action1(sinh_cosh,cosh(A),Offend,A,_) :- 
        maplist(sinhp(A),Offend,L1),
        check_even(L1),
        !.

action1(sinh_cosh,sinh(A),_,A,sim) :- !.  %Only for sim case

action1(sech_tanh,tanh(A),Offend,A,_) :- 
        maplist(sechp(A),Offend,L1),
        check_even(L1),
        !.

action1(sech_tanh,sech(A),Offend,A,_) :- 
        maplist(tanhp(A),Offend,L1),
        check_even(L1),
        !.

action1(sech_tanh,tanh(A),_,A,sim) :- !.  %Only for sim case

action1(cosech_coth,coth(A),Offend,A,_) :- 
        maplist(cosechp(A),Offend,L1),
        check_even(L1),
        !.

action1(cosech_coth,cosech(A),Offend,A,_) :- 
        maplist(cothp(A),Offend,L1),
        check_even(L1),
        !.

action1(cosech_coth,coth(A),_,A,sim) :- !.  %Only for sim case

 % Check for tan case
tantype([],_) :- !.
tantype([H|T],X) :- tantype1(H,X),!,tantype(T,X).

tantype1(tan(_),_) :- !.
tantype1(cot(_),_) :- !.
tantype1(sec(X),Y) :- match(X,Y),!.
tantype1(cosec(X),Y) :- match(X,Y),!.

taneqn(Eqn,X,Ang) :- parse2(Eqn,X,Offend),check_tan(Offend,Ang),!.

check_tan([],_) :- !.
check_tan([H|T],Ang) :- check_tan1(H,Ang),!,check_tan(T,Ang).

check_tan1(tan(_),_) :- !.
check_tan1(cot(_),_) :- !.
check_tan1(sec(Ang)^N,Ang1) :- integer(N),even(N),match(Ang,Ang1),!.
check_tan1(cosec(Ang)^N,Ang1) :- integer(N),even(N),match(Ang,Ang1),!.

 % Choose reduced term in order of niceness  

anaz2(Ang,Offend,sin(Ang)) :- 
        member(sin(Ang),Offend),
        member(cosec(Ang),Offend),
        !.

anaz2(Ang,Offend,cos(Ang)) :- 
        member(cos(Ang),Offend),
        member(sec(Ang),Offend),
        !.

anaz2(Ang,Offend,cos(Ang)) :- 
        member(cos(Ang),Offend),
        member(cos(X),Offend),
        diff(X,Ang),
        !.

anaz2(Ang,Offend,sin(Ang)) :- member(sin(Ang),Offend),!.

anaz2(Ang,Offend,cos(Ang)) :- member(cos(Ang),Offend),!.

anaz2(Ang,Offend,cos(Ang)) :- member(sec(Ang),Offend),!.

anaz2(Ang,Offend,sin(Ang)) :- member(cosec(Ang),Offend),!.

anaz2(Ang,Offend,sin(Ang)) :- some(sinfind,Offend),!.

anaz2(Ang,Offend,cos(Ang)) :- some(cosfind,Offend),!.

anaz2(Ang,Offend,sin(Ang)) :- some(cosecfind,Offend),!.

anaz2(Ang,Offend,cos(Ang)) :- some(secfind,Offend),!.

anaz2(Ang,_,tan(Ang)) :- !.


cs(X,sin(X)) :- !.
cs(X,cos(X)) :- !.
cc(X,cot(X)) :- !.
cc(X,cosec(X)) :- !.
st(X,sec(X)) :- !.
st(X,tan(X)) :- !.

 % Hyperbolic cases 
csh(X,sinh(X)) :- !.
csh(X,cosh(X)) :- !.
cch(X,coth(X)) :- !.
cch(X,cosech(X)) :- !.
sth(X,sech(X)) :- !.
sth(X,tanh(X)) :- !.

sinfind(sin(_)) :- !.
cosfind(cos(_)) :- !.
secfind(sec(_)) :- !.
cosecfind(cosec(_)) :- !.

 % Recognize powers of trig functions in the equation   
cosp(Ang,cos(Ang)^N,N) :- integer(N),!.
cosp(Ang,cos(Ang),1) :- !.
cosp(_,_,0) :- !.
secp(Ang,sec(Ang)^N,N) :- integer(N),!.
secp(Ang,sec(Ang),1) :- !.
secp(_,_,0) :- !.
cosecp(Ang,cosec(Ang)^N,N) :- integer(N),!.
cosecp(Ang,cosec(Ang),1) :- !.
cosecp(_,_,0) :- !.

 % Recognize powers of hyperbolic functions in the equation   
coshp(Ang,cosh(Ang)^N,N) :- integer(N),!.
coshp(Ang,cosh(Ang),1) :- !.
coshp(_,_,0) :- !.
sinhp(Ang,sinh(Ang)^N,N) :- integer(N),!.
sinhp(Ang,sinh(Ang),1) :- !.
sinhp(_,_,0) :- !.
sechp(Ang,sech(Ang)^N,N) :- integer(N),!.
sechp(Ang,sech(Ang),1) :- !.
sechp(_,_,0) :- !.
tanhp(Ang,tanh(Ang)^N,N) :- integer(N),!.
tanhp(Ang,tanh(Ang),1) :- !.
tanhp(_,_,0) :- !.
cosechp(Ang,cosech(Ang)^N,N) :- integer(N),!.
cosechp(Ang,cosech(Ang),1) :- !.
cosechp(_,_,0) :- !.
cothp(Ang,coth(Ang)^N,N) :- integer(N),!.
cothp(Ang,coth(Ang),1) :- !.
cothp(_,_,0) :- !.

makenice(cosec(X),sin(X)) :- !.
makenice(sec(X),cos(X)) :- !.
makenice(cot(X),tan(X)) :- !.
makenice(X,X) :- !.

 % expss(P,Q,X,T) expresses sin(Z) in terms of sin(X) where Z/X=Q/P 
 % expcs expresses cos(Z) in terms of sin(X) etc.    The 4
 % functions are more or less mutually recursive, but expcc does
 % not depend on the others, though they call it

expss(P,P,X,sin(X)) :- !.

expss(P,Q,X,2*sin(X)*(1-sin(X)^2)^(1/2)) :- eval(Q/P=:=2),!.

expss(P,Q,X,(3*sin(X)-4*sin(X)^3)) :- eval(Q/P=:=3),!.

 % Where Q/P is odd a simple series expansion can be applied
expss(P,Q,X,A) :- eval(Q/P,N),eval(N mod 2,1),!,sinexp(sin(X),N,0,A).

 % sin(Y) = sin((Y-3*X) + 3*X) = sin(3*X)*cos(Y-3*X) + cos(3*X)*sin(Y-3*X)  
 % We can now express each of these 4 terms in terms of sin(X) as
 % a recursive step. The 4 terms are A,B,C and D below.

expss(P,Q,X,(A*B+C*D)) :- 
        eval(3*P,P1),
        eval(Q-3,Q1),
        expss(P,P1,X,A),
        expcs(P,Q1,X,B),
        expcs(P,P1,X,C),
        expss(P,Q1,X,D),
        !.

 % Similarly for sin in terms of cos
expsc(P,P,X,(1-cos(X)^2)^(1/2)) :- !.

expsc(P,Q,X,2*cos(X)*(1-cos(X)^2)^(1/2)) :-eval(Q/P=:=2),!.

expsc(P,Q,X,(4*cos(X)^2-1)*(1-cos(X)^2)^(1/2)) :- eval(Q/P=:=3),!.

expsc(P,Q,X,(A*B+C*D)) :- 
        eval(3*P,P1),
        eval(Q-3,Q1),
        expsc(P,P1,X,A),
        expcc(P,Q1,X,B),
        expcc(P,P1,X,C),
        expsc(P,Q1,X,D),
        !.

 %  cos in terms of sin
expcs(P,P,X,(1-sin(X)^2)^(1/2)) :- !.

expcs(P,Q,X,(1-2*sin(X)^2)) :-eval(Q/P=:=2),!.

expcs(P,Q,X,(1-4*sin(X)^2)*(1-sin(X)^2)^(1/2)) :- eval(Q/P=:=3),!.

expcs(P,Q,X,(A*B-C*D)) :- 
        eval(3*P,P1),
        eval(Q-3,Q1),
        expcs(P,P1,X,A),
        expcs(P,Q1,X,B),
        expss(P,P1,X,C),
        expss(P,Q1,X,D),
        !.

 % Series exists for cos in terms of cos
expcc(P,Q,X,Y) :- eval(Q/P,N),cosexp(cos(X),N,0,Y),!.

 % Base case, series complete
cosexp(A,N,R,X) :- eval(2*R,R1),eval(R1+1,R2),(N=R1;N=R2),coeff1(A,N,R,X),!.

 % Recurse
cosexp(X1,N,R,X-(Y)) :- coeff1(X1,N,R,X),eval(R+1,R1),!,cosexp(X1,N,R1,Y).
 
 % Produce the coefficients for the series, very ugly

coeff1(Fang,N,R,X*(ZZ)) :- 
        fact(R,R1),
        eval(N-2*R-1,N1),
        eval(N-R-1,N2),
        eval(N1+1,N3),
        fact(N2,Z2),
        fact(N3,Z3),
        eval((2^N1*N*Z2)/(R1*Z3),X),
        form4(Fang,N3,ZZ),
        !.

 % The sin expansion for odd Q/P is very similar to cos cos series
sinexp(X,N,A,B*(Z)) :- eval((-1)^((N-1)/2),B),cosexp(X,N,A,Z),!.

 % Expand tan(n*x) in terms of tan(m*x)  (m < n)  
 % Tan produces a numerator and denominator series.

exptt(I,J,X,(Z)/(Y)) :- 
        eval(J/I,N),
        tanexp_num(tan(X),N,1,Z),
        tanexp_denom(tan(X),N,0,Y),
        !.

 % Obtain numerator
tanexp_num(A,N,R,X) :- eval(R+1,R1),(N=R1;N=R),coeff2(A,N,R,X),!.
tanexp_num(A,N,R,X-(Y)) :- 
        coeff2(A,N,R,X),
        eval(R+2,R1),
        !,
        tanexp_num(A,N,R1,Y).


 % Obtain the denominator
tanexp_denom(A,N,R,X) :- eval(R+1,R1),(N=R1;N=R),coeff2(A,N,R,X),!.
tanexp_denom(A,N,R,X-(Y)) :- 
        coeff2(A,N,R,X),
        eval(R+2,R1),
        !,
        tanexp_denom(A,N,R1,Y).

 % Different coefficients from the other series

coeff2(A,N,R,X*(ZZ)) :- calc_coeff(N,R,X),form4(A,R,ZZ),!.

calc_coeff(N,R,X) :- 
        fact(R,Rfact),
        fact(N,Nfact),
        eval(N-R,P),
        fact(P,Pfact),
        eval(Nfact/(Pfact*Rfact),X),
        !.

 % Modified checklists
check_cs(_,[]) :- !.
check_cs(X,[H|T]) :- cs(X,H),check_cs(X,T).


check_cc(_,[]) :- !.
check_cc(X,[H|T]) :- cc(X,H),check_cc(X,T).

check_st(_,[]) :- !.
check_st(X,[H|T]) :- st(X,H),check_st(X,T).

check_csh(_,[]) :- !.
check_csh(X,[H|T]) :- csh(X,H),check_csh(X,T).

check_cch(_,[]) :- !.
check_cch(X,[H|T]) :- cch(X,H),check_cch(X,T).

check_sth(_,[]) :- !.
check_sth(X,[H|T]) :- sth(X,H),check_sth(X,T).


check_half_angle_check1(_,[]) :- !.
check_half_angle_check1(A,[H|T]) :- 
        half_angle_check1(A,H),
        check_half_angle_check1(A,T).


check_half_angle_check2(_,[]) :- !.
check_half_angle_check2(A,[H|T]) :- 
        half_angle_check2(A,H),
        check_half_angle_check2(A,T).


check_even([]) :- !.
check_even([H|T]) :- even(H),check_even(T).
