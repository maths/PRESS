%   File   : HOMOG
%   Author : Bernard Silver
%   Updated: 7 October 1984
%   Purpose: Homogenization Code for LP

% Top Level of Homogenization proper 

homog(Eqn,Unk,Offend,Homeqn) :- 
	findtype(Type,Offend),
	anaz(Type,Eqn,Unk,Offend,Term),
	perform_rewrites(Eqn,Term,Offend,Homeqn,Unk,Type).



 % Rewrite the offenders set and obtain new Homogenized equation
perform_rewrites(Eqn,Term,Offend,Homeqn,Unk,Type) :-
	rew(Term,Offend,Sub,Unk,Type),
	subs1(Eqn,Sub,Homeqn).

 
% Try to choose reduced term . Arguments of anaz are
% Type of offenders set, Equation, the Unknown, the offenders set
% the reduced term.

 % Trig case, find the gcd of all angles that occur, then choose functor

anaz(trig,Eqn,Unk,Offend,Term) :- 
	findangle(Unk,Offend,Angle),!,
	anaz1(Eqn,Angle,Offend,Term,Unk,_).


%  Normal log case dealing with terms like log(x,4) and log(2,x) in the 
%  offenders set.

anaz(log(_),_,Unk,Offend,Term) :- 
	map_laura(Offend,NewList,Arg2,Unk),
	onetest(NewList,Arg1),
	logocc(Arg1,Arg2,X,Offend).

map_laura([],[],_,_) :- !.
map_laura([H|T],[H1|T1],Arg,Unk) :-
	laura(Arg,Unk,H,H1),
	!,
	map_laura(T,T1,Arg,Unk).

%  Other log case where the logs are converted to base 10.

anaz(log(10),_,Unk,Offend,log(10,Term)) :- 
	check_laura1(Unk,Term,Offend),
	!.

check_laura1(_,_,[]) :- !.
check_laura1(Unk,Term,[H|T]) :-
	laura1(Unk,Term,H),
	!,
	check_laura1(Unk,Term,T).

% Choose reduced_term  using simplicity metric
  
anaz(_,_,Unk,Offend,T) :- 
	 reduced_term(Offend,Unk,T).


 %   Find gcd of angles in offending set 
findangle(Unk,Offend,Angle) :- 
	map_anglesize(Offend,List,Unk,Rest),
	form(Rest,List,Angle),
	!.

map_anglesize([],[],_,_) :- !.
map_anglesize([H|T],[H1|T1],Unk,Rest) :-
	angle_size(Unk,Rest,H,H1),
	!,
	map_anglesize(T,T1,Unk,Rest).

angle_size(Unk,Rest,Term,Coeff) :- 
	arg(1,Term,Arg),
	angle_size1(Unk,Rest,Arg,Coeff),
	!.

angle_size1(Unk,Rest,Arg,Coeff) :- 
	match(Arg,Coeff*Rest),
	ok_number(Coeff),
	contains(Unk,Rest),
	!.

angle_size1(Unk,Rest,Other,1) :- 
	not ok_number(Other),
	contains(Unk,Other),
	match(Other,Rest),
	!.

 % Find the reduced term
 % First,see if offending set contains only cos & sin,or sec & tan,
 % or cot & cosec.  If so eliminate (ie choose the other as reduced term) 
 % the one that occurs to only even powers,if this happens  
 % Flag indicates whether sim or solve is the top level 

anaz1(Eqn,Ang,Offend,R,X,Flag) :- 
	findtype_trig(Type,Offend),
	action(Type,R,Eqn,Ang,X,Flag),
	!.

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
	map_anglesize(Offend,L1,X,Rest),
	((match(Ang,M*Rest),ok_number(M));M=1),
	half_angle(M,L1,Ang,R,Rest),
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

find_common(L1,Eqn,R,Ang) :-  
	map_occ(L1,L2,Eqn),
	great_el(L2,Ans),
	Ans>1,
	correspond(R,L1,L2,Ans),
	arg(1,R,x),
	!.

map_occ([],[],_) :- !.
map_occ([H|T],[H1|T1],Eqn) :-
	occ(H,Eqn,H1),
	!,
	map_occ(T,T1,Eqn).

 % Check for sin_cos etc pairs    

findtype_trig(sin_cos,Offend) :- 
	memberchk(cos(X),Offend),
	memberchk(sin(X),Offend),
	check_cs(X,Offend),
	!.

findtype_trig(cosec_cot,Offend) :- 
	memberchk(cosec(X),Offend),
	memberchk(cot(X),Offend),
	check_cc(X,Offend),
	!.

findtype_trig(sec_tan,Offend) :- 
	memberchk(sec(X),Offend),
	memberchk(tan(X),Offend),
	check_st(X,Offend),
	!.


action(Type,R,Eqn,Ang,X,Flag) :- 
	parse2(Eqn,X,Offend),
	action1(Type,R,Offend,Ang,Flag),
	!.

 % If one of pair occurs only to even powers eliminate it    
action1(sin_cos,sin(A),Offend,A,_) :- 
	map_cosp(Offend,L1,A),
	check_even(L1),
	!.

action1(sin_cos,cos(A),Offend,A,_) :-  !.

action1(sec_tan,tan(A),Offend,A,_) :- 
	map_secp(Offend,L1,A),
	check_even(L1),
	!.

action1(sec_tan,sec(A),Offend,A,_) :- !.

action1(cosec_cot,cot(A),Offend,A,_) :- 
	map_cosecp(Offend,L1,A),
	check_even(L1),
	!.

action1(cosec_cot,cosec(A),Offend,A,_) :- !.

map_cosp([],[],_) :- !.
map_cosp([H|T],[H1|T1],A) :-
	cosp(A,H,H1),
	!,
	map_cosp(T,T1,A).

map_secp([],[],_) :- !.
map_secp([H|T],[H1|T1],A) :-
	secp(A,H,H1),
	!,
	map_secp(T,T1,A).

map_cosecp([],[],_) :- !.
map_cosecp([H|T],[H1|T1],A) :-
	cosecp(A,H,H1),
	!,
	map_cosecp(T,T1,A).



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

fact(X,Y) :-
	fact(X,Y,1).

fact(0,X,X).
fact(N,Ans,Acc) :-
	eval(N*Acc,NewAcc),
	eval(N-1,M),
	!,
	fact(M,Ans,NewAcc).



			%  Check is the tan(half-angle) method can be used
half_angle_check1(M,M) :- !.
half_angle_check1(M,N) :- eval(2*M,N),!.

half_angle_check2(M,M) :- !.

			%  Standard log case
laura(B,X,log(A,B),A) :- freeof(X,A),!.
laura(A,X,log(A,B),B) :- freeof(X,B),!.

			%  Convert to log base 10 case
laura1(Unk,Term,log(A,Term)) :-
	ok_number(A),
	contains(Unk,Term),
	!.
	
laura1(Unk,Term,log(Term,A)) :-
	ok_number(A),
	contains(Unk,Term),
	!.

 % When the terms are being raised to powers the reduced term should
 % be the smallest if all terms are less than one, the largest
 % if they are all greater than one, otherwise unless they are
 % all the same (listtoset is a singleton) fail

onetest(K,A) :- check_moreone(K),least_el(K,A),!.
onetest(K,A) :- check_lessone(K),great_el(K,A),!.
onetest(K,A) :- listtoset(K,[A]),!.

check_moreone([]) :- !.
check_moreone([H|T]) :-
	moreone(H),
	!,
	check_moreone(T).
check_lessone([]) :- !.
check_lessone([H|T]) :-
	lessone(H),
	!,
	check_lessone(T).

 % Choosing the reduced term in the log case, we choose it to have
 % the term containing the unknown as its second argument, whether or
 % not this log term occurred in the original equation
logocc(A,B,log(A,B),L) :- member(log(A,B),L),!.
logocc(A,B,log(B,A),L) :- member(log(B,A),L),!.

 % These form functions put terms together prettily,so 1*A is A for example

form(Unk,K,Z) :-rational_gcd_list(K,Gcd),absol(Gcd,Gcd1),!,form1(Unk,Gcd1,Z).

form1(Unk,A,Res) :- tidy(A*Unk,Res),!.

form2(M,Rest,Res) :- !,tidy(Rest*M/2,Res).

form4(_,0,1) :- !.
form4(A,1,A) :- !.
form4(A,N,A^N) :- !.


 % Parser for trig method  
parse2(Exp,X,L) :- dl_parse2(Exp,X,L1-[]),!,listtoset(L1,L).

dl_parse2(A=B,X,L-L1) :- !,dl_parse2(A,X,L-L2),dl_parse2(B,X,L2-L1).
dl_parse2(A*B,X,L-L1) :- !,dl_parse2(A,X,L-L2),dl_parse2(B,X,L2-L1).
dl_parse2(A+B,X,L-L1) :- !,dl_parse2(A,X,L-L2),dl_parse2(B,X,L2-L1).
dl_parse2(A^N,_,[A^N|L]-L) :- integer(N),trigf(A),!.
dl_parse2(A^N,X,L) :- ok_number(N),dl_parse2(A,X,L),!.
dl_parse2(A,X,L-L) :- freeof(X,A),!.
dl_parse2(A,X,[A|L]-L) :- !.

 % Find the r.t. from the  offenders set 

reduced_term([Unk],Unk,_) :- !,fail.		%Unk can't be the reduced term
reduced_term([A],Unk,A) :- !.
reduced_term(L,Unk,A) :- 
	member(A,L),  %  return the smallest
	A \= Unk.


 % Find the smallest and largest elements of a list of numbers 
least_el([Hd],Hd) :- !.
least_el([Hd|Tl],Ans) :- least_el(Tl,Lwr),(eval(Hd < Lwr) -> Hd=Ans;Lwr=Ans),!.

lessone(A) :- ok_number(A),eval(A < 1),!.

moreone(A) :- ok_number(A),eval(A > 1),!.

 % Absolute value 
absol(X,X1) :- eval(sign(X)*X,X1),!.

 % Given terms A and B break(A,B,I,J) finds I and J 
 % so that A=I*Y,and B=J*Y,if this is possible   
break(A,B,1,1) :- match(A,B),!.
break(A,B,1,C) :- ok_number(A),ok_number(B),eval(B/A,C),!.
break(A,B,1,J1) :- match(A,I*Y),ok_number(I),match(B,Y*J),ok_number(J),eval(J/I,J1),!.
break(A,B,1,J) :- match(B,J*A),ok_number(J),!.
break(A,B,J,1) :- match(A,J*B),ok_number(J),!.


/* Try to rewrite each of the terms in the offending set as a 
    function of the reduced term */

rew(X,L,Subs,Unk,Type) :-  newtype(Type,New),
	map_rew1(L,L1,New,X,Unk),
	make_subl(L,L1,Subs),
	!.

map_rew1([],[],_,_,_) :- !.
map_rew1([H|T],[H1|T1],New,X,Unk) :-
	rew1(New,X,Unk,H,H1),
	!,
	map_rew1(T,T1,New,X,Unk).

			%  Kludge for stopping recursive calls of rew-rule
			%  in mixed case, and for getting the log case right
newtype(mixed,_) :- !.
newtype(log(X),log) :- X \== 10.
newtype(C,C) :- !.

rew1(_,X,_,X,X) :- !.
rew1(Type,A^B,Unk,Old,New) :- !,rew_rule(Type,A^B,Old,New,Unk).
rew1(Type,X,Unk,A^B,C^D) :- rew1(Type,X,Unk,A,C),rew1(Type,X,Unk,B,D),!.
rew1(Type,X,Unk,Old,New) :- rew_rule(Type,X,Old,New,Unk),!.

/* rew_rule(Type,Term1,Term2,Exp,Unk) gives Exp as a rewrite of Term2 in terms */
/* of Term1,where Unk is the  unknown, and the rule is for type Type */



:- dynamic rew_rule/5.

/* Special cases   */
rew_rule(_,X,Y,X,_) :- match(X,Y),!.

rew_rule(_,_,Y,Y,Unk) :- freeof(Unk,Y),!.

/* Trignometric Rewrite rules */
rew_rule(trig,sin(X),sin(Z),V*cos(C) + V1*sin(C),U) :- 
	match(Z,B + C),
	contains(U,B),
	freeof(U,C),
	rew_rule(trig,sin(X),sin(B),V,U),
	rew_rule(trig,sin(X),cos(B),V1,U),
	!.

rew_rule(trig,sin(X),cos(Z),V*cos(C) - V1*sin(C),U) :-
	match(Z,B + C),
	contains(U,B),
	freeof(U,C),
	rew_rule(trig,sin(X),sin(B),V1,U),
	rew_rule(trig,sin(X),cos(B),V,U),
	!.

rew_rule(trig,cos(X),sin(Z),V*cos(C) + V1*sin(C),U) :-
	match(Z,B + C),
	contains(U,B),
	freeof(U,C),
	rew_rule(trig,cos(X),sin(B),V,U),
	rew_rule(trig,cos(X),cos(B),V1,U),
	!.

rew_rule(trig,cos(X),cos(Z),V*cos(C) - V1*sin(C),U) :-
	match(Z,B + C),
	contains(U,B),
	freeof(U,C),
	rew_rule(trig,cos(X),cos(B),V,U),
	rew_rule(trig,cos(X),sin(B),V1,U),
	!.


rew_rule(trig,sin(X),cos(Z),V,_) :-
	break(X,Z,P,Q),
	absol(Q,Q1),
	expcs(P,Q1,X,V),
	!.

rew_rule(trig,sin(X),sin(Z),I*(V),_) :-
	break(X,Z,P,Q),
	absol(Q,Q1),
	eval(sign(Q),I),
	expss(P,Q1,X,V),
	!.

rew_rule(trig,cos(X),sin(Z),I*(V),_) :-
	break(X,Z,P,Q),
	absol(Q,Q1),
	eval(sign(Q),I),
	expsc(P,Q1,X,V),
	!.

rew_rule(trig,cos(X),cos(Z),V,_) :-
	break(X,Z,P,Q),
	absol(Q,Q1),
	expcc(P,Q1,X,V),
	!.

rew_rule(trig,tan(X),sec(X),(1+tan(X)^2)^(1/2),_) :- !.

rew_rule(trig,sec(X),tan(X),(sec(X)^2-1)^(1/2),_) :- !.

rew_rule(trig,cot(X),cosec(X),(1+cot(X)^2)^(1/2),_) :- !.

rew_rule(trig,cosec(X),cot(X),(cosec(X)^2-1)^(1/2),_) :- !.

rew_rule(trig,tan(X),tan(Z),(V + tan(C))/(1 - tan(C)*V),U) :-
	match(Z,B + C),
	contains(U,B),
	freeof(U,C),
	rew_rule(trig,tan(X),tan(B),V,U),
	!.

rew_rule(trig,tan(X),tan(Z),I*(V),_) :-
	break(X,Z,P,Q),
	absol(Q,Q1),
	eval(sign(Q),I),
	exptt(P,Q1,X,V),
	!.

rew_rule(trig,tan(X),cosec(X),(1+tan(X)^2)^(1/2)/tan(X),_) :- !.

rew_rule(trig,tan(X),sin(X),tan(X)/(1+tan(X)^2)^(1/2),_) :- !.

rew_rule(trig,tan(X),cos(X),1/(1+tan(X)^2)^(1/2),_) :- !.

/* Tan half-angle Rewrite rules */

rew_rule(trig,tan(X),sin(Z),2*tan(X)*(1+tan(X)^2)^(-1),_) :-
	break(X,Z,P,Q),
	eval(Q/P=:=2),
	!.

rew_rule(trig,tan(X),cos(Z),(1-tan(X)^2)*(1+tan(X)^2)^(-1),_) :-
	break(X,Z,P,Q),
	eval(Q/P=:=2),
	!.
	

/* Reciprocal function Rewrite rules  */
rew_rule(trig,X,tan(Z),A*B^ -1,Unk) :-
	rew_rule(trig,X,sin(Z),A,Unk),
	rew_rule(trig,X,cos(Z),B,Unk),
	!.

rew_rule(trig,A,sec(Z),(B)^ -1,Unk) :-
	rew_rule(trig,A,cos(Z),B,Unk),
	!.

rew_rule(trig,A,cosec(Z),(B)^ -1,Unk) :-
	rew_rule(trig,A,sin(Z),B,Unk),
	!.

rew_rule(trig,A,cot(Z),(B)^ -1,Unk) :-
	rew_rule(trig,A,tan(Z),B,Unk),
	!.


/* Logarithmic Rewrite rules  */
rew_rule(log,log(X,Y),log(Y,X),log(X,Y)^ -1,_) :- !.
 
rew_rule(log,log(X,Y),log(Z,Y),N1*log(X,Y),_) :- 
	powered(X,N,Z),
	!,
	eval(1/N,N1).

rew_rule(log,log(X,Y),log(Y,Z),N*log(X,Y)^ -1,_) :- powered(X,N,Z),!.

rew_rule(log,log(X,Y),log(X,Z),N1*log(X,Y),_) :- 
	powered(Y,N,Z),
	!,
	eval(1/N,N1).


rew_rule(log,log(X,Y),log(Z,X),N*log(X,Y)^ -1,_) :- powered(Y,N,Z),!.

			%  Reduced term is log base 10
rew_rule(log(10),log(10,X),log(X,10),log(10,X)^ -1,_) :- !.

rew_rule(log(10),log(10,X),log(A,X),Term,Unk) :-
	ok_number(A),
	tidy(log(10,X)/log(10,A),Term),
	!.

rew_rule(log(10),log(10,X),log(X,A),Term,Unk) :-
	ok_number(A),
	tidy(log(10,A)*(log(10,X)^ -1),Term),
	!.


rational_gcd_list([H|T],Gcd) :- 
	eval(numer(H),N),
	eval(denom(H),D),
	rgl(T,N,D,Gcd).

rgl([],N,D,Gcd) :- eval(N/D,Gcd).
rgl([H|T],N,D,Gcd) :-
	eval(numer(H),N1),
	eval(denom(H),D1),
	eval(gcd(N*D1,D*N1),G),
	eval(D*D1,Dnew),
	rgl(T,G,Dnew,Gcd).


