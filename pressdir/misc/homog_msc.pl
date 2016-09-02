/* HOMOG.MSC : 

						Bernard Silver
						Updated: 2 September 82

*/

:- dynamic report/0.

% :- public
% 		absol/2,
% 		break/4,
% 		expcase1/5,
% 		expcase2/4,
% 		form/3,
% 		form1/3,
% 		form2/3,
% 		form4/3,
% 		genpolcase/3,
% 		great_el/2,
% 		half_angle_check1/2,
% 		half_angle_check2/2,
% 		laura/4,
% 		laura1/3,
% 		least_el/2,
% 		lessone/1,
% 		logocc/4,
% 		make_subl/3,
% 		moreone/1,
% 		neg22/1,
% 		nocc/3,
% 		onetest/2,
% 		parse2/3,
% 		powered/3,
% 		reduced_term/3,
% 		report_subs/2,
% 		signed/2,
% 		subs1/3.


 % Various functions for recognizing certain forms 

			%  The exponential case with all offending terms
			%  of the form a^f(x), a the same in all terms.
expcase1(A,B,X,A^Z,C) :- atom_num(A),match(Z,C*B+D),ok_number(C),freeof(X,D),!.
expcase1(A,B,X,A^Z,1) :- atom_num(A),match(Z,B+C),freeof(X,C),!.
expcase1(A,B,_X,A^Z,C) :- atom_num(A),match(Z,C*B),ok_number(C),!.
expcase1(A,B,_X,A^B,1).


			%  The other exponential case
expcase2(B,A^Y,set(A,Z)) :- ok_number(A),match(Y,Z*B+C),ok_number(Z),freeof(B,C),!.
expcase2(B,A^Y,set(A,1)) :- ok_number(A),match(Y,B+C),freeof(B,C),!.
expcase2(B,A^Y,set(A,Z)) :- ok_number(A),match(Y,Z*B),ok_number(Z),!.
expcase2(B,A^B,set(A,1)).

			%  Check is the tan(half-angle) method can be used
half_angle_check1(M,M) :- !.
half_angle_check1(M,N) :- eval(2*M,N),!.

half_angle_check2(M,M) :- !.

genpolcase(X,X,1) :- !.
genpolcase(X,X^N,N) :- !.

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

 % From the exponential case (expcase2), find the gcd of bases and exponents to
 % form the reduced term

coeff_exp(L,M,N) :- 
	get_members(L,L1,L2),
	rational_gcd_list(L1,M),			
	rational_gcd_list(L2,N),
	!.
 % Maplist for above
get_members([],[],[]) :- !.
get_members([set(A,B)|T],[A|X],[B|Y]) :- !,get_members(T,X,Y).

 % When the terms are being raised to powers the reduced term should
 % be the smallest if all terms are less than one, the largest
 % if they are all greater than one, otherwise unless they are
 % all the same (listtoset is a singleton) fail

onetest(K,A) :- checklist(moreone,K),least_el(K,A),!.
onetest(K,A) :- checklist(lessone,K),great_el(K,A),!.
onetest(K,A) :- listtoset(K,[A]),!.

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

 % This recognizes numeric expressions eg 3^(1/2),on which number fails 
numeric(X) :- wordsin(X,L),!,L=[].

atom_num(X) :- atomic(X),!.
atom_num(X) :- numeric(X),!.

 % Parser for trig method  
parse2(Exp,X,L) :- dl_parse2(Exp,X,L1-[]),!,listtoset(L1,L).

dl_parse2(A&B,X,L-L1) :- !,dl_parse2(A,X,L-L2),dl_parse2(B,X,L2-L1).
dl_parse2(A=B,X,L-L1) :- !,dl_parse2(A,X,L-L2),dl_parse2(B,X,L2-L1).
dl_parse2(A*B,X,L-L1) :- !,dl_parse2(A,X,L-L2),dl_parse2(B,X,L2-L1).
dl_parse2(A+B,X,L-L1) :- !,dl_parse2(A,X,L-L2),dl_parse2(B,X,L2-L1).
dl_parse2(A^N,_,[A^N|L]-L) :- integer(N),(trigf(A);hyperf(A)),!.
dl_parse2(A^N,X,L) :- ok_number(N),dl_parse2(A,X,L),!.
dl_parse2(A,X,L-L) :- freeof(X,A),!.
dl_parse2(A,_X,[A|L]-L) :- !.

 % Find the "smallest" term in the  offenders set 

reduced_term([Unk],Unk,_) :- !,fail.		%Unk can't be the reduced term
reduced_term([A],_Unk,A) :- !.
reduced_term(L,Unk,A) :- 
	extreme_term(L, <, A),  %  return the smallest
	!,
	A \= Unk.

 % Make a list of the rewrites found,and substitute them into 
 % the expression 
subs1(Exp,[],Exp) :- !.
subs1(Exp,[H|T],E1) :- subst(H,Exp,E2),!,subs1(E2,T,E1).

make_subl([],[],[]) :- !.
make_subl([X|R],[X|R1],R2) :- !,make_subl(R,R1,R2).
make_subl([Hd|R],[H1|R1],[Hd=H1|R2]) :- !,make_subl(R,R1,R2). 

 % List the rewrites used, if desired  

report_subs(X,List) :- 
	report,
	!,
	sublist(contains(X),List,New),
	trace_press('\nRewrites used are:\n',1),
	report_subs1(New).

report_subs(_,_) :- !.

report_subs1([]) :- !.
report_subs1([L=R|T]) :- trace_press('\n %t -> %t\n',[L,R],1),!,report_subs1(T).

 % Turn on the reporting 
report_on :- report,trace_press('\nReporting is already on! Nothing done\n',1),!.
report_on :- asserta((report :- !)),trace_press('\nReporting turned on\n',1),!.

 % Turn off reporting 
report_off :- report,retract((report :- !)),trace_press('\nReporting turned off\n',1),!.
report_off :- trace_press('\nReporting is not on! Nothing done\n',1),!.

report :- fail.

 % Find the smallest and largest elements of a list of numbers 
least_el([Hd],Hd) :- !.
least_el([Hd|Tl],Ans) :- least_el(Tl,Lwr),(eval(Hd < Lwr) -> Hd=Ans;Lwr=Ans),!.

great_el([Hd],Hd) :- !.
great_el([Hd|Tl],Ans) :- great_el(Tl,Hgr),(eval(Hd>Hgr) -> Hd=Ans;Hgr=Ans),!.

 % powered(A,B,C) if A^B=C,A not equal 1   
powered(1,_,_) :- !,fail.
powered(A,1,A) :- !.
powered(A,N,A^N) :- ok_number(N),!.
powered(A,B,C) :- ok_number(A),ok_number(C),eval(log(A,C),X),!,ok_number(X),B=X.

nocc(Eqn,A,N) :- occ(A,Eqn,N),!.

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

 % If all numbers on a list are negative then signed(list,-1),   
 % else signed(list,1)   
signed(L,-1) :- checklist(neg22,L),!.
signed(_,1) :- !.

 % Stupid name, but needed function for above checklist!
neg22(Num) :- eval(sign(Num)=:= (-1)),!.

/*  Old Code - now replaced by code in odds
 % Factorial function  
fact(X,Y) :- fact(X,1,Y).
fact(0,Acc,Acc) :- !.
fact(N,Acc,Ans) :- eval(N>0),eval(N-1,N1),eval(N*Acc,M),!,fact(N1,M,Ans).

 % Find the least common multiple of a set of integers  
lcm([A],A) :- !.
lcm([A,B|T],X) :- gcd(A,B,Z),eval((A*B)/Z,Y),lcm([Y|T],X),!.


 % Find the greatest common divisor of a list of integers 
gcd1([A],A) :- !.
gcd1([H|T],X) :- gcd1(T,Y),gcd(H,Y,X),!.

 % Find the greatest common divisor of a list of rationals  
rational_gcd_list(L,X) :- listtoset(L,L1),gcd3(L1,X),!.

gcd3([A],A) :- !.
gcd3([H|T],Y) :- 
	gcd3(T,X),
	eval(numer(H),H2),
	eval(denom(H),H1),
	gcd_calc(H2,H1,X,Y),
	!.

 % Do the calculations for rational gcd (gcd of a/b and c/d is found
 % by expressing both terms as functions of the lcm of the denominators
 % and taking the hcf of the resulting numerators.  Apply recursivly

gcd_calc(A,B,C,C) :- eval(A/B,C),!.
gcd_calc(A,B,X2,X3) :-
	eval(numer(X2),C),
	eval(denom(X2),D),
	lcm([B,D],Z), 
	eval((Z/B)*A,Z1),
	eval((Z/D)*C,Z2),
	gcd(Z1,Z2,Y),
	eval(Y/Z,X3),
	!.
*/
