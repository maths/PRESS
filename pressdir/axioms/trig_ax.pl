%   File   : TRIG.AX
%   Author : Bernard Silver
%   Updated: 11 February 1984
%   Purpose: Axioms for Trig Method, needs TRIG.NEW, TREW



trig_rule(cos(A)+cos(B),2*cos(C)*cos(D),trig_tidy(A,B,C,D)).

trig_rule(sin(A)+sin(B),2*sin(C)*cos(D),trig_tidy(A,B,C,D)).

trig_rule(sin(A)+ -1*sin(B),2*cos(C)*sin(D),trig_tidy(A,B,C,D)).

trig_rule(cos(A)+ -1*cos(B),2*sin(C)*sin(D),trig_tidy(B,A,C,D)).

trig_rule(sin(A+B), sin(A)*cos(B) + sin(B)*cos(A),true).

trig_rule(sin(A+ -1*B), sin(A)*cos(B) + -1*sin(B)*cos(A),true).

trig_rule(cos(A+B), cos(A)*cos(B) + -1*sin(B)*sin(A),true).

trig_rule(cos(A+ -1*B), cos(A)*cos(B) + sin(B)*sin(A),true).

trig_rule(cos(A),cos(A1),negative_angle(A,A1)).

trig_rule(sin(A),sin(A1),negative_angle(A,A1)).

 % Add angles, forming half sums and differences, and tidy them
trig_tidy(A,B,HalfS,HalfD) :-
	tidy((A+B)/2,C1),
	tidy((A-B)/2,D1),
	simplify(C1,HalfS),
	simplify(D1,HalfD),
	!.



 % Angle is negative, make it positive, multiplying term by -1 is function 
 % is sin.
negative_angle(A1*B1,C) :-
	match(A1*B1,A*Rest),
	number(A),
	eval(A<0),
	!,
	eval(-1*A,PosA),
	tidy(PosA*Rest,C),
	!.

negative_angle(-A,A) :- !.

