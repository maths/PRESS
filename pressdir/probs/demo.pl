/* DEMO. : 

						Bernard Silver
						Updated: 30 June 82
*/     
	%  Problems for demonstration of Press

text(1) :- writef('\nThis problem comes from the London 1978 A level exam.\nWe are asked to find the value(s) of for which log(2,x) + 4.log(x,2) = 5.\n\n').

text(2) :-
	writef('\nThis problem is from the A.E.B. A level exam of 1971.\nWe are required to find the value(s) of x such that cos(x) + 2.cos(2.x) + cos(3.x) = 0.\n\n').

text(3) :-
	writef('\nThis problem is from the A.E.B. 1971 A level paper.\nThe question asks for the value(s) of x which satisfy 4^x - 2^(x+1) - 3 = 0.\n\n').

text(4) :-
	writef('\nThis question demonstrates the basic methods of PRESS.\nThe problem is to find the value(s) of x that satisfy log(e,x+1) + log(e,x-1) = 3.\n\n').

basic :- example4.

example1 :- text(1),demo1,ttynl.

example2 :- text(2),demo2,ttynl.

example3 :- text(3),demo3,ttynl.

example4 :- text(4),demo4,ttynl.

	% The questions	

demo1 :- solve(log(2,x) + 4*log(x,2) = 5).  		%lon(15)

demo2 :- solve(cos(x) + 2*cos(2*x) + cos(3*x) = 0).	%aeb(7)

demo3:- solve(4^x - 2^(x+1) - 3 = 0).			%aeb(6)

demo4 :- solve(log(e,x+1) + log(e,x-1) = 3). %logeqn


