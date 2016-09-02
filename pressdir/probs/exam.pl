/* EXAM. : 

						Bernard Silver
						Updated: 15 August 1984
*/

 %  A-Level questions gathered together by Bernard 21.4.81 

:- dynamic present/1.

:- assert((present(exam))).

:- no_style_check(single_var).

		 % AEB exam questions   

 %  June 1971  Paper2    			%solved
aeb(1) :- solve(sec(2*x) + tan(2*x) = 3).    
 % Show first that sec(2x)+tan(2x)=(1+tan(x))/(1-tan(x)) 

aeb(2) :- solve(3*sech(x)^2 + 4*tanh(x) + 1 = 0).	%solved        

 %  Nov 1971 Paper1 

aeb(3) :- simsolve(3*x^2 + 15*x*y - 56*y^2 + 56 = 0 &
	2*x^2 + 9*x*y - 33*y^2 + 28 = 0,[x,y],X).
 % Told to solve by eliminating the constant terms  

aeb(4) :- solve(1 - 3*cos(x)^2 = 5*sin(x)).     	%solved

aeb(5) :- solve(4^(2*x+1) * 5^(x-2) = 6^(1-x)).	 	%solved

aeb(6) :- solve(4^x - 2^(x+1) - 3 = 0).			%solved

 % Paper2 

aeb(7) :- solve(cos(x) + 2*cos(2*x) + cos(3*x) = 0).	%solved

 % Nov 1972 Paper1 

aeb(8) :- simsolve(x^3 = 9*y & 4^(2*x) = 3^(x+y),[x,y],X).	%solved
 % Find the non-zero values of x & y 

aeb(9) :- solve(2*sin(x) + cos(x) = 1).			%solved

aeb(10) :- solve(2*sin(x) + cos(2*x) = 1).		%solved

 % Paper2 

aeb(11) :- solve(25*cos(x)^2 - 4*sin(x)^2 - 20*cos(x) - 8*sin(x) = 0).
 % First show that left-hand side can be expressed as the difference of
 % two squares  

 % June 1973 Paper1 

aeb(12) :- solve(9^(3*x^2) = 27^(15-x)).		%solved

aeb(13) :- solve(log(e,2*x-5) + log(e,x-3) = 2*log(e,2*x-1) - log(e,2)). %solved

aeb(14) :- solve(cos(6*x) + sin(6*x) + cos(4*x) + sin(4*x) = 0). %solved

aeb(15) :- solve(cos(2*x) + 3*sin(x) + 1 = 0).		%solved

 % Paper2 

aeb(16) :- solve((cot(2*x) + cosec(2*x))^2 = sec(2*x)).
 % Show first that (cot(x) + cosec(x))^2 = (1 + cos(x))/(1 - cos(x)) 

aeb(17)  :- solve(sin(2*x) + sin(3*x) + sin(5*x) = 0).

aeb(18) :- sim(cosh(x) - 3*sinh(y) = 0 &
	2*sinh(x) + 6*cosh(y) = 5,[x,y],X). %solved with sim

 % June 1974 Paper1 

aeb(19) :- solve(3*sin(x) + 4*cos(x) = 1).		%solved

 % Nov 1974 Paper1 

aeb(20) :- solve(sin(150-x) = 2*sin(x-30)).		%solved


aeb(21) :- solve(5*cos(2*x) - 2*sin(2*x) =  2).		%solved

 % 20 & 21  make one complete question  

 %      June 1975 Paper1 

aeb(22)  :- simsolve(log(16,x*y) = 7/2 & 
	log(4,x)*log(4,y) = -8,[x,y],X).
 % Show first that log(16,x*y) = 1/2*log(4,x) + 1/2*log(4,y) 

aeb(23) :- solve(3*cos(x)^2 + 5*sin(x) - 1 = 0).		%solved

 % 	Paper2	

aeb(24) :- solve((1-tan(x))*(1+sin(2*x)) = 1 + tan(x)).       	%solved

aeb(25) :- sim(2*cosh(y) - 7*sinh(x) = 3 & 
	cosh(y) - 3*sinh(x)^2 = 2,[x,y],X). %solved with sim

 % 	Nov 1975 Paper1 

aeb(26) :- solve(log(x,8) + log(8,x) = 13/6).		%solved

 % Paper2 

aeb(27) :- solve(sin(x) - sin(4*x) + sin(7*x) = 0). %solved

aeb(28) :-  solve(sin(3*x) = 2*cos(2*x)).
 % Verify that x=30 is a solution.  Find general solution 

 % June 1976 Paper1 

aeb(29) :-  simsolve(log(y,x) = 2 &
	 log(2,x) + log(2,y) = 3,[x,y],X).  %solved

aeb(30) :-  solve(7*sin(x) - 24*cos(x) = 15).		%solved

aeb(31) :-  sim(cos(x) + cos(y) = 1 & 
	sec(x) + sec(y) = 4,[x,y],X). %solved with sim

 % Paper2 

aeb(32) :-  solve(cos(x) + cos(3*x) + cos(5*x) = 0).		%solved

 % Nov 1976 Paper1  

aeb(33) :-  simsolve(a*log(4,128) - b*log(8,2) = 6    &
	log(2,a) + (1/3)*log(2,b^3) = 2*log(4,6),[a,b],X). %solved

aeb(34) :-  solve(2*sec(x) + 3*sin(x) = 4*cos(x)).	%solved

 % Paper2 

aeb(35) :-  solve(x^3 - 9*x + 4 = 0).
 % aeb(35) gives substitution x = 2*3^(1/2)*cos(y)  

 % June 1978 S paper 
 
aeb(36) :-  solve(cos(5*x) = cos(2*x)). %solved


/* June 1977 Paper 1 */

aeb(37) :- solve(3*cos(x)+6*sin(x)=1). % solved

/* Paper 2 */

aeb(38) :- solve(tan(x)=tan(4*x)). % solved

aeb(39) :- solve(2*cosh(x)+sinh(x)=2). % solved

/* Nov 1977 Paper 1*/

aeb(40) :- solve(log(4,x)+log(2,x)=6). % solved
  % Show first that log(a,n)=log(b,n)/log(b,a).

aeb(41) :- sim(8^x=2^(x+y) & log(x,y) = 3). % solved

aeb(42) :- solve(cos(2*x)-2*sin(2*x)=2). % solved

aeb(43) :- solve(sin(x) - sin(2*x) + sin(3*x)=0). % solved

 /* Paper 2 */

aeb(44) :- sim(2*cos(x-y)=1 & tan(x-y) = 1).

/* June 1978 Paper 1 */

aeb(45) :- solve(3*log(8,x) - 8*log(x,2)=8). % solved

aeb(46) :- solve(6*sin(x)^2 - cos(x) = 5). % solved

aeb(47) :- solve(tan(2*x)+4*cot(x) = 0). % solved

/* November 1978 Paper 1 */

aeb(48) :- solve(sec(x)^2 = 3+tan(x)). % solved

aeb(49) :- solve(sin(2*x)=4*cos(2*x)-1). % solved


/* June 1979  Paper 1 */

aeb(50) :- solve(log(3,(9*x-2)) = 2+2*log(3,x)). % solved

aeb(51) :- solve(6*(3^(2*x)) - 5*3^x + 1 = 0). % solved

/* Paper 2 */

aeb(52) :- solve(sin(x)+sin(2*x)+sin(3*x)=sin(x)*sin(2*x)).

aeb(53) :- sim(2*sinh(x)-cosh(y) = 0 & 2*cosh(x) + sinh(y) = 2). % solved

/* Nov 1979 Paper 1*/

aeb(54) :- solve(5^(2*x) - 9*5^x - 10 = 0). % solved

aeb(55) :- solve((10/98)^x = 15/10*10^947). % solved

aeb(56) :- solve(5*sin(x)^2 + sin(x)*cos(x) = 3). % solved

/* Paper 2 */

aeb(57) :- solve(sin(x) + cos(x)*cos(2*x)=cos(2*x)*cos(3*x)).

/* June 1980 Paper 1 */

aeb(58) :- sim(cos(a)+cos(b)=2*k/3 & cos(a)*cos(b)= - k^2/3,[a,b]). % solved

/* Paper 2 */

aeb(59) :- solve(2*3^(1/2)*sin(x)^2 -sin(2*x) = 0). % solved

/* Nov 1980 Paper 1*/

aeb(60) :- sim(log(2,x-5*y + 4)=0&log(2,x+1)-1=2*log(2,y)). % solved

/* Paper 2 */

aeb(61) :- solve(sin(5*x) - sin(3*x) = sin(4*x)-sin(2*x)).

/* June 1981 Paper 1 */

aeb(62) :- sim(log(10,x+y)=1 & log(2,x) + 2*log(4,y) = 4).
% Show first that second equation implies that xy = 16

aeb(63) :- solve(sin(x)-2*cos(x) = 1). % solved

/* Nov 1981 Paper 1 */

aeb(64) :- solve(e^(3*x) - 11*e^x + 14 = 0). % solved

aeb(65) :- solve(4*sin(2*x) + 3*cos(2*x) = 249/100). % solved

aeb(66) :- solve(cos(x)-cos(2*x)=1/2). % solved
% Show first that
% 2*cos(x)*cos(x/2) - 2*cos(2*x)*cos(x/2) = cos(x/2) - cos(5*x/2)
% Hence show that pi/5 and 3pi/5 are roots 

/* June 1982 Paper 1 */

aeb(67) :- sim(e^x + 3*e^y = 3 & e^(2*x) - 9*e^(2*y) = 6). % solved

aeb(68) :- solve(arcsin(x) - arccos(x) = arcsin(1-x)). % solved
% Show first that sin(arcsin(x) - arccos(x)) = 2*x^2 - 1

aeb(69) :- solve(sec(x)^2 - 3*tan(x) - 5 = 0). % solved

aeb(70) :- solve(cos(4*x) + cos(2*x) - sin(4*x) + sin(2*x) = 0). % solved

/* Nov 1982 Paper 1 */

aeb(71) :- solve(log(3,x)=log(x,2)). % solved

aeb(72) :- solve(6^(3-4*x)*4^(x+4)=2). % solved
% Solve both the above in terms of p and q, where p is log(10,2) and
% q is log(10,3)

aeb(73) :- solve(8*sin(x) + cos(x)=4). % solved
% Ask to find values of tan(x/2) and cos(x)

/* Paper 2 */

aeb(74) :- solve(2*cosh(x) + sinh(x) = 2). % solved

aeb(75) :- solve(sec(x) + cosec(x) = 2*2^(1/2)).

/* June 1983 Paper 1 */

aeb(76) :- solve(4*log(3,x) = 9*log(x,3)). % solved

aeb(77) :- solve(10^(y-5)= 5^(y+2),y). % solved

aeb(78) :- solve(sin(x) + 2*cos(x) = 0). % solved

aeb(79) :- solve(sin(x) + 2*cos(x) = 1). % solved

aeb(80) :- solve(arctan(x) + arctan(2*x) = 45). % solved

/* Nov 1983 Paper 1 */

aeb(81) :- solve(sec(x/2)^2 - 2*tan(x/2)^2 = 0). % solved

/* June 1984 Paper 1 */

aeb(82) :- solve(x^3 - 7*p^2*x + 6*p^3 = 0).

aeb(83) :- solve(4*sec(x)^3 - 7*sec(x) + 3 = 0). % solved
% Solve 82 in terms of p using factor theorem or otherwise, hence solve 83

aeb(84) :- solve(sin(x) + cos(x) = 1/2). % solved

aeb(85) :- solve(10*sin(2*x + 26)*cos(2*x - 26) = 1).

 		 % London questions   

 % Jan 1976 Paper1  

lon(1) :-  solve(10^(x - 3) = 2^(10 + x)).		%solved

lon(2) :-  solve(cot(2*x) = 2 + cot(x)).		%solved

lon(3) :-  solve(cos(3*x) - 3*cos(x) = cos(2*x) + 1).		%solved

 % Paper3 

lon(4) :-  solve(9*cosh(x) - 6*sinh(x) = 7).		%solved

 % June 1976 Paper1 

lon(5) :-  solve(sin(x) + sin(2*x) = sin(3*x)).		%solved

lon(6) :-  solve(2*tan(x) + sec(2*x) = 2*tan(2*x)).
 % Whole question 


lon(7) :-  solve(log(x,45) + 4*log(x,2) - (1/2)*log(x,81) - log(x,10) = 3/2).
		%solved
 % Jan 1977 Paper1  

lon(8) :-  solve(2^(2*x) - 5*2^(x + 1) + 16 = 0).		%solved

lon(9) :-  solve(8*cos(x) - 15*sin(x) = 3).		%solved

 % June 1977 Paper1  

lon(10) :-  solve(e^(3*x) - 2*e^x - 3*e^( - x) = 0).		%solved

lon(11) :-  solve(2*sin(x) + cos(x) + 2 = 0).		%solved
 % lon(11) asks for tan(x/2) substitution   

 % Paper2 

lon(12)  :-  solve(7*sin(x)^2 - 5*sin(x) + cos(x)^2 = 0).		%solved

lon(13) :-  solve(8*sin(x) + 15*cos(x) = 17/2).		%solved

 % Special Paper  

lon(14) :-  solve(x^4 - 7*x^3 + 14*x^2 - 7*x + 1 = 0).		%solved

 % Jan 1978 Paper1  

lon(15) :-  solve(log(2,x) + 4*log(x,2) = 5).		%solved
 % Whole question 

 % Paper2 

lon(16) :-  simsolve(2*x + 6*y + z = 0 & 
   		( - 1)*x + 2*y - z = 10 & 
		4*x + 3*y + z = 1,[x,y,z],X).		%solved

 % June 1978 Paper2 

lon(17) :-  solve(5*cosh(x) - 3*sinh(x) = 5).		%solved

 % Jan 1979 Paper1 

lon(18) :-  solve(3*cot(2*x) + 7*tan(x) = 5*cosec(2*x)).

 % Paper2 

lon(19) :-  solve(sin(2*x) = sin(x)).		%solved
 % Whole question 

 % June 1979 Paper1 

lon(20) :-  solve(sin(x) - 7*cos(x) + 5 = 0).		%solved
 % lon(20) suggests tan(x/2) method 

 % Paper2 

lon(21) :-  solve(cos(3*x) + sin(3*x) = 1).		%solved

 % June 1980 Paper1 

lon(22) :- solve(sin(3*x) = sin(x)^2). %solved
 % First expand sin(3*x) in the normal way  

lon(23) :- solve(4*cos(x) + sin(x) = 1).  %solved
 % Must use  tan(x/2) method  

 % Paper2 

lon(24) :- solve(4^(3+x)/8^(10*x) = 2^(10 - 2*x)/64^(3*x)). %solved

lon(25) :- solve(log(2,(x+4)) = 2 - log(2,x)). %solved

lon(26) :- solve(6^(1/2)*cos(x) - 2^(1/2)*sin(x) = 2). %solved
 % given that a.cos(x) - b.sin(x) = 2.cos(x+pi/6)  

 % Special Paper 
lon(27) :- solve(2*cosh(2*x) + sinh(x) = 2). %solved

lon(28) :- sim(sinh(x)*cosh(y) = 3 & cosh(x)*sinh(y) = -1, [x,y],X).
	%solved with sim

 % Jan 1981 Paper 2 

lon(29) :- solve(sin(x) = cos(a)).		%solved
 % Solve for x, x and a are both in degrees 

 % June 1981  Paper1 

lon(30) :- solve(e^(log(e,x)) + log(e,e^x) = 8). %solved

 % Paper2 

lon(31) :- solve(sin(2*x) + sin(x) = 0). %solved

lon(32) :- solve(2*cosh(x) - 2*sinh(x) = 3). %solved

lon(33) :- solve(2^(2/x) = 32). %solved

lon(34) :- solve(log(x,2)*log(x,3) = 5). %solved

lon(35) :- solve(x^3 - 2*x^2 - 4*x + 8 = 0). %solved
 % Theory of equations type question.
 % First we must find a relation between b,c and d which
 % holds when the roots of x^3 + b.x^2 + c.x + d = 0 are in G.P.
 % Then we solve the equation above and verify that the roots are in
 % G.P.  Note we do not prove that this relation holds implies
 % roots in G.P. 

lon(36) :- solve(4*x^3 - 24*x^2 + 23*x + 18 = 0). %solved
 % Given that roots are in A.P.  
 % Special Paper 

lon(37) :- solve(sin(8*x)^2 - sin(7*x)^2 = sin(x)^2).

lon(38) :- simsolve(tan(y) + 2*sec(y) = 2*x & x*cot(y) - 2*cosec(y) = 3,
		 [x,y],X).

 % Jan 1983 Paper 2

lon(39) :- solve(sin(x) - cos(x) = 1).  % solved

 % June 1983 Paper 2

lon(40) :- solve(cos(2*x) + 1 = sin(2*x)). %solved

 % 		Oxford Board       

 % Additional Maths 

 % 1976 Paper2  

oxf(1) :-  simsolve(3*x + y = 5 &
		x^2 + 2*y^2 - 3*x + 2*y + 2 = 0,[x,y],X).		%solved

 % Summer 1977 Paper1 

oxf(2) :-  solve(8*cos(x) - sin(x) = 4).		%solved

 % Paper2 

oxf(3) :-  simsolve(2*x + 3*y = 5 &
		x^2 + y^2 - 6*x + 4*y = 0,[x,y],X).		%solved

 % Autumn 1977 Paper2 

oxf(4) :-  solve(2*sin(x)^2 - 1 = (1 + cos(x))^2).		%solved

oxf(5) :-  solve(sec(x) - 1/sec(x) = sin(x)).		%solved

 % Summer 1978 Paper1 

oxf(6) :-  solve(3*sin(x)^2  - cos(x) - 1 = 0).		%solved

 % Paper2 

oxf(7) :-  solve(4*cot(2*x)*cosec(2*x) + sec(x)^2*cosec(x)^2 = 8/3). 
 % For oxf(7) the solver was asked to show first:
 % a) cosec(x)^2 + sec(x)^2 = sec(x)^2*cosec(x)^2,
 % b) cosec(x)^2 - sec(x)^2 = 4*cot(2*x)*cosec(2*x)     

 % Autumn 1978 Paper1 

oxf(8) :-  solve(3*tan(3*x) - tan(x) + 2 = 0). 		%solved
 % Show first that tan(3*x) =  (3*tan(x) - tan(x)^3)/(1 - 3*tan(x)^2)  

 % Summer 1979 Paper2 

oxf(9) :-  solve(sin(3*x) = 2*sin(x)). 		%solved

oxf(10) :-  solve(sin(3*x) = 4*sin(x)). 		%solved

 % A level 

 % Summer 1977 Paper1 

oxf(11) :-  simsolve(2*x^2 - 3*x*y + 2*y^2 = 8 &
		4*x^2 - 5*x*y + 2*y^2 = 4,[x,y],X).

 % Summer 1979 Paper1 

oxf(12) :-  solve(4*cos(x) + 3*sin(x) = 2). 		%solved

 % Paper2 

oxf(13) :-  solve(x^4 - 6*x^3 - 7*x^2 + 36*x + 36 = 0).		%solved

	 % London Syllabus D A level 

 % Jan 1978 Paper2 

dlon(1) :- solve(150*cos(x) + 80*sin(x) = 51).		%solved

 % June 1978 Paper2 
dlon(2) :- solve(2*e^x - 2*e^(- x) = 3).		%solved

dlon(3) :- solve(3*cos(x) + 2*sec(x) + 5 = 0).		%solved
 % Question asks for values of cos(x) and tan(x)^2,rather than x 

dlon(4) :- solve(sin(x) + 7*cos(x) = 5).		%solved
 % Questions 3 and 4 make one complete question 

 % Special Paper 

dlon(5) :- solve(4*tan(2*x) + 3*cot(x)*sec(x)^2 = 0).   %solved

 % Jan 1979 Paper2 

dlon(6) :- solve(sin(2*x) = cos(x)).		%solved

 % June 1979 Paper2 

dlon(7) :- solve(sin(5*x) + sin(3*x) = 0).		%solved

dlon(8) :- solve(cos(x) + cos(x + a) + cos(x + 2*a) = 1 + 2*cos(a)).
 % Find the smallest positive x,given that 0 < a < 90 

dlon(9) :- solve(sin(30 + x) = cos(45 + x)). %solved

 % Special Paper 

dlon(10) :- solve(x^3 - 3*x^2 - 3*x + 1 = 0).		%solved
 % First show tan(3x) = (3tan(x) - tan(x)^3)/(1 - 3tan(x)^2) and then deduce
 % that roots of above equation are tan(15),tan(75) and tan(135) 

 % Jan 1980

 % Paper 1

dlon(11) :- solve(sin(3*x) - sin(x) = cos(2*x)).  %solved

dlon(12) :- solve(6*sin(x) + 8*cos(x) = 5).	%solved

 % June 1980

 % Paper 2

dlon(13) :- solve(3*cos(x) + 4*sin(x) = (5/2)*(3)^(1/2)). %solved

 % Special Paper

dlon(14) :- sim(sin(3*x) - cos(y) = 0 & sin(3*y) - cos(x) = 0).
 % Solve using sin(x) = cos(pi/2 - x) or otherwise

 % Jan 1981

 % Paper 2

dlon(15) :- solve(cos(x) + cos(3*x) = 0).  %solved

dlon(16) :- solve(sin(x) + 3^(1/2)*cos(x) = 1). 	%solved

 % June 1981

 % Paper 2

dlon(17) :- solve(2*cos(2*x) + sin(2*x) = 5^(1/2)/2).  %solved

 % Jan 1982 Paper 2

dlon(18) :- solve(sin(x) + sin(3*x) = 0). % solved

dlon(19)  :- solve(2*sin(x) - 3*cos(x) = 1). % solved

 % June 1982 Paper 2

dlon(20) :- solve(cos(60 + x) + 2*sin(30 + x) = 0). %solved

dlon(21) :- solve(5*cos(x)^2 - 12*sin(x)*cos(x) = 2). % solved

 % June 1983 Paper 2

dlon(22) :-  solve(sin(3*x) = -1/2). % Solved, but answer required in radians

dlon(23) :- solve(2*cos(x) - sin(x) = 1).  % solved

 % 22 and 23 form one question


 % Special Paper

dlon(24) :- sim(tan(y) + 2*sec(y) = 2 & cot(y) + 3*cosec(y) = x,[x,y],_).
 % Find by eliminating y, or otherwise, the value of x for which the
 % equations are consistent.  Hence, or otherwise, find g.s. of y.
  		% Solved, but messily and possibly wrongly

dlon(25) :- solve(sin(4*x)^2 -sin(3*x)^2 = sin(x)^2).
 % First show that sin(4*x)^2 - sin(3*x)^2 = sin(7*x)*sin(x).  Question has
 % a misprint, claiming that the RHS is 7*x*sin(x)!

 % 24 and 25 are one question

	 % Scottish Higher Mathematics 
 % 1977 Paper2 

high(1) :- solve(10*cos(x)^2 + sin(x) - 7 = 0).  %solved

high(2) :- simsolve(x + 3*y = 4 & x^2 + 3*x*y + 5*y^2 - 6*x = 0,[x,y],X).
	%solved

 % 1978 Paper2 

high(3) :- simsolve(2*x - 3*y + 1 = 0 & 2*x^2 + 3*y^2 + 3*x + y = 4,[x,y],X).
	%solved

high(4) :- solve(sin(5*x) + sin(x) = 3*cos(2*x)).	%solved

 % 1979 Paper2 

high(5) :- simsolve(4*x + y - z = 12 & 3*x - y + 3*z = 0
		& 5*x - 3*y + 2*z = -1,[x,y,z],X). 	%solved

high(6) :- solve(2*cos(2*x) + cos(x) - 1 = 0).	%solved

 % 1981 Paper2 

high(7) :- solve(sin(5*x/2) - sin(3*x/2) = 0).	%solved

high(8) :- solve(9*6^(2*x) - 10*6^x + 1 = 0).	%solved

 % 1982 Paper2 

high(9) :- solve(sin(3*x) - sin(x) = 1/2*cos(2*x)).

 % To be solved by factorizing 9*a^(2*x) - 10*a^x + 1 and setting a to 6 

	 % Timing clauses 

timeprob(Prob) :- statistics(runtime,_),
	call(Prob),!,statistics(runtime,[_,Time]),
	trace_press('\n%t took %t milliseconds\n',[Prob,Time],0).

timeprob(Prob) :- statistics(runtime,[_,Time]),
	trace_press('\nCould not solve problem %t,the attempt  took %t milliseconds
	\n',[Prob,Time],0).

	 %  Runs 

runaeball :- aebrecurse(1),!.

runlonall :- lonrecurse(1),!.

runoxfall :- oxfrecurse(1),!.

rundlonall :- dlonrecurse(1),!.

runhighall  :- highrecurse(1),!.

aebrecurse(86) :-  trace_press('\nAEB run complete\n',0),!.

aebrecurse(N) :- timeprob(aeb(N)),eval(N+1,M),aebrecurse(M),!.

lonrecurse(41) :-  trace_press('\nLondon run complete\n',0),!.

lonrecurse(N) :- timeprob(lon(N)),eval(N+1,M),lonrecurse(M),!.

oxfrecurse(14) :-  trace_press('\nOxford run complete\n',0),!.

oxfrecurse(N) :- timeprob(oxf(N)),eval(N+1,M),oxfrecurse(M),!.

dlonrecurse(26) :-  trace_press('\nLondon D run complete\n',0),!.

dlonrecurse(N) :- timeprob(dlon(N)),eval(N+1,M),dlonrecurse(M),!.

highrecurse(10) :-  trace_press('\nScottish Higher run complete\n',0),!.

highrecurse(N) :- timeprob(highrecurse(N)),eval(N+1,M),highrecurse(M),!.

aebrunsol :-  checklist(timeprob,[aeb(1),aeb(2),aeb(4),aeb(5),aeb(6),aeb(7),
	aeb(8),aeb(9),aeb(10),aeb(12),aeb(13),aeb(14),aeb(15),aeb(18),
	aeb(19),aeb(20),aeb(21),aeb(23),aeb(24),aeb(25),aeb(26),aeb(27),aeb(30),
	aeb(31),aeb(32),aeb(33),aeb(34),aeb(36),aeb(37),aeb(38),aeb(39),
	aeb(40),aeb(41),aeb(42),aeb(43),aeb(45),aeb(46),aeb(47),aeb(48),
	aeb(49),aeb(50),aeb(51),aeb(53),aeb(54),aeb(55),aeb(56),aeb(57),
	aeb(58),aeb(59),aeb(60),aeb(63),aeb(64),aeb(65),aeb(66),aeb(67),
	aeb(68),aeb(69),aeb(70),aeb(71),aeb(72),aeb(73),aeb(74),aeb(76),
	aeb(77),aeb(78),aeb(79),aeb(80),aeb(81),aeb(83),aeb(84)]).

lonrunsol :- checklist(timeprob,[lon(1),lon(2),lon(3),lon(4),
	lon(5),lon(7),lon(8),lon(9),lon(10),lon(11),lon(12),
	lon(13),lon(14),lon(15),lon(16),lon(17),lon(19),lon(20),
	lon(21),lon(22),lon(23),lon(24),lon(25),lon(26),lon(27),lon(28),
	lon(29),lon(30),lon(31),lon(32),lon(33),lon(34),lon(35),lon(36),
	lon(39),lon(40)]).

oxfrunsol :- checklist(timeprob,[oxf(1),oxf(2),oxf(3),oxf(4),
	oxf(5),oxf(6),oxf(8),oxf(9),oxf(10),oxf(12),oxf(13)]).

dlonrunsol :- checklist(timeprob,[dlon(1),dlon(2),dlon(3),dlon(4),dlon(5),
	dlon(6),dlon(7),dlon(9),dlon(10),dlon(11),dlon(12),dlon(13),
	dlon(15),dlon(16),dlon(17),dlon(18),dlon(19),dlon(20),dlon(21),
	dlon(22),dlon(23),dlon(24)]).

highrunsol :- checklist(timeprob,[high(1),high(2),high(3),high(4),high(5),
	high(6),high(7),high(8),high(9)]).

eurorun :- checklist(timeprob,[aeb(5),aeb(32),oxf(8),lon(15),aeb(2),
	solve(log(e,x+1) + log(e,x-1) =3),lon(10)]).

