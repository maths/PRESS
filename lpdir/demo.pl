%   File   :  /usr/bs/lpdir/demo.pl
%   Author : Bernard Silver
%   Updated: Wed Oct 30 10:28:59 1985
%   Purpose: Demo questions for LP.


/*
This files contains a simple demo for LP.  Consult or compile it into LP.
Optionally, the resulting state can then be saved so the system can be run
without obviously loading in a special file.  The demo looks best when run
in a Sun window, with a big font (e.g. gallant.r.19) NOT using emacs, 
with Page Mode on.(SEE NOTE BELOW) 

This file defines three problems.  The first is obtained by typing
"problem." to the LP prompt.  (Here as elsewhere in this file, don't type 
the acutal quote marks!)   This gets LP to try to solve the equation
	cos(x) + 2*cos(3*x) + cos(5*x) = 0.

Normally, LP would in fact be able to solve this problem by
Homogenization and Nasty Function methods.  For ease of demonstration,
these methods have been disabled by this file.  LP is thus unable to
solve this problem and fails very quickly.  

LP is then given a worked example for a similar problem,
by typing "example".  During this problem five inputs are needed.
The first, and most complex, is when the user is asked for a rule.
Type
	"cos(A)+cos(B)=>2*cos((A+B)/2)*cos((A-B)/2)."

A and B can be replaced by any two Prolog variables, (they must  begin with an
upper case letter).  The two variables  should of course be distinct.

The next two prompts ask for the unknown and any conditions.  Just hit return
both times (as indicated in the prompt).  Next give a name for the rule.
This can be any Prolog atom, it should not be a variable.

Finally, you are asked if you want the problem run again, type either "y" or "n",
followed by return, no period is needed.  If you do run the problem again, no
more questions will be asked (assuming the program is working).

Now run problem again.  LP should be able to solve this using the schema generated
in the previous worked example.


If you have time, then run "other".  This asks LP to solve an equation similar
to that of "problem" but the schema needs to be patched by added an additional
step at the beginning.

Thats it.

NOTE:   There seems to be a bug in the version 2 release of Sunwindows in that 
        if  you type a character to move onto the next page with Page Mode on, 
	that character is sent to the program in the window.  As LP uses
	character input, use only the mouse and menu to move on.  menu to go 
	onto the next page, as LP uses character input.  

*/

:- flag(method('Homogenization'),_,off),flag(output,_,yes),
 flag(method('Change of Unknown'),_,off).

problem :- 
	writef('\nProblem is to solve cos(x) + 2*cos(3*x) + cos(5*x) = 0\n'),
	solve(cos(x) + 2*cos(3*x) + cos(5*x) = 0).
other :-
 writef('\nProblem is to solve 2*cos(x) + 3*cos(2*x) + 2*cos(3*x) = 0\n'),
	solve(2*cos(x) + 3*cos(2*x) + 2*cos(3*x) = 0).
example :- work([

	cos(x) + 3/2*cos(2*x) + cos(3*x) = 0,

	2*cos(2*x)*cos(x) + (3/2)* cos(2*x) = 0,

	cos(2*x)*(2*cos(x) + 3/2) = 0,

	cos(2*x) = 0 #  2*cos(x) + 3/2 = 0,

	cos(2*x) = 0,

	x =  (180*n1)*(1/2) +  45,

	2*cos(x)  + 3/2 = 0,


	x =  n2*360 + arccos(-(3/4)) # x= n2*360 + -1*arccos(-(3/4))

	],x).

