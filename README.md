# PRESS: PRolog Equation Solving System

v.01, September 2016.

## Introduction

This directory contains a copy of the PRESS (PRolog Equation Solving System)
source code that was obtained from (http://dream.inf.ed.ac.uk/software/press/).
It has been modified to run on SWI Prolog (http://www.swi-prolog.org/).

Not all parts of PRESS are running correctly yet, but enough is running
correctly to solve simple equations such as "log(2,x) + 4*log(x,2) = 5".


## Instructions for loading and running PRESS

1. Open a shell, and change into the directory that contains swiload.pl.
2. Launch SWI Prolog.
3. At the ?- prompt, enter "['swiload.pl']." to load PRESS.
4. At the ?- prompt, enter "solve(log(2,x) + 4*log(x,2) = 5)." to solve this equation.
5. Exit SWI Prolog by entering "Ctrl+C" followed by "e".

## Demos

For demos, look in pressdir/probs

## Other high-level functions.

1. tidy(X, A). Tidy up the expression X, but using identity transformations such as 1*x=x, 0*x=0, x+0=0 etc. See util/tidy.pl
2. Sets.   See util/setutl.pl
3. util/occur.pl contains useful structural functions such as freeof(A,B).

Makes sure B is free of A.  E.g freeof(x,x+y=1) It is literal, e.g.  freeof(x^2,x^2+y^2=1).

     contains/2,                     %   Term x Term ->
     freeof/2,                       %   Term x Term ->
     patharg/3,                      %   Path x Term -> Term
     position/3,                     %   Term x Term -> Path
     replace/4.                      %   Path x Term x Term -> Term
