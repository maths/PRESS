%   File   :  /usr/bs/lpdir/misce.pl
%   Author : Lawrence Byrd
%   Updated: Tue Oct 15 11:55:05 1985
%   Purpose: Misc!

:- op(100,fx,l).


  \=(X,X) :- !, fail.

  \=(X,Y) :- !.


  clean	:-  file_delete('prolog.log').


  diff(X,X) :- !, fail.

  diff(X,Y) :- !.


l(X) :- listing(X).

  not(X) :- X, !, fail.

  not(X) :- !.

