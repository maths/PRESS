/* NAS1. : 

                                                Bernard Silver
                                                Updated: 24 February 82
Created: May 1981       
*/

% :- public
%                 findrhs/2,
%                 nas1/3.

% This method is similar to isolate,it it used to solve equations where
% all occurrences of the unknown are dominated by a function other than =,+,*
% eg sin(x^2+x+1)=(1/2).  Should also remove multiplicative constants.

% Top level 
% If equation is of the right type find the position of the dominating 
% function and prepare to isolate it  
nas1(L=_R,X,[1|Pos]) :- L=..[Func|Args],
        nas1ok(Func,Args,X,Pos),
        !.

nas1ok((+),_,_,_) :- !,fail.
nas1ok(*,[A,B],X,[1]) :- contains(X,A),freeof(X,B),!.
nas1ok(*,[B,A],X,[2]) :- contains(X,A),freeof(X,B),!.
nas1ok(*,_,_,_) :- !,fail.
nas1ok(log,[A,B],X,[1]) :- contains(X,A),freeof(X,B),!.
nas1ok(log,[B,A],X,[2]) :- contains(X,A),freeof(X,B),!.
nas1ok(log,_,_,_) :- !,fail.
nas1ok(^,[A,B],X,[1]) :- contains(X,A),freeof(X,B),!.
nas1ok(^,[B,A],X,[2]) :- contains(X,A),freeof(X,B),!.
nas1ok(^,_,_,_) :- !,fail.
nas1ok(_,_,_,[1]) :- !.

% Defensive checking,make sure that no unknowns occur on the right hand
% side of the isolated equation  

findrhs(A#B,P) :- !,findrhs(A,C),findrhs(B,D),append(C,D,P).
findrhs(_A=B,[B]) :- !.



