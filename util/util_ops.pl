%   File   : UTIL.OPS
%   Author : Lawrence Byrd, L Hardman
%   Updated: 4 April 1984
%   Purpose: Operator declarations for UTIL and other MECHO programs

:- op(950,xfy,#).			% Used for disjunction
:- op(920,xfy,&).			% Used for conjunction
% :- op(900,fy,[not,thnot]).		% See INVOCA.PL
% :- op(700,xfx,\=).			% see IMISCE.PL

				% Conveniences

% :- op(600,xfy, (.)).			% see EDIT.PL
% :- op(300,fx,edit).			% see EDIT.PL
% :- op(300,fx,redo).
% :- op(300,fx,tlim).			% see TRACE.PL
% :- op(300,fx,ton).
% :- op(300,fx,toff).
% :- op(100,fx,l).
% :- op(100,xfy,(:)).			% see EDIT.PL
