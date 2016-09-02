/*  ATTRAC  Modified by Leon    Updated: 15 February 83
*/

%declarations%

% :- public               attract/3.
% 
% :- mode                 attract(+,+,-),
%                         closeness(+,+,?),
%                         attractable(+,+,-),
%                         tree_size(+,+,-),
%                         tree_size(+,+,+,+,-).
%end%

        %---------------------------------------%
        %          Attraction Routines          %
        %---------------------------------------%

attract(X,Exp,New) :-
        closeness(X,Exp,EC),
        attractable(X,Exp,New,EC),
        closeness(X,New,NC),
        NC < EC,
        !.

%   Try to apply an attraction axiom

attractable(X,Old,New1,Closeness) :-
        mult_occ(X,Old),
        least_dom(X,Old), 
        attrax(U & V,Template,Rewrite), % Assumes attraction between 2
        applicable(Template,Old,Rest),  % subterms only
        contains(X,U),
        contains(X,V),
        newform(Old,Rewrite,Rest,New),
        tidy(New,New1),
        closeness(X,New1,NewC),
        NewC < Closeness,  !,   % Insist on local improvement
        trace_press('%t  attracted in %t gives  %c\n',[X,Old,New1],2).


%   Try to attract within a sub-term

attractable(X,Old,New,_) :- 
        mult_occ(X,Old),
        decomp(Old,[Fun|Args]),
        corresponding_arguments(Args,Arg,NewArgs,NewArg),
        closeness(X,Arg,C),
        attractable(X,Arg,NewArg,C),
        recomp(New,[Fun|NewArgs]).

/* Heuristic measure of closeness used by attraction */

%   The "closeness" of all the occurrences of a kernel X in an expression
%   Exp is the number of arcs in the smallest subtree of Exp which holds
%   all the occurrences of X.  Strictly speaking, we ought to consider a
%   sum or product as a single node: closeness(x, x+x+x, 3).  Until PRESS
%   generally uses bags, this is not done, so closeness(x, x+x+x, 4).  It
%   is easier to compute the number of nodes in the subtree, there is gone
%   less arc.  The algorithm and corrected code are by R.A.O'Keefe.

closeness(X, Exp, Arcs) :-
        tree_size(X, Exp, Nodes),
        Arcs is Nodes-1.

tree_size(X, X, 1) :- !.
tree_size(_X, Exp, 0) :-
        atomic(Exp), !.
tree_size(X, Exp, Size) :-
        functor(Exp, _, N),                     %  cut not needed after all
        tree_size(N, Exp, X, 0, Size).

tree_size(0, _Exp, _X, 0, 0) :- !.                %  X doesn't occur in Exp
tree_size(0, _Exp, _X, M, N) :- !,                %  X does occur in Exp,
        N is M+1.                               %  so count Exp node too.
tree_size(N, Exp, X, Acc, Size) :-
        arg(N, Exp, Arg),
        tree_size(X, Arg, ArgSize),
        NewAcc is Acc+ArgSize,
        M is N-1, !,
        tree_size(M, Exp, X, NewAcc, Size).

/*
closeness(X,Exp,Num) :-
        findall_press(Path,position(X,Exp,Path),Paths),
        closeness(Paths,Num),
        !.

closeness([],0) :- !.

closeness([Path],Num) :- length(Path,Num).

closeness([Path|Rest],Num) :-
        dest_list(Head,Tail,Path),
        tree_divide(Head,Rest,Group,Others),
        closeness([Tail|Group],M),
        closeness(Others,N),
        Num is M + N + 1.

dest_list(Head,Tail,[Head|Tail]) :- !.

tree_divide(Head,Rest,Group,Others)  :-
        tree_divide(Head,Rest,Group,[],Others,[]).

tree_divide(_,[],Group,Group,Others,Others) :- !.

tree_divide(Head,[Path|Rest],Group,P,Others,Q) :-
        dest_list(Head,_,Path),
        !,
        tree_divide(Head,Rest,Group,[Path|P],Others,Q).

tree_divide(Head,[Path|Rest],Group,P,Others,Q) :-
        tree_divide(Head,Rest,Group,P,Others,[Path|Q]).
*/
