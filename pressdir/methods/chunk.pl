%   Press:Chunk.                        Updated: 30 August 82
%   Clause removed 19.2.81, modified 28.4.81, 26.5.81, 10.9.81.
%   subst_mesg moved to Misc, rest made compilable 12.9.81.

% :- public
%         changeunknown/3,
%         changevar/4,
%         good_subterm/4.         %   just so that 'setof' can find it.
% 
% :- mode
%         changeunknown(+, +, -),
%         changevar(+, +, +, -),
%         good_subterm(+, +, +, -),
%             good_subterm(+, -),
%                 good_subterm(+, +, -).


/*  There is a non-trivial BUG:
    change of unknown sometimes fails when it should apparently succeed,
    e.g. when solving for x in the equation
        y + x*(x+1)^(-1)*6 + (y+4)*x*(x+1)^(-1)*(-3) = 1
    (this is problem  d2hard  in the Lewis set).  The problem is due to
    the lack of associativity in the simple matcher, so that the subterm
    x*(x+1)^(-1) actually appears only once in this equation.  Fixing
    this will require extensive reworking of good_subterm/subterm.
*/

%   changeunknown(Eqn, Var, Ans) determines whether there is a suitable subterm 
%   (Term) of Eqn (which contains the unknown Var) for changing the unknown.
%   The equation is assumed to be in weak normal form.

changeunknown(Lhs=_Rhs, Var, Term) :-
        occ(Var, Lhs, N), N > 1,
        setof(Term, good_subterm(Lhs, Var, N, Term), TermSet),
        extreme_term(TermSet, >, Term),!.

%   changevar generates a new variable NewVar and performs the relevant
%   substitution.

changevar(Term, Eqn, New, NewEqn) :-
        identifier(New),
        subst_mesg(Term=New, Eqn, NewEqn).

%   find good subterms for the change of unknown method.

good_subterm(Exp, Var, N, Term) :-
        good_subterm(Exp, Term),
        occ(Var, Term, M), M > 0,
        occ(Term, Exp, L), L > 1,
        N is L*M.
        
        %   good_subterm(Term, Exp) is true when Term is a non-atomic subterm
        %   of Exp.  This enables us to drop the "Term \= Var" requirement in 
        %   good_subterm/4.

        good_subterm(Exp, _Term) :-
                (   atomic(Exp) ; ok_number(Exp)   ), !, fail.
        good_subterm(Exp, Term) :-
                functor(Exp, _, N),
                good_subterm(N, Exp, Term).

            %   good_subterm(N,E,T) <- T is a good subterm of Exp's Nth argument

                good_subterm(0, Exp, Term) :- !, Term = Exp.
                good_subterm(N, Exp, Term) :-
                        arg(N, Exp, Arg),
                        good_subterm(Arg, Term).
                good_subterm(N, Exp, Term) :-
                        M is N-1, !,
                        good_subterm(M, Exp, Term).

