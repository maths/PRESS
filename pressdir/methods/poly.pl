/*              POLY                    19.2.81 

                        Written as per note 82 in Mecho folder
                                        1.5.81  Leon 
                                 Updated: 28 October 1984
*/

% Poly_solve is only called when it has been determined that the
% equation is a polynomial equation.
%  i.e. a precondition that the method is called is that is_poly is true

poly_solve(Eqn1#Eqn2,X,Soln1#Soln2,Rules-Diff) :-
        poly_solve(Eqn1,X,Soln1,Rules-Inter),
        poly_solve(Eqn2,X,Soln2,Inter-Diff).

poly_solve(Lhs=Rhs,X,Soln,[Infer,Mult|Rules]-Diff) :-
        poly_norm(Lhs + -1*Rhs,X,Plist),
        poly_tidy(Plist,Qlist),
        cond_poly_print(Lhs + -1*Rhs,X,Qlist,Infer),
        remove_neg_powers(X,Qlist,Poly,Mult),           % Remove negative powers
        poly_method(X,Poly,Soln,Rules-Diff).

cond_poly_print(Poly,X,Plist,tidy(Pol1)) :-
        make_poly(X,Plist,Pol1),
        tidy(Poly,Pol2),
        \+ match(Pol1,Pol2),
        !,
        trace_press('\nPolynomial %t becomes \n\n%t when in normal form\n',
                                                        [Pol2,Pol1],1).
cond_poly_print(_,_,_,_).

poly_out(X,Poly) :-
        make_poly(X,Poly,P),
        trace_press('%t',[P],1).

remove_neg_powers(X,Plist,Qlist,multiply(Mult)) :-
        last(polyand(N,_),Plist),
        N < 0,
        !,
        eval(-N,N1),
        map_add_power(N1,Plist,Qlist),
        make_poly(X,Qlist,Poly),
        tidy(X^N1,Mult),
        trace_press('\nMultiply through by %t to get \n\n%t = 0\n',[Mult,Poly],1).

remove_neg_powers(_,Plist,Plist,nomult).

/*****************************************/
/* ROUTINES FOR POLYNOMIAL EQUATIONS */
/*****************************************/

/* Identities and unsatisfiable equations */

poly_method(_,[],true,[ident|Diff]-Diff) :- !.  % The polynomial has simplified away

poly_method(X,[Pterm],Ans,[single_term|Diff]-Diff) :-   % Polynomial simplified
        !,                                              % to a single term
        singleton_method(Pterm,X,Ans).

singleton_method(polyand(0,A),_,true) :-
        simplify(A,B),
        B = 0,
        !.

singleton_method(polyand(0,_),_,false) :- !.

singleton_method(polyand(_,_),X,X = 0) :- !.

/* LINEAR EQUATIONS */

poly_method(X,Poly,X=Ans,[linear|Diff]-Diff) :-
        linear(Poly),
        !,
        linear_method(Poly,Ans,_).

linear([polyand(1,_)|_]) :- !.

linear_method([polyand(N,A)|T],Ans,N) :-        % Handles disguised linear also
        find1(T,B),
        tidy(-B/A,Ans).

find1([polyand(0,B)],B) :- !.
find1([],0) :- !.               % Shouldn't be needed

/* QUADRATIC EQUATIONS*/

poly_method(X,Poly,Soln,[quadratic|Diff]-Diff) :-
        quadratic(Poly),
        !,
        trace_press('\nUsing quadratic equation formula\n',1),
        find_coeffs(Poly,A,B,C),
        discriminant(A,B,C,Discr),
        roots(X,A,B,C,Discr,Soln).

quadratic([polyand(2,_)|_]) :- !.

find_coeffs([polyand(2,A)|T],A,B,C) :- find2(T,B,C).

discriminant(A,B,C,Discr) :- tidy(B^2 - 4*A*C,Discr).

roots(X,A,B,_,0,X = Root) :-                    % Only 1 root
        !, 
        tidy(-B/(2*A),Root),
      trace_press('\nThe discriminant is zero, so the single solution is %t = %t\n',
                                                        [X,Root],1).

roots(X,A,B,_C,Discr,X = Root1 # X = Root2) :-
        warn_if_complex(Discr),
        tidy((-B + Discr^(1/2))/(2*A),Root1),
        tidy((-B - Discr^(1/2))/(2*A),Root2),
        trace_press('\nSolutions are %t = %t and %t = %t\n',[X,Root1,X,Root2],1).

warn_if_complex(Discr)  :-
        eval(Discr < 0),
        !,
        trace_press('\nRoots are complex',1).

warn_if_complex(_).

find2([polyand(1,B),polyand(0,C)],B,C) :- !.
find2([polyand(1,B)],B,0) :- !.
find2([polyand(0,C)],0,C) :- !.
%       find2([],0,0) :- !.     Shouldn't be needed

/* Polynomial divisible by an integral power of the unknown */

poly_method(X,Plist,X = 0 # Ans,[divide(X^N)|Rules]-Diff) :-
        last(polyand(N,_),Plist),
        N > 0,
        !,
        eval(-N,M),
        map_add_power(M,Plist,Qlist),
        poly_method(X,Qlist,Ans,Rules-Diff).

/* Disguised Linear */

poly_method(X,Poly,Soln,[linear|Rules]-Diff) :-
        disguised_linear(Poly),
        !,
        linear_method(Poly,Ans,N),
        isolate([1,1],X^N=Ans,Soln,Rules-Diff).

disguised_linear([polyand(_,_),polyand(0,_)]).

/* Disguised polynomial equations  */

poly_method(X,Plist,Ans,Rules-Diff) :- 
        poly_hidden(X,Plist,N),         % Disguised polynomial in X^N
        trace_press('\nThis is a hidden polynomial in %t\n',[X^N],1),
        !,
        map_div_power(N,Plist,Qlist),
        poly_method(X^N,Qlist,Inter,Rules-Laws),
        isolate([1,1],Inter,Ans,Laws-Diff). % Maybe needs poly_isolate

poly_hidden(_X,Poly,Gcd) :-
        gcd_powers(Poly,Gcd),
        Gcd > 1,
        !.

/* Special methods for reciprocal polynomial equations
        i.e. those that remain unchanged (w.r.t. roots)
        when unknown is replaced by 1/unknown           */

poly_method(X,Poly,X = -1 # Ans,[divide(X + 1)|Rules]-Diff) :-
        odd_symmetric(Poly),
        trace_press('\nPolynomial is odd-symmetric so %t + 1 is a factor\n',[X],1),
        !,
        factor_out(Poly,1,Plist),
        z_norm(Plist,Qoly),
        poly_method(X,Qoly,Ans,Rules-Diff).

poly_method(X,Poly,X = 1 # Ans,[divide(X - 1)|Rules]-Diff) :-
        odd_anti_symmetric(Poly),
        trace_press('\nPolynomial is odd anti-symmetric so %t - 1 is a factor\n',
                                                        [X],1),
        !,
        factor_out(Poly,-1,Plist),
        z_norm(Plist,Qoly),
        poly_method(X,Qoly,Ans,Rules-Diff).

poly_method(X,Poly,X = 1 # X = -1 # Ans,[divide(X^2 - 1)|Rules]-Diff) :-
        even_anti_symmetric(Poly),
trace_press('\nPolynomial is even anti-symmetric so %t - 1 and %t + 1 are both factors\n',
                                                        [X],1),
        !,
        factor_out(Poly,-1,Plist),
        factor_out(Plist,1,Qlist),
        z_norm(Qlist,Qoly),
        poly_method(X,Qoly,Ans,Rules-Diff).

poly_method(X,Poly,Ans,Rules-Diff) :-
        even_symmetric(Poly),
        sym_transform(Poly,NewPoly),
        trace_press('\nPolynomial is symmetric\n',1),
        !,
        poly_method(X+1/X,NewPoly,Soln,Rules-Inter),
        tidy(Soln,NewEqn),
        poly_solve(NewEqn,X,Ans,Inter-Diff).

/* Guess a root,using integers between 9 and -9  */

poly_method(X,Poly,X = Root # Ans,[divide(X - Root)|Rules]-Diff) :- 
        guess_list(Poly,Candidates),
        member(Root,Candidates),
        root(Poly,Root),
        !,
        trace_press('\nBy inspection %t = %t is a solution\n',[X,Root],1),
        eval(-Root,A),
        factor_out(Poly,A,Plist),
        z_norm(Plist,Qoly),
        trace_press('\nAfter division, polynomial becomes\n',1),
        poly_out(X,Qoly),
        poly_method(X,Qoly,Ans,Rules-Diff).


% isolate hack until code is reformed
isolate(Posn,Eqn,New,[isolate|L]-L) :- isolate(Posn,Eqn,New).
