/* LOG : Logmethod showing meta-level inference

                                                Leon + Bernard
                                                Updated: 12 May 1983
*/
                        %  The equation is solvable by log_method when the
                        %  equation is of the form A*B*...*N=P*Q*...*Z
                        %  where each of the multiplicative terms are of the
                        %  form base^f(x) for varying bases free of the unknown
                        %  and varying functions of the unknown.
                        %  Alternatively the equation can be the of the form
                        %  A+B=0, so that the equation can be transformed into
                        %  the above form.
                        %  For example the AEB question:
                        %
                        %  4^(2*x+1)*5^(x-2)=6^(1-x)
                        %  is solved by taking logs base 10 and solving the 
                        %  linear equation.

prod_exp_terms_eqn(A+B=0,X,New) :-
        !,
        prod_exp_terms(A,X),
        prod_exp_terms(B,X),
        form_new_equation(A+B=0,New).

prod_exp_terms_eqn(A=B,X,A=B) :- 
        prod_exp_terms(A,X),
        prod_exp_terms(B,X).

                        %  Describing the allowable multiplicative forms

prod_exp_terms(A*B,X) :-
        !,
        exp_term(B,X),          % Note the precedence of *
        prod_exp_terms(A,X).

prod_exp_terms(A,X) :- exp_term(A,X).

exp_term(A,X) :- freeof(X,A), !.
exp_term(A^_B,X) :- freeof(X,A).

                        %  Find the appropriate Base and take logs

log_reduce(A=B,X,Base,Eqn) :- 
        log_separate(A=B,X,Loglist,Prod),
        find_base(Loglist,Base),
        take_logs_and_recomp(Base,Loglist,Newlhs),
        tidy(Newlhs=log(Base,Prod),Eqn).

log_separate(A=B,X,Loglist,Prod) :- log_separate(A=B,X,[],Loglist,1,Prod).

log_separate(A=B,X,Loglist,L,Prod,P) :-
        prod_decomp(B,X,Loglist,Inter,Prod,Intp,rhs),
        prod_decomp(A,X,Inter,L,Intp,P,lhs).

prod_decomp(A*B,X,Log,L,Prod,P,Side) :-
        !,
        prod_decomp(A,X,Log,Intl,Prod,Intp,Side),
        prod_decomp(B,X,Intl,L,Intp,P,Side).

prod_decomp(A,X,L,L,Prod,A*Prod,rhs) :- freeof(X,A).
prod_decomp(A,X,L,L,Prod,Prod/A,lhs) :- freeof(X,A).

prod_decomp(A^B,_,Log,[exp_term(A,B,-1)|Log],P,P,rhs).
prod_decomp(A^B,_,Log,[exp_term(A,B,1)|Log],P,P,lhs).

find_base([exp_term(A,_,_)|Log],Base) :-
        ok_number(A),
        base(A,B),
        find_base(Log,B,Base,BaseList),
        check_power_of([B|BaseList],Base),
        !.

find_base(_,10).        % Logs to base 10 is the default

find_base([exp_term(A,_,_)|Log],B,Base,[Exp|BaseList]) :-
        ok_number(A),
        !,
        base(A,Exp),
        least(Exp,B,NewB),
        find_base(Log,NewB,Base,BaseList).
        
find_base([],B,B,[]).

base(A,A) :- integer(A), !.
base(A,Denom) :- eval(numer(A)=1),eval(denom(A),Denom).

                        %  Take logs and reconstitute

take_logs_and_recomp(Base,[exp_term(A,B,Sign)|Log],NewLhs) :-
        tlar(Base,Log,Sign*B*log(Base,A),NewLhs).

tlar(_Base,[],Lhs,Lhs) :- !.
tlar(Base,[exp_term(A,B,Sign)|Log],Sum,Lhs) :-
        tlar(Base,Log,Sign*B*log(Base,A)+Sum,Lhs).

                        %  Check that the new base is a root of all the
                        %  exponents, otherwise fail and use base 10.
check_power_of([],_).
check_power_of([H|T],Base) :-
        powered(Base,_,H),
        !,
        check_power_of(T,Base).

                        %  Manipulate equation of the form A + B = 0
                        %  to remove negative signs if possible.

form_new_equation(A+B=0,NewEqn) :-
        negative_number_product(A,New),
        !,
        \+ negative_number_product(B,_),
        tidy(B=New,NewEqn).

form_new_equation(A+B=0,NewEqn) :- 
        negative_number_product(B,New),
        !,
        \+ negative_number_product(A,_),
        tidy(A=New,NewEqn).


negative_number_product(Term,New) :-
        decomp(Term,[*|Args]),
        select(Number,Args,Rest1),
        ok_number(Number),
        eval(Number < 0),
        recomp(Rest,[*|Rest1]),
        tidy(-Number*Rest,New),
        !.

