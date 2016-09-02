/*  COLLEC      A more efficient version        Leon
                                Updated: 15 February 83
*/
/*****************************************/
/* COLLECTION ROUTINES*/
/*****************************************/
%declarations%
% :-  public              collect/3,
%                         applicable/3,
%                         newform/4.
% 
% :- mode                 collect(+,+,-),
%                         applicable(+,+,?),
%                         newform(+,+,+,?),
%                         template_match(+,+,?).


collect(X,Exp,New1) :- 
        mult_occ(X,Exp),
        least_dom(X,Exp),               % Expression is in weak normal form
        collax(U,Template,Rewrite),
        applicable(Template,Exp,Rest),
        contains(X,U),
        !,
        newform(Exp,Rewrite,Rest,New),
        tidy(New,New1).

/* TRY TO COLLECT WITHIN A SUBTERM*/

collect(X,Old,New) :- 
        mult_occ(X,Old),
        decomp(Old,[Fun|Args]),
        corresponding_arguments(Args,Arg,NewArgs,NewArg),
        collect(X,Arg,NewArg),
        recomp(New,[Fun|NewArgs]),
        !.

% Does a rewrite rule match an expression? 
% A more efficient version than relying on the built in commutativity
% and associativity of the matcher

applicable(Template,Exp,Rest) :-
        ident_operators(Template,Exp),          % quick test
        template_match(Template,Exp,Rest).

    ident_operators(A,B) :- A=..[Op|_], B=..[Op|_].

template_match(Template,Exp,Rest) :-
        Template=..[Op,C,D],
        ac_op(Op,_,_,_,_),
        !,
        decomp(Exp,[Op|Args]),
        select(A,Args,Rem),
        perm2(C,D,Pat1,Pat2),
        exp_match(A,Pat1,Pat2,Rem,Rest),
        !.

template_match(Template,Exp,[]) :-
        match(Exp,Template).

exp_match(A,C,D,Rem,Rest) :-
        match(A,C),             % stop match backtracking (the key idea)
        !,
        exp_match1(A,C,D,Rem,Rest).

exp_match1(_A,_C,D,Rem,_Rest) :-
        ops_to_find(D,Ops),
        tidy_ops(Ops,Term),
        absent(Term,Rem),
        !,
        fail.

exp_match1(A,C,D,Rem,Rest) :-
        match(A,C),
        select(B,Rem,Rest),
        match(B,D),
        !.

ops_to_find(Pat,Pat) :- atomic(Pat), !.
ops_to_find(Pat,var) :- var(Pat), !.
ops_to_find(Pat,Term) :-
        Pat =.. [Op|Args],
        ops_list(Args,NewArgs),
        Term =.. [Op|NewArgs].

ops_list([],[]) :- !.
ops_list([H|T],[NewH|NewT]) :-
        ops_to_find(H,NewH),
        ops_list(T,NewT).

absent(_,[]) :- !.
absent(_Ops,[H|_Rest]) :-
        compatible(_Term,H),
        !,
        fail.

absent(Term,[_|Rest]) :- absent(Term,Rest).

compatible(var,_H) :- !.
compatible(Term,H) :-
        Term=..[Op|Args],
        H=..[Op|Terms],
        list_compatible(Args,Terms).

list_compatible([],[]) :- !.
list_compatible([H|T],Terms) :-
        select(A,Terms,Rest),
        compatible(H,A),
        list_compatible(T,Rest),
        !.

newform(_,Rewrite,[],Rewrite) :- !.

newform(Exp,Rewrite,Rest,New) :-
        Exp=..[Op|_],
        recomp(Term,[Op|Rest]),
        New=..[Op,Rewrite,Term].

tidy_ops(var*Term,New) :- !, tidy_ops(Term,New).
tidy_ops(Term*var,New) :- !, tidy_ops(Term,New).
tidy_ops(var+Term,New) :- !, tidy_ops(Term,New).
tidy_ops(Term+var,New) :- !, tidy_ops(Term,New).
tidy_ops(Term,Term).
