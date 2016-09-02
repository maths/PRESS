%   File   :  /usr/bs/lpdir/interp.pl
%   Author : Bernard Silver
%   Updated: Tue Oct 29 19:26:58 1985
%   Purpose: Misc code that was interpreted on Dec-10

% Includes code by Alan, Lawrence, Leon and Richard


% Get help.
user_help :- 
	writef('\nDo you want help with LP or with Prolog?\n'),
	repeat,
	writef('Type l for LP, p for Prolog.\n'),
	prompt(_,'Type of Help:'),
	get0(Var1),
	name(NewVar,[Var1]),
	user_help(NewVar,Var1,Ans),
	!,
	type_help(Ans),
	!.

type_help(l) :- !,lphelp.
type_help(p) :- !,fail.   % Failing user_help so that Quintus help takes over
lphelp :- 
	monitor_help(File),
	writef('
Help is available either at monitor level or from within this program.
At UNIX level see file %w.\n',[File]),
	process_reply([y,n],help1(Ans),Ans,'More Help:',writef('\n
Do you want to continue with help in this program? (y/n)\n')),
	!.

help1(y) :- !,
	(current_predicate(give_help,_);
	writef('\n[Loading help file]\n'),
	compile(library(mdhelp))),
	ttynl,
	!,
	give_help(lp).

help1(n) :- 
	monitor_help(File),
	writef('\n\tOK.  (%w can also be examined from this 
	program by  typing ''doc'' in response to the Prolog prompt, but it
	will not be formatted very well) \n',[File]),
	!.

doc :- 
	monitor_help(File),
	name(File,NF),
	append("cat ",NF,Name),
	!,
	name(Atom,Name),
	unix(system(Atom)).

% user_help/1 based on process_reply in OUT.PL, which cant be used as
% we may need type_help to fail.

user_help(_,Var,_) :-
	Var >= "A", Var =< "Z",
	skip(10),
	!,
	writef('\nYou have used a variable, please try again.\n'),
	fail.
user_help(Var,_,Ans) :-
	(Var=10 -> Ans = l,writef('\nAssuming default value l\n'))
	;
	(skip(10),member(Var,[l,p]),Ans = Var),
	!.
user_help(a,_,_) :- !,writef('\n[Aborting]\n'),abort.
user_help(b,_,_) :-
	!,
	writef('\n[Entering break]\n'),
	break,
	writef('\n[Leaving break]\n'),
	fail.
user_help(t,_,_) :-
	writef('\n[Tracing]\n'),
	trace,
	fail.
user_help(?,_,_) :- !,help_response([l,p]).
user_help(h,_,_) :- !,help_response([l,p]).
user_help(_,_,_) :-
writef('\nNot a valid response!  Use one of [l,p]\n\nPlease try again.\n'),
	fail.
	   


			% Redo last equation

sredo :-
	retract(last_equation(Equation)),
	!,
	solve(Equation).

sredo :- writef('\nNo previous equation, nothing done.\n').

 % File read in
wep :- 
	consult(library(wep)).
go :-
	writef('\nGo option isn''t supported, type lphelp for help.\n'),
	!.



 % Quick definitions to replace the interval package
negative(X) :- tidy(X,Y),ok_number(Y),!,eval(Y<0).

non_neg(X) :- negative(X),!,fail.
non_neg(X) :- tidy(X,Y),eval(Y,0),!.
non_neg(_).

non_zero(X) :- tidy(X,New),eval(New=0),!,fail.
non_zero(_) :- !.

 % Hack for tidying factors
check_tidy(A*B=0,Y) :- !,tidy_expr(A*B=0,Y).
check_tidy(A*B,Y) :- tidy_expr(A*B=0,Y).


 % Two poly normal forms are the same.
check_same_poly(_,[],[]).
check_same_poly(Fac,[H|T],[H1|T1]) :-
	same_poly_fac(Fac,H,H1),
	!,
	check_same_poly(Fac,T,T1).

same_poly_fac(1,polyand(N,M),polyand(N,M)) :- !.
same_poly_fac(Fac,polyand(M,N),polyand(M,N1)) :-
	non_zero(N1),
	eval(N/N1,Fac),
	!.




 % Call a list of conditions
check_cond([]).
check_cond([H|T]) :- call(H),check_cond(T).


 % Get a file name
file_name(Type,File) :-
	repeat,
	writef('\nName the file to write %w to?\n',[Type]),
	prompt(_,'Filename:'),
	read_name(File),
	(valid_filename(File,_); % Check that a file name is not too long 
				 % (Uses Lincoln's Code in File)
	(writef('\nInvalid file name, please try again.\n'),fail)),
	!.


file_name_check(File,Type,NewFile) :-
	file_exists(File),
	!,
	process_reply([y,n],f_n1(Reply,File,Type,NewFile),Reply,'Overwrite:',
writef('\nFile %w already exists.  Overwrite it? (y/n)\n',[File])),
	!.

file_name_check(File,_,File) :- !.

f_n1(n,_,Type,File) :- !,
	process_reply([y,n],f_n2(Reply,Type,File),Reply,'Choose new File:',
writef('\nChoose new file? (y/n)\n')).

f_n1(y,File,_,File) :- !,
	writef('\nOK, overwriting file %w\n',[File]).

f_n2(y,Type,File) :- !,
	file_name(Type,File).
f_n2(n,_,_) :- !,
	writef('\nOK, Aborting from writeout command\n'),
	!,
	call(abort).

 % Get and check a file name, query if file already exists
file_name1(Type,File) :-
	file_name(Type,File1),
	file_name_check(File1,Type,File),
	!.

type_tidy(normal,Old,New) :- !,tidy(Old,New).
type_tidy(expr,Old,New) :- !,tidy_expr(Old,New).



 % Split a list of multiplicative factors into a list of 
 % equations, removing useless ones

remove_safe_divisors(_,[],[]) :- !.

remove_safe_divisors(X,[H|T],T1) :-
	safe_divisor(X,H),
	!,
	remove_safe_divisors(X,T,T1).

remove_safe_divisors(X,[H|T],[H1|T1]) :- 
	type_tidy(expr,H=0,H1),
	remove_safe_divisors(X,T,T1).

safe_divisor(X,H) :- freeof(X,H),non_zero(H).




 % match_check(X,Y) sees if match(X,Y) is true and cuts alternatives
match_check(X,Y) :- ground(X),!,match(X,Y).
match_check(X,Y) :- ground(Y),!,match(Y,X).

 % Maplist tidy
maptidy_example(X,Example,New,Flag) :-
	maptidy_example1(X,Example,Mid),
	!,
	merge_steps(Example,Mid,New,Flag).

maptidy_example1(_,[],[]) :- !.
maptidy_example1(X,[H|T],[H1|T1]) :- 
	mod_weak_normal_form2(H,X,H1),
	!,
	maptidy_example1(X,T,T1).

maptidy_example1(X,[H|_],_) :-
	writef('\n
[**Unable to weak_normal_form\n%t\nfor %t.\nFailing**]\n',[H,X]),
	!,
	fail.

 % If two steps are the same after tidying merge them and warn user
merge_steps([],[],[],_) :- !.
merge_steps([_],[B],[B],_) :- !.
merge_steps([H,H|T],[A,A|C],D,Flag) :- !,
(Flag = sol; writef('\n[**Consecutive step %t, steps merged**]\n',[H])),
	!,
	merge_steps([H|T],[A|C],D,Flag).

merge_steps([H,H1|T],[A,A1|C],[A|D],Flag) :- 
	match(A,A1),
	!,
(Flag = sol;	writef('\n[**Steps\n\t%t\n\nand\n\n\t%t\n
both tidy to \n\n\t%t\n\nsteps merged**]\n',[H,H1,A])),
	!,
	merge_steps(T,C,D,Flag).


merge_steps([_|T],[A|C],[A|D],Flag) :- !,
	merge_steps(T,C,D,Flag).



 % Tidy Conjectures	
tidy_up(A=B,C,D) :- 
	functor(A,F,_),
	(F= + ; F = *),
	decomp(A,[F|List]),
	shift_numbers(List,[],Rest,[H|Numbers],F), % [H|Numbers] must be 
	!,					   % non_empty
	recomp(Num,[F,B,H|Numbers]),		   
	recomp(C1,[F|Rest]),
	tidy(C1=Num,New),
	!,
	tidy_up(New,C,D).

tidy_up(A=B,A,B) :- !.


 % Add new auto method
 % General case
add_new_method(Method,Type) :- 
	(Type = upc(Post) -> add_to_table(method(Method),Post);true),
	cond_assertz(method(method(Method))),
	cond_assertz(just_created(Method)),
	flag(method(method(Method)),_,on).

	
cond_assertz(A) :- call(A),!.
cond_assertz(A) :- assert(asserted(A)),assertz(A).



 % Abolish the units rather than every term
mod_abolish(Functor,Arity) :-
	functor(Term,Functor,Arity),
	mod_ab(Term).

mod_ab(Term) :- retract(Term),fail.
mod_ab(_).

mod_asserta(X) :- assert(asserted(X)),asserta(X),!.
mod_assert(X) :- assert(asserted(X)),assert(X),!.
mod_assertz(X) :- assert(asserted(X)),assertz(X),!.


subs1(Exp,[],Exp) :- !.
subs1(Exp,[H|T],E1) :- subst(H,Exp,E2),!,subs1(E2,T,E1).


make_subl([],[],[]) :- !.
make_subl([X|R],[X|R1],R2) :- !,make_subl(R,R1,R2).
make_subl([Hd|R],[H1|R1],[Hd=H1|R2]) :- !,make_subl(R,R1,R2). 


great_el([Hd],Hd) :- !.
great_el([Hd|Tl],Ans) :- great_el(Tl,Hgr),(eval(Hd>Hgr) -> Hd=Ans;Hgr=Ans),!.

mod_gensym(Root,Term) :-
	repeat,
	gensym(Root,Term),
	really_new(Root,Term),
	!.

really_new(auto,Term) :- !,
	not (method(method(Term))).
really_new(n,Term) :- !,
	not integral(Term).
really_new(x,Term) :- !,
	not unknown(Term).
