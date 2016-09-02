/*	RUNEX
	Commands to run test examples.		Updated: 12 August 82
	The examples are found in the
	files testex.prb, mecho.prb, lewis.prb		Leon
	and exam in the area extras.			*/


run :- (present(testex) ; ['press:testex.prb']), !,
		checklist(stats, [logeqn(A1), expeqn(A2), trigeqn(A3), 
			negpolyeqn(B1),homogeqn(B2),chunkeqn(B3),
			invlogeqn(C1),nastyeqn(C2),cosapeqn(C3),acbseqn(C4),
			taklogeqn(C5),
			coseqn(D1),sqrteqn(D2),pow2eqn(D3),quarteqn(D4)]).

smallrun :- (present(testex) ; ['press:testex.prb']), !,
		checklist(stats,[logeqn(A1), expeqn(A2), trigeqn(A3)]).

mechorun :- (present(mecho), present(init) ;
		['press:init.mec','press:mecho.prb']), !,
		checklist(stats, [simppull(A1), nl4(A2), car(A3),
			pulltab(A4), tower1(A5), stvineq(A6), conjineq(A7),
			dome(A8), bloc(A9), train(A10), loop(A11)]).

lewisrun :- (present(lewis) ; ['press:lewis.prb']), !,
		checklist(stats, [a1(X1), b1(X2), a2(X3), b2(X4), a3(X5),
			b3(X6), a4(X7), b4(X8),	a5(X9), b5(X10), a6(X11),
			b6(X12), a7(X13), b7(X14), a1hard(X15), a2hard(X16),
			b1hard(X17), b2hard(X18), c1hard(X19), c2hard(X20), 
			d1hard(X21), d2hard(X22)]).

aebrun :- examcheck,aebrunsol,!.

lonrun :- examcheck,lonrunsol,!.

dlonrun :- examcheck,dlonrunsol,!.

oxfrun :- examcheck,oxfrunsol,!.

highrun :- examcheck,highrunsol,!.

eurocamrun :- examcheck,eurorun,!.

exam :- present(exam),!,
	writef('\npress:exam is already loaded, nothing done\n').
exam  :- writef('\n[Consulting press:exam]\n'),consult('press:exam'),!.

examcheck :- present(exam),!.
examcheck :- writef('\n[Consulting press:exam]\n'),consult('press:exam'),!.

/*Run problem with statistics*/

stats(Problem) :- Problem=..[Name,Arg], statistics(runtime,_),
		call(Problem), !, statistics(runtime,[ _, Time]),
		trace_press('\n%t took %t milliseconds and produced answer %e\n\n', 
			[Name,Time,Arg], 0).

stats(Problem) :- statistics(runtime,[ _, Time]),
  trace_press('\nSorry I could not prove %t and I spent %t not doing it \n\n', 
	[Problem, Time], 0).

