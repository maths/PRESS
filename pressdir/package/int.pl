/* INT : Finds intervals of terms in PRESS

						Alan Bundy
						Updated: 30 September 82

	Alan Bundy 19.12.79, revised 13.3.80, 26.3.81
	Cosmetics by Lawrence 18.6.81, later versions
	new pi, e handling by R.A.O'K 30.9.82
*/

/* EXPORT */

:- public
	vet/2,
	positive/1,
	negative/1,
	non_neg/1,
	non_pos/1,
	non_zero/1,
	acute/1,
	obtuse/1,
	non_reflex/1,
	
	less_than/2,			% Used in a \+
	
	find_int/2,			% Exported for convenience
	int_apply/3.


/* IMPORT */
/*
	error/3				from  UTIL:TRACE
	
	memberchk_press/2			from  UTIL:SETROU
	
	number/1			from  LONG
	eval/1
	eval/2
	
	measure/2			from  notional Mecho database
	quantity/1
	angle/3
	incline/3
	concavity/2
	slope/2
	partition/2
	
%%	special_atom/1  		from  ARITH:FACTS  not needed!
*/


/* MODES */

/* 
:- mode
	vet(+,?),
	positive(+),
	negative(+),
	non_neg(+),
	non_pos(+),
	non_zero(+),
	acute(+),
	obtuse(+),
	non_reflex(+),
 
	    gen_combine(+,?),
	    combine(+,+,?),
	    in(+,+),
	    sub_int(+,+),
	    below(+,+),
	    alan_disjoint(+,+),
	    overlap(+,+),
	    marker_flip(?,?),
 
		default_interval(?),
	    find_int(+,?),
		find_int2(+,-),
		find_int_args(+,-,-),
		find_simple_int(+,-),
		    make_assumption_positive(+),
 
	    int_apply(+,+,-),
		int_apply_all(+,+,-),
		all_are_contained(+,+),
		make_regions(+,+,-),
		    split(+,+,+,-),
			split1(+,+,-),
		    cartesian_product(+,+,-,?),
			cart_prod(+,+,+,-,?),
		find_limits(+,+,+,-),
		    clean_up(+,-),
		    limits(+,+,+,+,?),
		    get_bnds(+,+,+,-),
			updown_flip(+,+,-),
			get_bnd(+,+,-),
 
	    order(+,+,?,?),
	    less_than(+,+),
	    calc(+,+,?),
		breakup_bnds(+,-,-),
	    comb(+,?),
 
	    mono(+,?,?),
 
	    classify(+,-),
		interval(+,-,-),
		collect_intervals(+,+,-),
		quad(+,+,+,?).
 


    Data structures

	<interval>	has form	i(LMarker,Bottom,Top,RMarker)
	<boundary>	has form	b(N,Marker)

		where:
			Bottom, Top, N		  are  <numbers>
			LMarker, RMarker, Marker  are  one of {open,closed}

	An interval ranges between Bottom and Top and is open or closed at
	the ends depending on LMarker (for Bottom) and RMarker (for Top).

	A boundary is an end of an interval. There are operations defined
	over these boundaries which are then used to help define the
	operations over intervals. Note that the notion of a boundary does
	NOT involve any specific end of an interval (ie Top/Bottom). They
	are a generalisation over all such ends.

*/

:- dynamic assumed_positive/1.

%% @@@ - marker (top of code)


/****************************************/
/* Use interval information - top level */
/****************************************/

			% Check that solution is admissible

vet(true,true).

vet(false,false).

vet(A&B,A1&B1) :- vet(A,A1), vet(B,B1).

vet(A#B,A1#B1) :- vet(A,A1), vet(B,B1).

vet(A=B,A=B) :- 
	find_int(A,IntA), find_int(B,IntB),
	overlap(IntA,IntB),
	!.

vet(_A=_B,false).

	
			% X is positive, negative, acute, etc.

positive(X) :- find_int(X,i(L,B,_T,_R)), less_than(b(0,closed),b(B,L)).

negative(X) :- find_int(X,i(_L,_B,T,R)), less_than(b(T,R),b(0,closed)).

non_neg(X) :- find_int(X,i(L,B,_T,_R)), less_than(b(0,open),b(B,L)).

non_pos(X) :- find_int(X,i(_L,_B,T,R)), less_than(b(T,R),b(0,open)).

non_zero(X^_N) :- !, non_zero(X).	%ad hoc patch (replaces negative(N)

non_zero(X) :-
	find_int(X,i(L,B,T,R)),
	( less_than(b(0,closed),b(B,L)) ; less_than(b(T,R),b(0,closed)) ),
	!.

acute(X) :-
	find_int(X,i(L,B,T,R)),
	less_than(b(0,open),b(B,L)),
	less_than(b(T,R),b(90,open)).

obtuse(X) :-
	find_int(X,i(L,B,T,R)),
	less_than(b(90,open),b(B,L)),
	less_than(b(T,R),b(180,open)).

non_reflex(X) :-
	find_int(X,i(L,B,T,R)),
	less_than(b(0,open),b(B,L)),
	less_than(b(T,R),b(180,open)).



/*****************************************/
/*	Manipulating Intervals		 */
/*****************************************/


			% Combine a list of intervals by sweeping list and
			%  accumulating the combined intervals.

gen_combine([FirstInt|RestInts],Result)
     :-	gen_combine(RestInts,FirstInt,Result).


gen_combine([],Result,Result).

gen_combine([Int|RestInts],Acc,Result)
     :-	combine(Int,Acc,NewAcc),
	gen_combine(RestInts,NewAcc,Result).


			% Combine x and y intervals

combine(i(Lx,Bx,Tx,Rx), i(Ly,By,Ty,Ry), i(L,B,T,R)) :-
	order(b(Tx,Rx),b(Ty,Ry),_,b(T,R)),
	order(b(Bx,Lx),b(By,Ly),b(B,L),_).


			% Number N is contained in interval

in(N,i(L,B,T,R)) :- !,
	sub_int(i(closed,N,N,closed),i(L,B,T,R)).


			% x interval is contained in second interval

sub_int(i(Lx,Bx,Tx,Rx),i(L,B,T,R)) :-
	marker_flip(L,L1), marker_flip(R,R1),
	less_than(b(B,L1),b(Bx,Lx)), less_than(b(Tx,Rx),b(T,R1)).

			% x interval is wholly below y interval

below(i(_Lx,_Bx,Tx,Rx),i(Ly,By,_Ty,_Ry)) :-
	less_than(b(Tx,Rx),b(By,Ly)), !.

			% x and y intervals are disjoint

alan_disjoint(IntX,IntY) :- below(IntX,IntY), !.
alan_disjoint(IntX,IntY) :- below(IntY,IntX), !.

			% x and y intervals overlap

%% overlap(IntX,IntY) :- \+ alan_disjoint(IntX,IntY).

overlap(IntX,IntY) :- alan_disjoint(IntX,IntY), !, fail.
overlap(_,_).


			% open and closed are opposites
			%  (this is how to flip them)

marker_flip(open,closed) :- !.
marker_flip(closed,open).



/****************************************/
/* X lies in closed or open interval    */
/****************************************/


		% Worst case default for intervals

default_interval(i(open,neginfinity,infinity,open)).


		% Let's try to do better.

find_int(X,Interval)
     :-	find_int2(X,Result),		% guarantee mode (+,-)
	Interval = Result.


			% Catch variables (shouldn't be there!)

find_int2(V,_)
     :-	var(V),
	!,
	error('Interval package given variable: %w',[V],fail).

			% Base cases
			%  Numbers have point intervals
			%  Symbols (atoms) have various special cases

find_int2(X,i(closed,X,X,closed)) :- ok_number(X), !.

find_int2(X,Interval) :- atom(X), !, find_simple_int(X,Interval).


			% Special case normalisation

			% Convert ^(-1) to 1/

find_int2(X^(-1), Int) :- !,
	find_int2(1/X, Int).

			% Deal with exponentials to even power

find_int2(X^N, i(L,B,T,R)) :- 
	even(N), !,
	find_int(abs(X), i(Lx,Bx,Tx,Rx)),
	calc(^,[b(Bx,Lx),b(N,closed)],b(B,L)),
	calc(^,[b(Tx,Rx),b(N,closed)],b(T,R)).

			% Convert cosecant to sine

find_int2(csc(X), Int) :- !, find_int2(1/sin(X), Int).

			% Convert secant to cosine

find_int2(sec(X), Int) :- !, find_int2(1/cos(X), Int).

			% Convert cotangent to tangent

find_int2(cot(X), Int) :- !, find_int2(1/tan(X), Int).


			% General case
			%  Recursively find intervals for arguments and
			%  then int_apply to sort this out. This will use
			%  monotonicity of F to calculate interval of Term
			%  from arguments.

find_int2(Term,Int) :-
	find_int_args(Term,F,IntList),
	int_apply(F,IntList,Int),
	!.


			% If the general case fails

find_int2(sin(_X), i(closed,(-1),1,closed)) :- !.
find_int2(cos(_X), i(closed,(-1),1,closed)) :- !.

find_int2(_X,Default) :- default_interval(Default).



			% Find a list of intervals corresponding to the
			%  arguments of Term. Also return the functor.

find_int_args(Term,Fn,IntList)
     :-	functor(Term,Fn,Arity),
	find_int_args(1,Arity,Term,IntList).


find_int_args(N,Max,_,[]) :- N > Max, !.

find_int_args(N,Max,Term,[Int|IntRest])
     :-	arg(N,Term,Arg),
	find_int2(Arg,Int),
	N1 is N+1,
	find_int_args(N1,Max,Term,IntRest).



			% Find the interval for a simple symbol
			%  This involves looking to see if we know
			%  anything special about the symbol which will
			%  help us.
			% Ad hoc patch for gravity - proper solution means
			%  allowing equations between quantities and defining
			%  g as measure(g,32,ft/sec^2).
			% pi and e have explicit conservative intervals
			% Otherwise try to classify symbol (if it is an angle)
			% Otherwise assume all quantities are positive
			%  	(possibly extreme?)
			% If there is no useful info we must use the default.

find_simple_int(g, i(open,1,infinity,open)) :- !.

find_simple_int(e, i(open,5/2,3,open)) :- !.

find_simple_int(pi,i(open,3,10/3,open)) :- !.

find_simple_int(X,Int) :- classify(X,Int), !.

find_simple_int(M,i(open,0,infinity,open)) :-
	measure(Q,M), quantity(Q),
	!,
	make_assumption_positive(M).

find_simple_int(_X,Default) :- default_interval(Default).



			% Make and remember assumption

make_assumption_positive(X) :- assumed_positive(X), !.

make_assumption_positive(X)
     :-	assert( assumed_positive(X) ),
	trace_press('I assume %t positive.\n',[X],1).



/*************************************************************/
/* Find interval of function from intervals of its arguments */
/*************************************************************/


				% Simple case

int_apply(F,Region,Int) :-
	mono(F,Is,Mono),
	all_are_contained(Region,Is),
	!,
	find_limits(F,Region,Mono,Int).

				% Complex Case

int_apply(F,Region,Int) :-
	mono(F,MRegion,_Mono),
	make_regions(Region,MRegion,NewRegions),
	int_apply_all(NewRegions,F,IntervalSet),
	!,
	gen_combine(IntervalSet,Int).



			% int_apply all intervals in a set (list)

int_apply_all([],_,[]).

int_apply_all([Region1|Rest],F,[Int1|IRest])
     :-	int_apply(F,Region1,Int1),
	int_apply_all(Rest,F,IRest).



			% All the argument intervals are sub intervals of
			%  the corresponding monotonic intervals for the
			%  function (from mono). (ie maplist sub_int down
			%  the two "argument" lists).

all_are_contained([],[]).

all_are_contained([ArgInt|ArgRest],[FInt|FRest])
     :-	sub_int(ArgInt,FInt),
	all_are_contained(ArgRest,FRest).



			% Given the list of actual intervals and the list
			%  of monotonic intervals for the function build
			%  a set of similar interval lists, derived from the
			%  actual interval list, but such that each element
			%  of each list in the set is wholly inside or outside
			%  its corresponding monotonic function interval.
			% This amounts to case splitting the actual interval
			%  list into a set of intervals for more tractable
			%  (sub) regions in the nD space.
			% Implemented by splitting lists to form a list of
			%  sets and taking the nD cartesian product. Note
			%  that both split/4 and cartesian_product/4 perform
			%  order reversals - which cancel each other out.

make_regions(Region,MRegion,NewRegions)
     :-	split(Region,MRegion,[],ListOfSets),
	cartesian_product(ListOfSets,[],NewRegions,[]).



			% Given the list of actual intervals and the list of
			%  monotonic intervals for the function, we build
			%  a list of n sets, where n is the arity of the 
			%  function (ie the length of the lists) and where
			%  each set contains intervals which are wholly inside
			%  or outside the corresponding monotonic function
			%  intervals, such that the intervals in each set
			%  would combine to form the corresponding actual
			%  interval.
			% The combining property follows from the way we split
			%  up the actual intervals.
			% The sets produced at the moment will only ever have
			%  number of members m such that: 1 =< m =< 3.
			%  The following special representations are used for
			%  these cases:
			%  		singleton(A)
			%  		pair(A,B)
			%  		triple(A,B,C)
			%  In fact the code will currently never produce sets
			%  of 3 elements (triples), but I (Lawrence) think
			%  this is probably a bug so have left the option, and
			%  this comment, around til we see.
			% Note that the list of sets built will be in reverse
			%  order compared with the "argument" lists. This is
			%  is implemented by an extra accumulator argument
			%  (should be [] to start with) onto which each Set
			%  is pushed.

split([],[],Result,Result).

split([ArgInt|ArgRest],[FInt|FRest],Sofar,Result)
     :-	split1(ArgInt,FInt,Set),
	split(ArgRest,FRest,[Set|Sofar],Result).


				% Intx wholly within Int

split1(Intx,Int,singleton(Intx)) :-
	sub_int(Intx,Int),
	!.

				% Intx and Int overlap with Intx leftmost

split1(i(Lx,Bx,Tx,Rx), i(L,B,T,R), pair(i(L,B,Tx,Rx),i(Lx,Bx,B1,L1)) ) :-
	marker_flip(R,R1), marker_flip(L,L1), 
	marker_flip(Lx,Lx1),
	correct(B,B1),
	less_than(b(Tx,Rx),b(T,R1)),
	\+ less_than(b(Tx,Rx),b(B,L)),
	less_than(b(Bx,Lx1),b(B,L)), !.



			% Given a list of n sets produce the a set of the
			%  elements from the nD cartesian product of the sets.
			%  The incoming sets are represented with special
			%  functors as there are only a few special cases (see
			%  split). The resulting product set is represented as
			%  a list. Each element will itself be a list (of n
			%  intervals) where the order of this element list will 
			%  be the reverse of the order in which the items
			%  were found in the original list of sets.
			% The implementation involves an accumulator for the
			%  (partial) element being built and uses the 
			%  difference list technique to build the final set
			%  of elements (repn as a list).

cartesian_product([],Element,[Element|Z],Z).

cartesian_product([First|Rest],PartialElement,ProductSet,Z)
     :-	cart_prod(First,Rest,PartialElement,ProductSet,Z).



cart_prod(singleton(A),Rest,PartialElement,PSet,Z)
     :-	cartesian_product(Rest,[A|PartialElement],PSet,Z).

cart_prod(pair(A,B),Rest,PartialElement,PSet0,Z)
     :-	cartesian_product(Rest,[A|PartialElement],PSet0,PSet1),
	cartesian_product(Rest,[B|PartialElement],PSet1,Z).

cart_prod(triple(A,B,C),Rest,PartialElement,PSet0,Z)
     :-	cartesian_product(Rest,[A|PartialElement],PSet0,PSet1),
	cartesian_product(Rest,[B|PartialElement],PSet1,PSet2),
	cartesian_product(Rest,[C|PartialElement],PSet2,Z).



			% Calculate Bottom and Top of Interval

find_limits(F,Region,Mono,Int) :-
	limits(bottom,F,Region,Mono,b(B,L)),
	limits(top,F,Region,Mono,b(T,R)), 
	clean_up(i(L,B,T,R), Int).


			% Hack to clear up various funnies

clean_up(i(_,undefined,_,_), Int) :- !, default_interval(Int).
clean_up(i(_,_,undefined,_), Int) :- !, default_interval(Int).
clean_up(i(L,B,0,R), i(L,B,-(0),R)) :- !.
clean_up(Int, Int).

correct(0,-(0)) :- !.
correct(B,B) :- !.


			% Calculate limit for a particular boundary

limits(TopBot,F,Region,Mono,Boundary)
     :-	get_bnds(Mono,TopBot,Region,BoundaryList),
	calc(F,BoundaryList,Boundary).



			% Form a boundary list from an interval list
			%  given various details - up+down x top+bottom.

get_bnds([],_,[],[]).

get_bnds([Mono|MRest],TopBot,[Int|IRest],[Bnd|BRest])
     :-	updown_flip(TopBot,Mono,NewMono),
	get_bnd(NewMono,Int,Bnd),
	get_bnds(MRest,TopBot,IRest,BRest).


	updown_flip(top,UD,UD).
	updown_flip(bottom,up,down) :- !.
	updown_flip(bottom,down,up).


	get_bnd(up,  i(_L,_B,T,R), b(T,R)).
	get_bnd(down,i(L,B,_T,_R), b(B,L)).




/*****************************************/
/*	Manipulating Boundaries		 */
/*****************************************/


			% Put boundaries in order

					% Boundaries are identical
order(Bnd,Bnd,Bnd,Bnd) :- !.
					% One of Mis is closed
order(b(N,_M1),b(N,_M2),b(N,closed),b(N,closed)) :- !.
					% Numbers are different, N1 smallest
order(b(N1,M1),b(N2,M2),b(N1,M1),b(N2,M2)) :-
	eval(N1 < N2), !.
					% N2 is smallest
order(b(N1,M1),b(N2,M2),b(N2,M2),b(N1,M1)).



			% Ordering of boundaries
			%  (assumes intervals are consecutive)

less_than(b(X,Mx),b(Y,My)) :- 
	comb([Mx,My],M),
	less_than_eval(M,X,Y).


less_than_eval(open,X,Y) :- eval( X =< Y ).

less_than_eval(closed,X,Y) :- eval( X < Y ).



			% Apply Function F to a boundary list
			%  Do this by combining the boundary markers and
			%  applying F to the numbers.

calc(F,BoundaryList,b(X,M)) :-
	breakup_bnds(BoundaryList,Markers,Numbers),
	comb(Markers,M),
	Term =.. [F|Numbers],
	eval(Term,X),
	!.


breakup_bnds([],[],[]).

breakup_bnds([b(N,M)|Rest],[M|MRest],[N|NRest])
     :-	breakup_bnds(Rest,MRest,NRest).



			% Combine boundary markers
			%  Result = open if any of the inputs is open

comb(MarkerList,Result) :- memberchk_press(open,MarkerList), !, Result = open.

comb(_,closed).



/**********************************************/
/* Monotonicity of Functions in each Interval */
/**********************************************/

/* unary minus */
mono(-, [i(closed,neginfinity,infinity,closed)], [down]).

/* addition */
mono(+,[i(closed,neginfinity,infinity,closed), 
	i(closed,neginfinity,infinity,closed)], [up,up]).

/* binary minus */
mono(-,[i(closed,neginfinity,infinity,closed),
        i(closed,neginfinity,infinity,closed)], [up,down]).

/* absolute value */
mono(abs,[i(closed,neginfinity,-(0),closed)], [down]).
mono(abs,[i(closed,0,infinity,closed)], [up]).

/* multiplication */
mono(*,[i(closed,0,infinity,closed), i(closed,0,infinity,closed)], 
	[up,up]).
mono(*,[i(closed,0,infinity,closed), i(closed,neginfinity,-(0),closed)], 
	[down,up]).
mono(*,[i(closed,neginfinity,-(0),closed), i(closed,0,infinity,closed)], 
	[up,down]).
mono(*,[i(closed,neginfinity,-(0),closed), i(closed,neginfinity,-(0),closed)], 
	[down,down]).



/* division */
mono(/,[i(closed,0,infinity,closed), i(closed,0,infinity,closed)], 
	[up,down]).
mono(/,[i(closed,0,infinity,closed), i(closed,neginfinity,-(0),closed)], 
	[down,down]).
mono(/,[i(closed,neginfinity,-(0),closed), i(closed,0,infinity,closed)], 
	[up,up]).
mono(/,[i(closed,neginfinity,-(0),closed), i(closed,neginfinity,-(0),closed)], 
	[down,up]).


/* exponentiation */
mono(^,[i(open,0,infinity,closed),i(closed,0,infinity,closed)], 
	[up,up]).
mono(^,[i(open,0,infinity,closed),i(closed,neginfinity,-(0),closed)],
	[down,up]).


/* logarithm */
mono(log,[i(closed,0,infinity,closed),i(closed,0,infinity,closed)], 
	[down,up]).

/* sine */
mono(sin,[i(closed,(-90),90,closed)],[up]).
mono(sin,[i(closed,90,270,closed)],[down]).
mono(sin,[i(closed,270,450,closed)],[up]).

/* cosine */
mono(cos,[i(closed,0,180,closed)],[down]).
mono(cos,[i(closed,180,360,closed)],[up]).

/* tangent */
mono(tan,[i(open,(-90),90,open)],[up]).
mono(tan,[i(open,90,270,open)],[up]).
mono(tan,[i(open,270,450,open)],[up]).

/* inverse sine */
mono(arcsin,[i(closed,(-1),1,closed)],[up]).

/* inverse cosine */
mono(arccos,[i(closed,(-1),1,closed)],[down]).

/* inverse tangent */
mono(arctan,[i(open,neginfinity,infinity,open)],[up]).

/* inverse cosecant */
mono(arccsc,[i(closed,neginfinity,(-1),closed)],[down]).
mono(arccsc,[i(closed,1,infinity,closed)],[down]).

/* inverse secant */
mono(arcsec,[i(closed,neginfinity,(-1),closed)],[up]).
mono(arcsec,[i(closed,1,infinity,closed)],[up]).

/* inverse cotangent */
mono(arccot,[i(closed,neginfinity,-(0),open)],[down]).
mono(arccot,[i(open,0,infinity,closed)],[down]).

	


/*************************************************/
/*  Calculate Interval of Angle from Curve Type  */
/*************************************************/


			% We classify a symbol using semantic information
			%  from the (Mecho) database. Calls which are to
			%  this database (notionally, Press does not really
			%  share the same object-level database) are marked
			%  as such.
			% This method is only appropriate if the symbol is an
			%  <angle>, and tries to find the interval of the
			%  angle using general principles about curve types.

classify(Angle, Int ) :-
	measure(Q, Angle ),			% database
	angle(_Point, Q, Curve ), !,		% database
	interval(angle, Curve, Int ).

classify(Angle, Int ) :-
	measure(Q, Angle ),			% database
	incline(Curve, Q, _Point ), !,		% database
	interval(incline, Curve, Int ).



			% Find interval from curve shape

				% For simple curves
interval(AI, Curve, Int ) :-
	concavity(Curve, Conv ),		% database
	slope(Curve, Slope ), !,		% database
	quad(AI, Slope, Conv, Int ).

				% For complex curves
interval(AI, Curve, Int ) :-
	partition(Curve, Clist ), !,		% database
	collect_intervals(Clist, AI, Rlist),
	gen_combine(Rlist, Int ).



			% Collect up a list of intervals for all the parts
			%  of a partitioned curve.

collect_intervals([],_,[]).

collect_intervals([First|Rest],AI,[FirstInt|RestInt])
     :-	interval(AI,First,FirstInt),
	collect_intervals(Rest,AI,RestInt).



			% Information about properties of simple curves
			%  The interval depends on both the slope and the
			%  concavity.

quad(angle,left,right,i(closed,0,90,closed)) :- !.
quad(incline,left,right,i(closed,90,180,closed)) :- !.

quad(angle,right,right,i(closed,90,180,closed)) :- !.
quad(incline,right,right,i(closed,180,270,closed)) :- !.

quad(angle,left,left,i(closed,180,270,closed)) :- !.
quad(incline,left,left,i(closed,270,360,closed)) :- !.

quad(angle,right,left,i(closed,270,360,closed)) :- !.
quad(incline,right,left,i(closed,0,90,closed)) :- !.

quad(angle,left,stline,i(open,180,270,open)) :- !.
quad(incline,left,stline,i(open,270,360,open)) :- !.

quad(angle,right,stline,i(open,270,360,open)) :- !.
quad(incline,right,stline,i(open,0,90,open)) :- !.

quad(angle,hor,stline,i(closed,270,270,closed)) :- !.
quad(incline,hor,stline,i(closed,0,0,closed)) :- !.

quad(angle,vert,stline,i(closed,180,180,closed)) :- !.
quad(incline,vert,stline,i(closed,270,270,closed)) :- !.



/* JOBS TO DO

	write symbolic version for finding max/mins

	use monotonicity in > >= etc Isolation rules
*/

assumed_positive(_) :- fail.

