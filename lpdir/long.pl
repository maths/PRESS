%   File   :  /usr/bs/lpdir/long.pl
%   Author : Richard O'Keefe
%   Updated: Tue Oct 15 11:49:39 1985
%   Purpose: Arbitrary precision rational arithmetic package.

/*
Copyright (C) 1981 - R.A.O'Keefe.               

        Designed and written by Richard O'Keefe.
        Scenery by Lawrence Byrd.

        This package provides arithmetic for arbitrary precision rational
        numbers.  The normal domain of prolog 'integers' is extended to
        full rational 'numbers'.  This domain includes all Prolog integers.
        The predicate:

                        ok_number(N)

        will recognise any number in this extended domain.
        Rational numbers are produced by using the predicates

                        eval(Command)

                        eval(Expression,Answer)

        Expression can involve any form of rational number, whether such
        numbers can be represented by Prolog integer or not.  Any form of
        number produced as output by "eval" is acceptable as input to it.

        For convenience the Answer produced by eval is normalised as follows:

        a) Integers X (where |X| <= 99999) are represented as Prolog integers;

        b) 1/0, 0/0, -1/0 are represented as infinity, undefined, neginfinity;

        c) All other numbers are represented as full rationals in reduced form
           i.e. numerator and denominator are relatively prime.

        In the current representation, one normalised number will unify with
        another (including an integer) iff the two numbers are equal.  But it
        is better to test for equality between arbitrary numbers by calling

                        eval(N1=:=N2)

        which also handles infinity & undefined, and is guaranteed to work.|
        Once created, representations of rational numbers can be passed round
        your program, used with eval, or printed.  The predicate

                        portray_number(Number)

        will pretty-print arbitrary numbers, and will fail for anything
        else.  In particular, it will not evaluate an expression.  (But
        eval(write(Expr)) combines evaluation and printing if you want.)
        If this is connected up to your general "portray" mechanism, you
        will never have to see the internal representation of rationals.
        It is ill-advised to write procedures which assume knowledge of
        this internal representation as it is subject to change (rarely),
        not to mention that such activities are against all the principles
        of abstraction and structured programming.

   NB   Note that eval/1 and eval/2 will only evaluate fully numeric
        expressions. If there is some garbage in the expression (such
        as an atom) then no evaluation at all occurs and the whole
        input expression is returned untouched. If you want to evaluate
        mixed symbolic and numeric expressions then use tidy/2 (from
        TIDY.PL) which is designed for this purpose.

FIXES

[3 April 81]

        Added the functions numer(_), denom(_) and sign(_) to the 
        evaluator (ie eva2).

[8 April 81]

        removed choice-points from comq, and corrected sign(.).
        replaced the log routine completely.

[14 April 81]

        changed all XXXr routines to XXXn (for Natural or zero)
        changed all XXXs routines to XXXm (for Modified (Natural routines))
        changed all XXXl routines to XXXz (for the ring Z of integers)
        replaced "digits" by "conn" as I've meant to for some time.
        removed experimental 'xwd' code which doesn't work compiled.  Eheu.
        changed estq,chkq,gest to estd,chkd,estg (estimate division digit,
        check digit, estimate Gcd) to avoid confusion; they don't use rationals.
        rewrote norm_number and renamed it to standardise.
        laid the trig routines out in MY style not Lawrence's.
        Increased the radix from 10,000 to 100,000 after fixing addn to use
        unsigned numbers.

[21 April 81]

        Continued tidying things up.
        made 0^(1/N) = 0; this was an oversight.
        added new xwd(,) code in eva2, and beautified portray_number.

[8 July 81]

        fixed mode error bug in eva2(abs(_),_).  Foolish oversight.

[9 Sept 81]

        fixed negative number bugs in arccos and arcsin.  How long have
        these been around without anyone (except Bernard) noticing?
        Also shunted some cuts around in the same general area.

[13 Sept 81]

        corrected typo {da=>Da} in gcdq/4.

[2 Dec 81]

        corrected a benign bug in number/5 (100000 had been written
        where R should have been), and some minor cosmetic changes.
        Unified error reporting into long_error[_message].  Added a
        few mode declarations for trig functions.

[9 Dec 81]

        when writing eval up for EuroSam, discovered that logs aren't
        handled properly.  Rewrote absq and logq to return 'undefined'
        in more cases, instead of failing.

[27 July 82]

        changed prnq/3 to portray_number/3 and laid it out properly.
        changed prin/1 to putn/1 (put Natural) to avoid conflict elsewhere.     
        Made this stuff call put/1 where it made sense, and used ASCII
        codes instead of strings.  Don't know if it matters, really.
        Also rewrote arctaneval completely, so that it should succeed in a
        few more cases.  Really, the trig stuff is PITIFUL.  Please, will
        someone do a proper job of it (preferrably someone PAID to do it).

[30 August 82]

        fixed bug in gcdq/4 so that gcd(1/2,1/4) = 1/4, not 1/2!

[12 July 1983]

        arctaneval used to call addn/4, and there isn't any such
        predicate.  Made it call addn/5.


:-  public
        number/1,               %  number(N) <=> N is a number
        eval/1,                 %  eval(E) => E/rational-eval is true
        eval/2,                 %  eval(E,A) => (A is E)/rational-eval
        portray_number/1,       %  writes rational assumed radix 100000.
%                               %  Lawrence's Low Level TIDY interface
        add/3,                  %  add(A,B,C) => (C is A+B)/rational-eval
        multiply/3,             %  similar for *.  NB A,B must be numbers
        power/3.                %  similar for ^.  NOT general terms.
*/

/* OPERATORS */

:- op(300, xfx, div).           %  integer quotient A div B = fix(A/B).


/* MODES and types */
/*
%   The comments at the right give the argument types for each predicate.
%   The predicates can of course be called with any arguments, but these
%   are the only types they are supposed to work on or deliver.  
%   ? = any Prolog term, possibly including variables.
%   E = an arithmetic Expression, a term.
%   A = a Prolog atom (but not an integer).
%   I = a Prolog integer.  Generally positive, but not always.
%   T = a Truth-value, 'true' or 'false'.
%   S = a Sign, '+' or '-'.  {Sometimes can be 0 or *.}
%   R = a Relational operator, {<, =, >; sometimes =<, >=, =/=}
%   N = a long positive (Natural) number.
%   Q = a rational number.

:- mode

%% Top Level %%

    number(+),                                  % Q
    eval(+),                                    % E
    eval(+, -),                                 % E Q
        eva2(+, -),                             % E Q
            relational_op(+, -, -),             % R R T
            combine_ops(+, +, +, -),            % R R T T
    portray_number(+),                          % Q
        portray_number(+, +, +),                % S N N
            putn(+),                            % N

%% Conversions %%

    number(+, +, ?, ?, ?),                      % Q I S N N
        binrad(+, +, -),                        % I I N
    standardise(+, ?),                          % Q Q

%% Low Level %%

    add(+, +, ?),                               % Q Q Q
    multiply(+, +, ?),                          % Q Q Q
    power(+, +, ?),                             % Q Q Q

%% Rational Arithmetic %%

    mod2(+, ?),                                 % Q I
    intq(+,    +, -),                           % Q   I Q
    gcdq(+, +, +, -),                           % Q Q I Q
%   invq(+,    +, -),                           % Q   I Q  
    mulq(+, +, +, -),                           % Q Q I Q
    divq(+, +, +, -),                           % Q Q I Q
    divo(+, +, +, -, -),                        % Q Q I Q Q
    powq(+, +, +, -),                           % Q Q I Q
    negq(+,    +, -),                           % Q   I Q
    addq(+, +, +, -),                           % Q Q I Q
    subq(+, +, +, -),                           % Q Q I Q
    comq(+, +, +, ?),                           % Q Q I R
    nthq(+, +, +, -),                           % I Q I Q
        nthn(+, +, +, -),                       % I N I N
            newton(+, +, +, +, -),              % I N N I N
                newton(+, +, +, +, +, -),       % R I N N I N

  %% Long Arithmetic %%

    addz(+,+, +,+, +, -,-),                     % S N S N I S N
        addn(+, +, +, +, -),                    % N N I I N
            add1(+, +, -),                      % N I N
    comz(+,+, +,+, ?),                          % S N S N R
        comn(+, +, +, ?),                       % N N R R
            com1(+, +, +, -),                   % I I R R
    subz(+,+, +,+, +, -,-),                     % S N S N I S N
        subn(+, +, +, -,-),                     % N N I S N
            subn(+, +, +, +, -,-),              % R N N I S N
                prune(+, -),                    % N N
                subp(+, +, +, +, -),            % N N I I N
                    sub1(+, +, -),              % N I N
    sign(+, +, -),                              % S S S
%   mulz(+,+, +,+, +, -,-),                     % S N S N I S N  
        muln(+, +, +, -),                       % N N I N
            muln(+, +, +, +, -),                % N N N I N
                mul1(+, +, +, -),               % N I I N
                    mul1(+, +, +, +, -),        % N I I I N
    powz(+,+, +, +, -,-),                       % S N I I S N
        pown(+, +, +, +, -),                    % I N N I N
    divz(+,+, +,+, +, -,-, -,-),                % S N S N I S N S N
        divn(+, +, +, -, -),                    % N N I N N
            conn(?, ?, ?),                      % I N N
        %   both +, +, - and -, -, + are used.
            div1(+, +, +, -, -),                % N I I N N
            divm(+, +, +, -, -),                % N N I N N
                div2(+, +, +, -, -),            % N N I N N
                    estd(+, +, +, -),           % N N I I
                    chkd(+, +, +, +, +, -, -),  % N N I I I I N
%  gcdz(+,+, +,+, +, -, -,-, -,-),             % S N S N I N S N S N  
        gcdn(+, +, +, -, -, -),                 % N N I N N N
            gcdn(+, +, +, -),                   % N N I N
                gcdn(+, +, +, +, -),            % R N N I N
                    estg(+, +, +, -),           % N N I I

%% Logarithms %%

    logq(+, +, +, -),                           %  Q Q I Q
        logq(+,+, +,+,+,-),                     %  R R Q Q I Q
        absq(+, -, -),                          %  Q S Q
            logq(+, -,-),                       %  S S N
            oneq(+, -, -),                      %  Q R Q
            ratlog(+, +, +, -),                 %  Q Q I Q
                ratlog(+,+, +,+,+, -),          %  S S Q Q I Q
                    lograt(+,+,+, -,-),         %  Q Q I N N
                        loop(+, +, +, -),       %  N N I N
                            loop(+,+,+,+,-),    %  N N N I N
                        logn(+,+,+,+,-),        %  Q I Q Q I I 

%% Trigonometry %%

    sineval(+, -),                              %  Q Q
    coseval(+, -),                              %  Q Q
    taneval(+, -),                              %  Q Q
    arcsineval(+, -),                           %  Q Q
    arccoseval(+, -),                           %  Q Q
    arctaneval(+, -),                           %  Q Q
        arctaneval(+, +, -, -),                 %  N N N N
        sineval1(?, ?),                         %  Q Q

%% Error handing %%

    long_error(+, ?),                           %  A ?
        long_error_message(+, -).               %  A A
*/

/* Implementation

        The internal representation for rationals is of the form:

                number(Sign, Numerator, Denominator)

                    where
                        Sign is in {+,-}
                        Numerator is a list of (Prolog) integers
                        Denominator is a list of (Prolog) integers

        The lists of Prolog integers represent arbitrary precision unsigned
        long integers

                eg [n0,n1,....,nz]

                    is n0+R*(n1+R*(....R*nz)...)

                    where R is the Radix.

        The Radix used in the current version is 100000. Most of the code
        in this module is completely independent of the radix - it all
        uses the value passed in by the top level procedures. However the
        printing routine currently assumes that the radix is a power of
        10 as this makes things easier. In general the radix must be such
        that both:

                        Radix^2 - 1
                   and  Radix*2 + 1

                                are representable as Prolog integers (which
        are 18 bit quantities on the DEC10). This is a little restrictive,
        however, and this implementation only assumes that Radix^2 - 1 is
        "obtainable" as an intermediate during Prolog arithmetic. On the
        DEC10 intermediate results can be 36 bit quantities and so 100000
        becomes a suitable radix.

        The code actually unpacks the number terms into their separate
        bits for all the low level operations. At this stage the following
        additional number forms are appropriately converted

                <integer>   -   (Prolog integers)
                infinity    -   represented as +1/0
                neginfinity -   represented as -1/0
                undefined   -   represented as  0/0

        The treatment of these strange things is not supposed to be
        mathematically beautiful, but sensible things happen using
        this representation. They are strictly an extension to the
        rationals and could be removed (with eval failing should 0
        denominator numbers ever get produced) if desired.

        Results from eval are normalised before being returned.
        This operation reverses the above transformation except that
        only integers within the range -99999 to +99999 are turned
        back into Prolog integers.
*/


%% TOP LEVEL PREDICATES %%



                        % Number recognition predicate

ok_number(N)      :- integer(N), !.
ok_number(number(S,N,D))   :- !.
ok_number(infinity)        :- !.
ok_number(neginfinity)     :- !.
ok_number(undefined)       :- !.



                        % Simple eval interpreter with various features.

eval(Var)      :- var(Var), !, long_error(eval, Var).
eval(B is Y)   :- !, eval(Y, B).
eval(write(Y)) :- !, eval(Y, B), print(B).
eval(even(X))  :- !, eva2(X, A), !, mod2(A, 0).
eval(odd( X))  :- !, eva2(X, A), !, mod2(A, 1).
eval(compare(R,A,B)) :-
                  !, eva2(X, A), eva2(Y, B), comq(A, B, 100000, S), !, R=S.
eval(Term)     :- Term =.. [F,X,Y], relational_op(F, R, Flag),
                  !, eva2(X, A), eva2(Y, B), comq(A, B, 100000, S), !,
                  combine_ops(R, S, Flag, true).

        mod2(number(_,_,[]),    M) :- !, fail.
        mod2(number(_,[],_),    0).
        mod2(number(_,[L|_],_), M) :- M is L mod 2.


                        % General evaluation of rational expressions

eval(Exp, Ans) :-       %    Hope for the best
        eva2(Exp, N),
        standardise(N, A), !,
        Ans = A.
eval(Exp, Exp).         %    Cannot evaluate so leave alone
%       ttynl, display('[Couldn''t evaluate: '),
%       print(Exp), ttyput("]"), ttynl, ttynl.



eva2(Var, C)     :- var(Var), !, long_error(eva2, Var).
eva2(X+Y, C)     :- !, eva2(X, A), eva2(Y, B), addq(A, B, 100000, C).
eva2(X-Y, C)     :- !, eva2(X, A), eva2(Y, B), subq(A, B, 100000, C).
eva2( -Y, C)     :- !,             eva2(Y, B), negq(   B, 100000, C).
eva2(X*Y, C)     :- !, eva2(X, A), eva2(Y, B), mulq(A, B, 100000, C).
eva2(X/Y, C)     :- !, eva2(X, A), eva2(Y, B), divq(A, B, 100000, C).
eva2(X div Y, C) :- !, eva2(X, A), eva2(Y, B), divo(A, B, 100000, C, _).
eva2(X mod Y, C) :- !, eva2(X, A), eva2(Y, B), divo(A, B, 100000, _, C).
eva2(X++Y, C)    :- !, eva2((X+Y) mod 360, C).
eva2(X--Y, C)    :- !, eva2((X-Y) mod 360, C).
eva2(X^Y, C)     :- !, eva2(X, A), eva2(Y, B), powq(A, B, 100000, C).
eva2(sqrt(Y), C) :- !,             eva2(Y, B), nthq(2, B, 100000, C).
eva2(pi,number(+,[355],[113])) :- !.
eva2(log(X,Y),C) :- !, eva2(X, A), eva2(Y, B), logq(A, B, 100000, C).
eva2(gcd(X,Y),C) :- !, eva2(X, A), eva2(Y, B), gcdq(A, B, 100000, C).
eva2(fix(X), C)  :- !, eva2(X, A),             intq(A,    100000, C).
eva2(sin(X), C)  :- !, eva2(X, A), sineval(A, C).
eva2(cos(X), C)  :- !, eva2(X, A), coseval(A, C).
eva2(tan(X), C)  :- !, eva2(X, A), taneval(A, C).
eva2(arcsin(X),C):- !, eva2(X, A), arcsineval(A, C).
eva2(arccos(X),C):- !, eva2(X, A), arccoseval(A, C).
eva2(arctan(X),C):- !, eva2(X, A), arctaneval(A, C).
eva2(abs(X),   number(+,N, D )) :- !, eva2(X, A), A = number(_,N,D).
eva2(numer(X), number(+,N,[1])) :- !, eva2(X, A), A = number(_,N,_).
eva2(denom(X), number(+,D,[1])) :- !, eva2(X, A), A = number(_,_,D).
eva2(sign(X),  number(S,B,[1])) :- !, eva2(X, A), A = number(S,N,_),
                                      (N=[], B=[]; B=[1]), !.
 eva2(xwd(X,Y),C) :- !, U is (Y mod 262143)>>9, V is (Y mod 262143)/\511,
                    eva2((X*512+U)*512+V, C).
eva2(X,       C) :- number(X, 100000, S, N, D), !, C = number(S, N, D).
eva2(Term,    C) :- Term =.. [F,X,Y], relational_op(F,R, Flag),
                    !, eva2(X, A), eva2(Y, B), comq(A, B, 100000, S), !,
                    combine_ops(R, S, Flag, C).


        relational_op(  =, =, true).
        relational_op( \=, =, false).
        relational_op(  <, <, true).
        relational_op( >=, <, false).
        relational_op(  >, >, true).
        relational_op( =<, >, false).
        relational_op(=:=, =, true).
        relational_op(=\=, =, false).

        combine_ops(Sign, Sign, Flag,  Ans) :- !, Ans = Flag.
        combine_ops(Sign, Diff, true,false) :- !.
        combine_ops(Sign, Diff, false,true) :- !.



                        % Pretty-Print a number.
                        %  This now always forces parentheses. When a
                        %  proper general portray handler is written
                        %  this could be made cleverer (as it once was).
                        %  The magic numbers are 40 = "(", 41 = ")",
                        %  45 = "-", 47 = "/", 48 = "0" {ASCII codes}.

portray_number(A) :-
        number(A, 100000, S, N, D),     
	flag(number_portray,on,on),	!,
        portray_number(S, N, D).

        portray_number(_,[],  []) :- !,         %  0/0 = undefined
                write(undefined).
        portray_number(+, N,  []) :- !,         % +N/0 = +infinity
                write(infinity).
        portray_number(-, N,  []) :- !,         % -N/0 = -infinity
                write(neginfinity).
        portray_number(+, N, [1]) :- !,         % +N/1 = a +ve integer
                putn(N).
        portray_number(-, N, [1]) :- !,         % -N/1 = a -ve integer
                put(45), putn(N).
        portray_number(+, N,  D ) :- !,         % +N/D = a +ve rational
                put(40), putn(N), put(47), putn(D), put(41).
        portray_number(-, N,  D ) :- !,         % -N/D = a -ve rational
                put(40), put(45), putn(N), put(47), putn(D), put(41).

                putn([]   ) :- !, put(48).
                putn([D]  ) :- !, write(D).
                putn([D|T]) :- 
                        putn(T),
                        D4 is (D//10000)       +48, put(D4),     % D4*10^4 +
                        D3 is (D//1000) mod 10 +48, put(D3),     % D3*10^3 +
                        D2 is (D//100) mod 10  +48, put(D2),     % D2*10^2 +
                        D1 is (D//10) mod 10   +48, put(D1),     % D1*10^1 +
                        D0 is (D) mod 10      +48, put(D0).     % D0*10^0 = D.

%% INTERFACE CONVERSIONS %%

                        % Conversion of a number, of any form, to its
                        %  essential bits.

number(infinity,        R, +,[1], []) :- !.
number(neginfinity,     R, -,[1], []) :- !.
number(undefined,       R, +, [], []) :- !.
number(number(S, N, D), R, S,  N,  D) :- !.
number(N, R, +, L, [1]) :- integer(N), N >= 0, !,          binrad(N, R, L).
number(N, R, -, L, [1]) :- integer(N), N  < 0, !, M is -N, binrad(M, R, L).

        binrad(0, R, [])    :- !.
        binrad(N, R, [M|T]) :- K is N//R, M is N mod R, !, binrad(K, R, T).



                        % Normalise a number

standardise(number(S,[N],[1]), Ans) :- !,
        (   S = '+', Ans = N
        ;   S = '-', Ans is -N
        ),  !.
standardise(number(_, [],[1]),  0 ) :- !.
standardise(number(S,  N, []), Ans) :- !,
        (   N =  [], Ans = undefined
        ;   S = '+', Ans = infinity
        ;   S = '-', Ans = neginfinity
        ),  !.
standardise(Number,         Number).




%% LOW LEVEL INTERFACE %%



                        % These routines provide a low level interface
                        %  for procedures which want to operate directly
                        %  on pairs of numbers.
                        % Only currently used by TIDY (27/2/81),
                        %  so only those necessary are provided.

add(A, B, C) :-         % eval(C is A+B).
        addq(A, B, 100000, X),
        standardise(X, C).

multiply(A, B, C) :-    % eval(C is A*B).
        mulq(A, B, 100000, X),
        standardise(X, C).

power(A, N, C) :-       % eval(C is A^B).
        powq(A, N, 100000, X),
        standardise(X, C).

%% BASIC ARITHMETIC OVER RATIONALS %%


                        % Integer part of a rational

intq(A, R, number(S, Q, [1])) :-
        number(A, R, S, N, D),
        divn(N, D, R, Q, _).



                        %   The greatest common divisor of two numbers is
                        %   defined for all pairs of non-zero rationals.
                        %   gcd(X,Y) = Z iff Z > 0 and there are integers
                        %   M,N relatively prime for which X=MZ & Y=NZ.

gcdq(A, B, R, number(+,Nd,Dd)) :-
        number(A, R, _, Na, Da),
        number(B, R, _, Nb, Db),
        gcdn(Da, Db, R, _, Ga, Gb),
        muln(Gb, Na, R, Ma),
        muln(Ga, Nb, R, Mb),
        gcdn(Ma, Mb, R, Nd),
        muln(Gb, Da, R, Dd).

/*      The above seems to be right, but I'm not sure.  This IS right.
gcdq(A, B, R, number(+,Nd,Dd)) :-
        number(A, R, _, Na, Da),        %  |A| = Na/Da
        number(B, R, _, Nb, Db),        %  |B| = Nb/Db
        muln(Na, Db, R, N1),            %  N1 = Na.Db
        muln(Nb, Da, R, N2),            %  N2 = Nb.Da
        gcdn(N1, N2, R, Nc),            %  Nc = gcd(Na.Db, Nb.Da)
        muln(Da, Db, R, Dc),            %  Dc = Da.Db
        gcdn(Nc, Dc, R, _, Nd, Dd).     %  Nd/Dd = Nc/Dc in standard form
*/

/*                      % Take the inverse of a rational

invq(A, R, number(S, D, N)) :-
        number(A, R, S, N, D).

*/

                        % Multiplication of two rationals

mulq(A, B, R, number(Sc, Nc, Dc)) :-
        number(A, R, Sa, Na, Da),
        number(B, R, Sb, Nb, Db),
        sign(Sa, Sb, Sc),
        gcdn(Na, Db, R, _, Na1, Db1),
        gcdn(Da, Nb, R, _, Da1, Nb1),
        muln(Na1, Nb1, R, Nc),
        muln(Da1, Db1, R, Dc).



                        % Division of two rationals

divq(A, B, R, number(Sc, Nc, Dc)) :-
        number(A, R, Sa, Na, Da),
        number(B, R, Sb, Nb, Db),
        sign(Sa, Sb, Sc),
        gcdn(Na, Nb, R, _, Na1, Nb1),
        gcdn(Da, Db, R, _, Da1, Db1),
        muln(Na1, Db1, R, Nc),
        muln(Da1, Nb1, R, Dc).



                        % Quotient and remainder of two rationals

divo(A, B, R, number(Sq,Nq,[1]), number(Sx,Nx,Dx)) :-
        number(A, R, Sa, Na, Da),       %  A = Sa.Na/Da
        number(B, R, Sb, Nb, Db),       %  B = Sb.Nb/Db
        muln(Na, Db, R, N1),            %  A/B = (Sa.Na.Db)/(Sb.Nb.Da)
        muln(Nb, Da, R, D1),            %      = (Sa.N1)/(Sb.D1)
        divz(Sa,N1, Sb,D1, R, Sq,Nq, Sx,Ny),
        muln(Da, Db, R, Dy),            %  A/B = Q + (Sx.Ny)/(Sb.Nb.Da)
        gcdn(Ny, Dy, R, _, Nx, Dx).     %  A = Q.B + (Sx.Ny)/Dy



                        % Exponentiation of rationals
                        %  This is always defined for (positive or
                        %   negative) integer powers, however there
                        %   is a current implementation restiction that
                        %   the power be between -99999 and +99999 (ie
                        %   within the current Radix).
                        %  This may be defined for some rational powers
                        %   but since there are results from this which are
                        %   not representable as rationals it will fail
                        %   in such cases.  The code for rational powers
                        %   relies on numerator and denominator being
                        %   relatively prime, which is standard.

powq(A, B, R, C) :-
        number(B, R, S, N, [1]), !,
        powq(S, N, A, R, C).
powq(A, B, R, C) :-
        number(B, R, S, N, [D]),
        nthq(D, A, R, X), !,
        powq(S, N, X, R, C).

        powq(S, [], A, R, number(+,[1],[1])) :- !.
        powq(+,[N], A, R, number(Sc, Nc, Dc)) :- !,
                number(A, R, Sa, Na, Da),
                powz(Sa, Na, N, R, Sc, Nc),
                pown(N,  Da,[1],R,     Dc).
        powq(-,[N], A, R, number(Sc, Nc, Dc)) :- !,
                number(A, R, Sa, Na, Da),
                powz(Sa, Da, N, R, Sc, Nc),
                pown(N,  Na,[1],R,     Dc).



                        % Negate a rational

negq(A, R, number(Sc, Nc, Dc)) :-
        number(A, R, Sa, Nc, Dc),
        (   Nc = [], Dc = [], Sc = +            %  -undefined=undefined
        ;   sign(Sa, -, Sc)                     %  -0 = -(0) now.
        ),  !.



                        % Addition of two rationals

addq(A, B, R, number(Sc, Nc, Dc)) :-
        number(A, R, Sa, Na, Da),
        number(B, R, Sb, Nb, Db),
        muln(Na, Db, R, Xa),
        muln(Nb, Da, R, Xb),
        addz(Sa,Xa, Sb,Xb, R, Sc,Xc),
        gcdn(Xc, Da, R, _, Nx, Ya),
        gcdn(Nx, Db, R, _, Nc, Yb),
        muln(Ya, Yb, R, Dc), /*Q'*/ Nc/Dc\==[]/[], !.
addq(A, B, R, number(Sc, Nc, [])) :- /*Q'*/
        number(A, R, Sa, Na, Da),
        number(B, R, Sb, Nb, Db),
        (   Na\==[], Nb\==[], Sa==Sb, Sc=Sa, Nc=[1]
        ;   Sc= +, Nc=[]
        ),  !.



                        % Subtraction of two rationals

subq(A, B, R, number(Sc, Nc, Dc)) :-
        number(A, R, Sa, Na, Da),
        number(B, R, Sb, Nb, Db),
        muln(Na, Db, R, Xa),
        muln(Nb, Da, R, Xb),
        subz(Sa,Xa, Sb,Xb, R, Sc,Xc),
        gcdn(Xc, Da, R, _, Nx, Ya),
        gcdn(Nx, Db, R, _, Nc, Yb),
        muln(Ya, Yb, R, Dc), /*Q'*/ Nc/Dc\==[]/[], !.
subq(A, B, R, number(Sc, Nc, [])) :- /*Q'*/
        number(A, R, Sa, Na, Da),
        number(B, R, Sb, Nb, Db),
        (   Na\==[], Nb\==[], Sa\==Sb, Sc=Sa, Nc=[1]
        ;   Sc= +, Nc=[]
        ),  !.



                        % Comparison of two rationals

comq(A, B, R, S) :-
        number(A, R, Sa, Na, Da), /*Q'*/ Na/Da \== []/[],
        number(B, R, Sb, Nb, Db), /*Q'*/ Nb/Db \== []/[],
        muln(Na, Db, R, Xa),
        muln(Nb, Da, R, Xb),    !,
        comz(Sa, Xa, Sb, Xb, S).



                        % Try to find Nth root
                        %  This will fail in cases where the solution is
                        %  not representable as a rational

nthq(N, A, R, number(+, Nr, Dr)) :-
        number(A, R, +, Na, Da), !,
        nthn(N, Na, R, Nr),
        nthn(N, Da, R, Dr).
nthq(N, A, R, number(-, Nr, Dr)) :-
        number(A, R, -, Na, Da), !,
        1 is N mod 2,
        nthn(N, Na, R, Nr),
        nthn(N, Da, R, Dr).

        nthn(N,  [], R,  []) :- !.
        nthn(N, [1], R, [1]) :- !.
        nthn(N,   A, R,   S) :-
                newton(N, A, A, R, S), !,
                pown(N, S, [1], R, B), !, B=A.  % check that S^N=A !

                newton(N, A, E, R, S) :-
                        M is N-1,
                        pown(M, E, [1], R, E1), % E1=E^(N-1)
                        mul1(E1,N, R, D2),      % D2=N.E^(N-1)
                        muln(E, E1,R, E2),      % E2=E^N
                        mul1(E2,M, R, N1),      % N1=(N-1).E^N
                        addn(N1,A, 0, R, N2),   % N2=(N-1).E^N+A
                        divn(N2,D2,R, F, _),    % F = {(N-1).E^N+A}div{N.E^(N-1)}
                        comn(F, E, =,  Z), !,   % F Z E
                        newton(Z, N, A, F, R, S).

                        newton(<, N, A, F, R, S) :- !, newton(N, A, F, R, S).
                        newton(=, N, A, F, R, F) :- !.


                        % Take the logarithm of a rational to a rational base.
                        % This can be expected to fail for almost every pair
                        % of rational numbers.  To keep the search space within


%   logq(B, X, R, L) is true iff
%       B, X, and L are rationals such that B^L = X.
%   This does its best for strange mixtures, like log(-3,-27) = 3.

logq(B, X, R, L) :-
        absq(B, S, C),  %   B S 0 & |B| = C
        absq(X, T, Y),  %   X T 0 & |X| = Y
        logq(S, T, C, Y, R, L).

        %   absq(A, R, S, B) is true iff
        %       A and B are rationals, |A| = B, and
        %       S = {+,-,0,*} as A {<,=,>} 0 or is undefined.

        absq(number(Sa,[],[]),  *, number(+,[],[]))  :- !.
        absq(number(Sa,[],Da),  0, number(+,[],[1])) :- !.
        absq(number(Sa,Na,Da), Sa, number(+,Na,Da)).

        %   logq(S, T, ...) is just a case analysis of logq.

        logq(+, +, B, X, R, L) :- !,
                ratlog(B, X, R, L).
        logq(-, +, B, X, R, L) :- !,
                ratlog(B, X, R, L),
                mod2(L, 0).             %  L must be "even"
        logq(-, -, B, X, R, L) :- !,
                ratlog(B, X, R, L), !,
                mod2(L, 1).             %  L must be "odd"
        logq(+, -, _, _, _, number(+,[],[])) :- !.
        logq(*, _, _, _, _, number(+,[],[])) :- !.
        logq(_, *, _, _, _, number(+,[],[])) :- !.
        logq(0, _, _, _, _, number(+,[],[])) :- !.
        logq(_, 0, B, X, R, number(Z, N,[])) :- !,
                oneq(B, S, _),
                logq(S, Z,N).

                logq(+, -,[1]) :- !.    %  log(B,0) = -inf for 1<B<inf
                logq(-, +,[1]) :- !.    %  log(B,0) = +inf for 0<B<1
                logq(_, +,[]).          %  log(B,0) = ???? otherwise

                %  oneq(A, S, B) is true when A and B are positive
                %  defined rationals, |log A| = log B, and S = sign(log A).

                oneq(number(_, _,[]), *, number(+,[1],[])) :- !.
                oneq(number(_,Na,Na), 0, number(+,Na,Na)) :- !.
                oneq(number(_,Na,Da), +, number(+,Na,Da)) :-
                        comn(Na, Da, =, >), !.
                oneq(number(_,Na,Da), -, number(+,Da,Na)).


                %   ratlog(B, X, R, L) is true iff
                %       B, X > 0 and B^L = X.

                ratlog(B, X, R, L) :-
                        oneq(B, S, C),  %  B S 1 & |log B| = log C
                        oneq(X, T, Y),!,%  X T 1 & |log X| = log Y
                        ratlog(S, T, C, Y, R, L).

                        %  ratlog(S,T, ...) is just a case analysis

                        ratlog(+, +, B, X, R, number(+,N,D)) :- !,
                                lograt(B, X, R, N, D).
                        ratlog(+, -, B, X, R, number(-,N,D)) :- !,
                                lograt(B, X, R, N, D).
                        ratlog(-, +, B, X, R, number(-,N,D)) :- !,
                                lograt(B, X, R, N, D).
                        ratlog(-, -, B, X, R, number(+,N,D)) :- !,
                                lograt(B, X, R, N, D).
                        ratlog(0, _, _, _, _, number(+,[], [])) :- !.
                        ratlog(_, 0, _, _, _, number(+,[],[1])) :- !.
                        ratlog(+, *, _, _, _, number(+,[1],[])) :- !.
                        ratlog(-, *, _, _, _, number(-,[1],[])) :- !.
                        ratlog(_, *, _, _, _, number(+,[], [])) :- !.
                        ratlog(*, _, _, _, _, number(+,[], [])) :- !.

%   lograt(B, X, R, N, D) is true iff
%       B > 1, X > 1 are rationals, B^N = X^D, and gcd(N,D) = 1.

lograt(number(+,Nb,Db), number(+,Nx,Dx), R, [N], [D] ) :-
        gcdn(Db, Nx, R, U), !, U = [1],         %  Db co-prime Nx
        gcdn(Nb, Dx, R, V), !, V = [1],         %  Nb co-prime Dx
        loop(Nb, Nx, R, G), !,
        logn(G, 1, G, Nb, R, D), !,             %  D=log(G,Nb)
        logn(G, 1, G, Nx, R, N), !,             %  N=log(G,Nx)
        pown(N, Db, [1], R, K1),
        pown(D, Dx, [1], R, K2), !,
        K1 = K2.                                %  Db^N = Dx^D

        loop(A, B, R, G) :-
                comn(A, B, =, S), !,
                loop(S, A, B, R, G).

                loop(=, A, B, R, A) :- !.
                loop(<, A, B, R, G) :-
                        divn(B, A, R, Q, X), X = [], !,
                        loop(A, Q, R, G).
                loop(>, A, B, R, G) :-
                        divn(A, B, R, Q, X), X = [], !,
                        loop(Q, B, R, G).

        %   logn(B, N, P, X, R, L) is true iff
        %       X >= B > 1, P = B^N, and X = B^L.

        logn(B, N, X, X, R, N) :- !.
        logn(B, N, P, X, R, L) :-
                comn(P, X, =, <),
                muln(B, P, R, Q),
                M is N+1, !,
                logn(B, M, Q, X, R, L).

%% BASIC ARITHMETIC OVER LONG INTEGERS %%


                        % Addition of two long integers

addz(+,A, +,B, R, +,C) :- !, addn(A, B, 0, R, C).
addz(+,A, -,B, R, S,C) :- !, subn(A, B, R, S, C).
addz(-,A, +,B, R, S,C) :- !, subn(B, A, R, S, C).
addz(-,A, -,B, R, -,C) :- !, addn(B, A, 0, R, C).

        addn([D1|T1], [D2|T2], Cin, R, [D3|T3]) :-
                Sum is D1+D2+Cin,
                (   (Sum mod 262143) >= R, Cout = 1, D3 is (Sum mod 262143)-R
                ;   (Sum mod 262143) <  R, Cout = 0, D3 =  Sum
                ),  !, 
                addn(T1, T2, Cout, R, T3). 
        addn([], L, 0, R, L) :- !.
        addn([], L, 1, R, M) :- !, add1(L, R, M).
        addn(L, [], 0, R, L) :- !.
        addn(L, [], 1, R, M) :- !, add1(L, R, M).

                add1([M|T], R, [N|T]) :- N is M+1, N < R, !.
                add1([M|T], R, [0|S]) :- R is M+1, !, add1(T, R, S).
                add1([],    R, [1]).



                        % Comparison of two long integers

comz(_,[],_,[],S) :- !, S = '='.        % -0 = 0 now, alas.
comz(+,A, +,B, S) :- !, comn(A, B, =, S).
comz(+,A, -,B, >).
comz(-,A, +,B, <).
comz(-,A, -,B, S) :- !, comn(B, A, =, S).

        comn([D1|T1], [D2|T2], D, S) :-
                com1(D1, D2, D, N), !,
                comn(T1, T2, N, S).
        comn([],      [],      D, S) :- !, S = D.
        comn([],      L,       D, <) :- !.
        comn(L,       [],      D, >) :- !.

                com1(X, X, D, D) :- !.
                com1(X, Y, D, <) :- X < Y, !.
                com1(X, Y, D, >) :- X > Y, !.



                        % Subtraction of two long integers

subz(+,A, +,B, R, S,C) :- !, subn(A, B, R, S, C).
subz(+,A, -,B, R, +,C) :- !, addn(A, B, 0, R, C).
subz(-,A, +,B, R, -,C) :- !, addn(B, A, 0, R, C).
subz(-,A, -,B, R, S,C) :- !, subn(B, A, R, S, C).

        subn(A, B, R, S, C) :-
                comn(A, B, =, O), !,  %  Oh for Ordering
                subn(O, A, B, R, S, C).

                subn(<, A, B, R, -, C) :- !, subp(B, A, 0, R, D), prune(D, C).
                subn(>, A, B, R, +, C) :- !, subp(A, B, 0, R, D), prune(D, C).
                subn(=, A, B, R, +,[]) :- !.

                        prune([0|L], M ) :- !,
                                prune(L, T),
                                (T = [], M = []; M = [0|T]).
                        prune([D|L], [D|M]) :- !,
                                prune(L, M).
                        prune([],    []) :- !.

                subp([D1|T1], [D2|T2], Bin, R, [D3|T3]) :-
                        S is D1-D2-Bin,
                        (   S >= 0, Bout = 0, D3 =  S
                        ;   S <  0, Bout = 1, D3 is S+R
                        ),  !,
                        subp(T1, T2, Bout, R, T3).
                subp(L, [], 0, R, L) :- !.
                subp(L, [], 1, R, M) :- !, sub1(L, R, M).

                        sub1([0|T], R, [K|S]) :- !, K is R-1, sub1(T, R, S).
                        sub1([N|T], R, [M|T]) :- M is N-1.



                        % Multiplication of Signs

sign(S, S, +) :- !.
sign(S, T, -) :- !.



                        % Multiplication of two long integers
/*
mulz(S,A, T,B, R, U,C) :-|
        sign(S, T, U), !,
        muln(A, B, R, C).
*/
        muln([], B, R, []) :- !.
        muln(A, [], R, []) :- !.
        muln(A,  B, R,  C) :- !, muln(A, B, [], R, C).

        muln([D1|T1], N2, Ac, R, [D3|Pr]) :-
                mul1(N2, D1, R, P2),
                addn(Ac, P2, 0, R, Sm),
                conn(D3, An, Sm), !,
                muln(T1, N2, An,R, Pr).
        muln([],      N2, Ac, R, Ac) :- !.

                mul1(A, 0, R, []) :- !.
                mul1(A, M, R, Pr) :- !,
                        mul1(A, M, 0, R, Pr).

                        mul1([], M, 0, R, []) :- !.
                        mul1([], M, C, R, [C]) :- !.
                        mul1([D1|T1], M, C, R, [D2|T2]) :-
                                D2 is (D1*M+C) mod R,
                                Co is (D1*M+C)  //  R,
                                mul1(T1, M, Co, R, T2).



                        % Exponentiation of a long integer to a short
                        %  (Prolog) integer. Note that this means the
                        %  power must be less than 100000 (current radix).
                        %  This code should always be called with positive
                        %  powers.

powz(-,A, N, R, -,C) :-
        N mod 2 =:= 1, !,
        pown(N, A, [1], R, C).
powz(S,A, N, R, +,C) :- !,
        pown(N, A, [1], R, C).

        pown(0, A, M, R, M) :- !.
        pown(1, A, M, R, P) :- !,
                muln(A, M, R, P).
        pown(N, A, M, R, P) :-
                 N1 is N//2,
                (   N mod 2 =:= 0, M1 = M
                ;   N mod 2 =:= 1, muln(A, M, R, M1)
                ),
                muln(A, A, R, A1), !,
                pown(N1, A1, M1, R, P).


                        % Division of two long integers

divz(S,A, T,B, R, U,Q, S,X) :-
        sign(S, T, U), !,
        divn(A, B, R, Q, X).

        divn(A, [], R, _, _) :- !, fail. % division by 0 is undefined
        divn(A,[1], R, A,[]) :- !.       % a very common special case
        divn(A,[B], R, Q, X) :- !,       % nearly as common a case
                div1(A, B, R, Q, Y),
                conn(Y, [], X).
        divn(A,  B, R, Q, X) :-
                comn(A, B, =, S),
                (   S = '<', Q =  [], X = A
                ;   S = '=', Q = [1], X = []
                ), !.
        divn(A,  B, R, Q, X) :- !,
                divm(A, B, R, Q, X).

                conn(0, [],   []) :- !.
                conn(D, T, [D|T]).

                div1([D1|T1], B1, R, Q1, X1) :- !,
                        div1(T1, B1, R, Q2, X2),
                        D2 is (X2*R+D1)  //  B1,
                        X1 is (X2*R+D1) mod B1,
                        conn(D2, Q2, Q1).
                div1([],      B1, R, [],  0).

% divm(A, B, R, Q, X) is called with A > B > R

                divm([D1|T1], B, R, Q1, X1) :- !,
                        divm(T1, B, R, Q2, X2),
                        conn(D1, X2, T2),
                        div2(T2, B, R, D2, X1),
                        conn(D2, Q2, Q1).
                divm([],      B, R, [], []).

                        div2(A, B, R, Q, X) :-
                                estd(A, B, R, E), !,
                                chkd(A, B, R, E, 0, Q, P), !,
                                subn(A, P, R, S, X).   %  S=+
                        div2(A, B, R, _, _) :-
                                long_error(divq, A/B).

                                estd([A0,A1,A2], [B0,B1], R, E) :-
                                        B1 >= R//2, !,
                                        E is (A2*R+A1)//B1.
                                estd([A0,A1,A2], [B0,B1], R, E) :- !,
                                        L is (A2*R+A1)//(B1+1),
                                        mul1([B0,B1],    L, R, P),
                                        subn([A0,A1,A2], P, R, S, N), !, %S=+
                                        estd(N, [B0,B1], R, M),    !,
                                        E is L+M.
                                estd([A0,A1],    [B0,B1], R, E) :- !,
                                        E is (A1*R+A0+1)//(B1*R+B0).
                                estd([A0],       _,       R, 0) :- !.
                                estd([A0|Ar],    [B0|Br], R, E) :- !,
                                        estd(Ar, Br, R, E).
                                estd([],         _,       R, 0) :- !.
        
                                chkd(A, B, R, E, 3, _, _) :-       !,
                                        long_error(divq, A/B).
                                chkd(A, B, R, E, K, E, P) :-
                                        mul1(B, E, R, P),
                                        comn(P, A, <, <), !.
                                chkd(A, B, R, E, K, Q, P) :-
                                        L is K+1, F is E-1, !,
                                        chkd(A, B, R, F, L, Q, P).



                        % GCD of two long integers
/*
gcdz(S,A, T,B, R, D, S,M, T,N) :- !,
        gcdn(A, B, R, D, M, N).
*/
        gcdn([], [], R, [1],  [],  []) :- !.
        gcdn([],  B, R,   B,  [], [1]) :- !.
        gcdn( A, [], R,   A, [1],  []) :- !.
        gcdn([1], B, R, [1], [1],   B) :- !.    %  common case
        gcdn( A,[1], R, [1],   A, [1]) :- !.    %  common case
        gcdn( A,  B, R,   D,   M,   N) :-       %  A, B > 1
                gcdn(A, B, R, D),
                divn(A, D, R, M, _),
                divn(B, D, R, N, _).

                gcdn(A, B, R, D) :-             %  A, B >= 1  !!
                        comn(A, B, =, S), !,
                        gcdn(S, A, B, R, D).

                        gcdn(<,[], B, R, B) :- !.
                        gcdn(<, A, B, R, D) :-
                                estg(B, A, R, E),
                                muln(E, A, R, P),
                                subn(B, P, R, _, M), !,
                                gcdn(A, M, R, D).
                        gcdn(>, A,[], R, A) :- !.
                        gcdn(>, A, B, R, D) :-
                                estg(A, B, R, E),
                                muln(E, B, R, P),
                                subn(A, P, R, _, M), !,
                                gcdn(M, B, R, D).
                        gcdn(=, A, B, R, A).

                                estg(    A,   [B], R, E) :- !,
                                        div1(A, B, R, Q, X),
                                        (   X*2 =< B, E = Q
                                        ;   add1(Q, R, E)
                                        ),  !.
                                estg([_|A], [_|B], R, E) :- !,
                                        estg(A, B, R, E).

%% TRIGONOMETRIC EVALUATION %%

        % This stuff needs some work done on it, and the mode
        % declarations haven't been written yet.  Taihoa.
        % To do:
        %       Since at this stage all the argumentss are known to be
        %       numbers we shouldn't waste time using the general eval.
        %       Approximations should be used so that the routines work
        %       for ANY argument.  Care is needed, since little is known
        %       about rational approximations, lest the numbers explode.




sineval(X, S) :-
        eval(X < 0),    !,
        eva2(-X, Y),
        sineval(Y, T),
        eva2(-T, S).
sineval(X, S) :-
        eval(X > 90),   !,
        eva2(180-X, Y),
        sineval(Y, S).
sineval(X, S) :-        % 0 <= X <= 90
        sineval1(X, S).


        sineval1(number(+,[],[1]),   number(+,[],[1])).
        sineval1(number(+,[30],[1]), number(+,[1],[2])).
%        sineval1(number(+,[45],[1]), number(+,[99],[140])).    % Use C code now
%        sineval1(number(+,[60],[1]), number(+,[45],[52])).     % Use C code now
        sineval1(number(+,[90],[1]), number(+,[1],[1])).



coseval(X, C) :-
        eva2(90-X, Y), !,
        sineval(Y, C).


taneval(X, T) :-
        sineval(X, S),
        coseval(X, C), !,
        eva2(S/C, T).


arcsineval(S, X) :-
        eval(S >= 0), !,
        sineval1(X, S).
arcsineval(S, X) :-
        eval(S < 0),
        eva2(-S, T), !,
        sineval1(Y, T),
        eva2(-Y, X).


arccoseval(C, X) :-
        arcsineval(C, Y), !,
        eva2(90-Y, X).


arctaneval(number(S,N,D), number(S,M,C)) :-
        arctaneval(N, D, M, C).
        
arctaneval([],X, [], X) :- !.           %  arctan(0) = 0, arctan(undef) = undef
arctaneval(X, X, [45], [1]) :- !.       %  arctan(1) = 45`
arctaneval(X,[], [90], [1]) :- !.       %  arctan(inf) = 90`
arctaneval(N, D, M, C) :-
        R = 100000,                     %  the common radix
        muln(N, N, R, Nsq),
        muln(D, D, R, Dsq),
        addn(Nsq, Dsq, 0, R, Sq), !,
        nthn(2, Sq, R, Den), !,
        arcsineval(number(+,N,Den), S),
        S = number(+,M,C).



%% ERROR HANDLING %%

long_error(Culprit, Expression) :-
        long_error_message(Culprit, Message),
        display('** '), display(Message), display(': '),
        print(Expression), ttynl,
        break, fail.

long_error_message(eval, 'EVAL given a variable').
long_error_message(eva2, 'EVAL given an expression containing a variable').
long_error_message(divq, 'Unexpected rational division problem').

