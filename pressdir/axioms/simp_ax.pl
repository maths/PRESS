
/* SIMP.AX : Simplification axioms for TIDY

						Bernard Silver
						Updated: 13 May 82
*/

% % PUBLIC 
% :- public simplify_axiom/2.
% 
% % MODES
% 
% :- mode simplify_axiom(+,-).

% Logs
simplify_axiom(log(U,U^V),V) :- !.
simplify_axiom(log(_A,1),0) :- !.
simplify_axiom(U^log(U,V),V) :- !.
simplify_axiom(U ^( N*log(U,V)),Ans) :- ok_number(N),tidy(V^N,Ans),!.
% Normalize square roots
simplify_axiom(sqrt(U),U^number((+),[1],[2])) :- !.

% Trig cancelling pairs
simplify_axiom(cos(arccos(X)),X) :- !.
simplify_axiom(arccos(cos(X)),X) :- !.

simplify_axiom(arcsin(sin(X)),X) :- !.
simplify_axiom(sin(arcsin(X)),X) :- !.

simplify_axiom(tan(arctan(X)),X) :- !.
simplify_axiom(arctan(tan(X)),X) :- !.

simplify_axiom(sec(arcsec(X)),X) :- !.
simplify_axiom(arcsec(sec(X)),X) :- !.

simplify_axiom(cosec(arccosec(X)),X) :- !.
simplify_axiom(arccosec(cosec(X)),X) :- !.

simplify_axiom(cot(arccot(X)),X) :- !.
simplify_axiom(arccot(cot(X)),X) :- !.

% Hyperbolic cancelling pairs
simplify_axiom(sinh(arcsinh(X)),X) :- !.
simplify_axiom(arcsinh(sinh(X)),X) :- !.

simplify_axiom(cosh(arccosh(X)),X) :- !.
simplify_axiom(arccosh(cosh(X)),X) :- !.

simplify_axiom(tanh(arctanh(X)),X) :- !.
simplify_axiom(arctanh(tanh(X)),X) :- !.

simplify_axiom(sech(arcsech(X)),X) :- !.
simplify_axiom(arcsech(sech(X)),X) :- !.

simplify_axiom(cosech(arccosech(X)),X) :- !.
simplify_axiom(arccosech(cosech(X)),X) :- !.

simplify_axiom(coth(arccoth(X)),X) :- !.
simplify_axiom(arccoth(coth(X)),X) :- !.

% Common trig cases
simplify_axiom(sin(arccos(X)),(1-X^2)^(1/2)#(1-X^2)^(1/2)*(-1)) :- !.

simplify_axiom(cos(arcsin(X)),(1-X^2)^(1/2)#(1-X^2)^(1/2)*(-1)) :- !.

simplify_axiom(arcsin(cos(X)),90-X) :- !.

simplify_axiom(arccos(sin(X)),90-X) :- !.

