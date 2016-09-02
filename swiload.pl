ttynl :- nl.

% Evaluates the exponential function exp(Number) ("e to the power of Number") and 
% unifies the resulting value with Result. See the following URL for more information.
% http://www.cs.uni-potsdam.de/wv/lehre/Material/Prolog/Eclipse-Doc/bips/kernel/arithmetic/exp-2.html
exp(A,B) :- B is exp(A).

%fwritef(File, Format) :-
%        fwritef(File, Format, []).
%
%fwritef(File, Format, List) :-
%        telling(Old),
%        tell(File),
%        writef(Format, List),
%        tell(Old).

:-
[
% UTILITIES',
'./util/util_ops.pl',
'./util/arith_ops.pl',
'./util/invoca.pl',
'./util/imisce.pl',
'./util/applic.pl',
'./util/arith.pl',
'./util/bagutl.pl',
'./util/betwee.pl',
% ./util/files.pl',
'./util/flagro.pl',
'./util/gensym.pl',
%./util/getfil.pl',
%./util/getfil.pl',
'./util/listut.pl',
'./util/long.pl',
'./util/metutl.pl',
'./util/occur.pl',
'./util/projec.pl',
'./util/readin.pl',
'./util/setutl.pl',
'./pressdir/toplevel/ident.pl',
'./util/struct.pl',
'./util/tidy.pl',
'./util/trace.pl',
'./util/writef.pl',

% METHODS',
'./pressdir/methods/chunk.pl',
'./pressdir/methods/collec.pl',
'./pressdir/methods/attrac.pl',

% AXIOMS',
'./pressdir/axioms/simp_ax.pl',

% PACKAGES',
'./pressdir/package/match.pl',
%./pressdir/pressjunk/init',
'./pressdir/package/diff.pl',
'./pressdir/package/polpak.pl',
'./pressdir/package/poltid.pl',
'./pressdir/package/odds.pl',
'./pressdir/package/weaknf.pl',
'./pressdir/package/real.pl',
'./pressdir/package/int.pl',

% MISCELLANEOUS',
'./pressdir/misc/words.pl',
'./pressdir/misc/gportr.pl',
'./pressdir/misc/misc.pl',
'./pressdir/misc/fld.pl',

% TOP LEVEL',
'./pressdir/toplevel/solve.pl',
'./pressdir/toplevel/simeq.pl',
'./pressdir/toplevel/sim.pl',
'./pressdir/toplevel/ineq.pl',

% METHODS',
'./pressdir/methods/isolat.pl',
'./pressdir/methods/factor.pl',
'./pressdir/methods/poly.pl',
'./pressdir/methods/trig_fac.pl',
'./pressdir/methods/nas1.pl',
'./pressdir/methods/homog_top.pl',
'./pressdir/methods/homog_trg.pl',
'./pressdir/methods/log.pl',
'./pressdir/methods/nasty.pl',

% AXIOMS',
'./pressdir/axioms/isolat_ax.pl',
%'./pressdir/axioms/ineqis.ax', todo:tk:commenting out because it redefines isolax/4 in isolat.ax.
'./pressdir/axioms/collec_ax.pl',
'./pressdir/axioms/attrac_ax.pl',
'./pressdir/axioms/homog_rew.pl',
'./pressdir/axioms/facts.pl',
'./pressdir/axioms/init.pl',

% PACKAGES',
'./pressdir/package/prover.pl',
'./pressdir/package/manip.pl',

% MISCELLANEOUS',
'./pressdir/misc/homog_msc.pl',

% PROBLEMS',

%./pressdir/probs/runex ',
'./pressdir/probs/demo.pl'

% JUNK',
%./pressdir/pressjunk/facile',

].


:- tlim(5).
