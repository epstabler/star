/* A simple implementation that derives Fig 1b from xbar example
   using transparent representation of the 9 rules listed in the paper

  For tcl/tk tree display, uncomment the 3 lines that say
    "uncomment for tree display"
*/
:- use_module(pptree,[pptree/1]).
:- use_module(library(apply),[maplist/3]).
% optional, for tree display:
:- use_module('tree_display/wish_tree'). % uncomment for tree display
:- use_module('tree_display/latex_tree'). % uncomment for tree display

xbar( cp/[
	  c/[],
	  tmp/[
	      tm/[
		  v/[ is/[] ],
		  tm/[] ],
	      vp/[
		  v/[],
		  ap/[
		      a/[ cuma/[] ],
		      lpredp/[
			  dp/[ e/[] ],
			  lpred1/[
			      lpred/[],
			      bp/[
				  predp/[
				      pred/[ na/[] ],
				      dp/[ shamhradh/[] ] ],
				  b1/[
				      b/[ no/[] ],
				      predp/[
					  pred/[ na/[] ],
					  dp/[ gheimhreadh/[] ] ] ] ] ] ] ] ] ] ] ).

fig1b( qi /[
    i /[
        w /[
            is /[]],
        phi /[
            w /[
                cuma /[]],
            phi /[
                phi /[
                    w /[
                        e /[]]],
                phi /[
                    phi /[
                        w /[
                            na /[]],
                        phi /[
                            w /[
                                shamhradh /[]]]],
                    phi /[
                        w /[
                            no /[]],
                        phi /[
                            w /[
                                na /[]],
                            phi /[
                                w /[
                                    gheimhreadh /[]]]]]]]]]] ).

head(X) :- member(X, [c,tm,v,a,lpred,pred,b]). 
phrase(XP) :- atom_chars(XP, L), last(L, p).

gen(T, Out) :- rule(T, Out).
gen(X/L, Out) :- maplist(gen,L,Subtrees), rule(X/Subtrees, Out).
gen(T,T).

% Putting the 9th rule first, get derives Fig 1a from xbar example above as first result
rule(_/[qw/[X0], qphi/[X1]], qi/[ i/[X0, X1] ]).      % 9th rule in the paper
rule(X/[ Phon/[] ], qw/[ w/[ Phon/[] ] ]) :- head(X). % 1st rule in the paper
rule(XP/[ Phon/[] ], qphi/[ phi/[ w/[ Phon/[] ] ] ] ) :- phrase(XP). % 2nd rule in the paper
rule(X/[], qe) :- head(X). % 3rd rule in the paper
rule(_/[qw/[X0], qphi/[X1]], qphi/[ phi/[X0, X1] ]).   % 4th rule in the paper
rule(_/[qphi/[X0], qphi/[X1]], qphi/[ phi/[X0, X1] ]). % 5th rule in the paper
rule(_/[qe, T], T). % 6th rule in the paper
rule(_/[T, qe], T). % 7th rule in the paper
rule(_/[X], X).     % 8th rule in the paper

example1b :-
    xbar(Syntax),
    nl,write('Attempting to transduce this syntactic structure:'),
    nl, pptree(Syntax), nl,
    Time1 is cputime,
    gen(Syntax, Root/Prosody),
    atom_chars(Root, [q|_]), % require a state, but could check finality too
    Time2 is cputime,
    Time is Time2-Time1,
    write('success!'),
    nl,write('Output prosodic structure:'),
    nl, pptree(Root/Prosody), nl,
    nl, write(Time), write(' seconds'), nl,
    wish_tree(Root/Prosody).  % uncomment for tree display

example1bDisplay :-
    xbar(Syntax),
    nl,write('Attempting to transduce this syntactic structure:'),
    nl, pptree(Syntax), nl,
    Time1 is cputime,
    gen(Syntax, Root/Prosody),
    atom_chars(Root, [q|_]), % require a state, but could check finality too
    Time2 is cputime,
    Time is Time2-Time1,
    write('success!'),
    nl,write('Output prosodic structure:'),
    nl, pptree(Root/Prosody), nl,
    nl, write(Time), write(' seconds'), nl,
    wish_tree(Root/Prosody).

%:- example1b.
:- example1bDisplay.  % uncomment for tree display
