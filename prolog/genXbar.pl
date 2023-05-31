/* To facilitate comparison with the first example of the paper, here
   a simple implementation that derives **a syllabified version of** Fig 1b
   from xbar, by slightly modifying the rules listed in the paper

  For tcl/tk tree display, uncomment the lines that say
    "uncomment for tree display"

  The definition of a more general (and possibly much simpler!) gen
  function that defines a larger space of trees -- based on linguistic
  investigation of what is actually found in human languages -- is
  left for future work.

  See warning below about the very simple method used here to explore
  derivations. Search for "WARNING"

  (See genXbarToFig1b.pl for the unsyllabified Figure 1,
  which is also discussed in the paper)
*/
:- use_module(pptree,[pptree/1]).
:- use_module(library(apply),[maplist/3]).
% optional, for tree display:
:- use_module('tree_display/wish_tree'). % uncomment for tree display
:- use_module('tree_display/latex_tree'). % uncomment for latex_tree

% X-bar tree with syllables
xbarSyll( cp/[
	      c/[],
	      tmp/[
		  tm/[
		      v/[ is/[] ],
		      tm/[] ],
		  vp/[
		      v/[],
		      ap/[
			  a/[ cu/[], ma/[] ],
			  lpredp/[
			      dp/[ e/[] ],
			      lpred1/[
				  lpred/[],
				  bp/[
				      predp/[
					  pred/[ na/[] ],
					  dp/[ shamh/[], radh/[] ] ],
				      b1/[
					  b/[ no/[] ],
					  predp/[
					      pred/[ na/[] ],
					      dp/[ gheimh/[], readh/[] ] ] ] ] ] ] ] ] ] ] ).

fig1b(qi /[
	  i /[
              w /[
		  sigma /[
                      is /[]]],
              phi /[
		  w /[
                      sigma /[
			  cu /[]],
                      sigma /[
			  ma /[]]],
		  phi /[
                      phi /[
			  w /[
                              sigma /[
				  e /[]]]],
                      phi /[
			  phi /[
                              w /[
				  sigma /[
                                      na /[]]],
                              phi /[
				  w/[
				      sigma /[
					  shamh /[]],
				      sigma /[
					  radh /[]]]]],
			  phi /[
                              w /[
				  sigma /[
                                      no /[]]],
                              phi /[
				  w /[
                                      sigma /[
					  na /[]]],
				  phi /[
				      w/[
					  sigma /[
					      gheimh /[]],
					  sigma /[
					      readh /[]]]]]]]]]]] ).

fig5a(qi /[
	  i /[
              phi /[
		  sigma /[
                      is /[]],
		  w /[
                      sigma /[
			  cu /[]],
                      sigma /[
			  ma /[]]]],
              phi /[
		  phi /[
                      w /[
			  sigma /[
                              e /[]]],
                      w /[
			  sigma /[
                              na /[]],
			  w/[
			      sigma /[
				  shamh /[]],
			      sigma /[
				  radh /[]]]]],
		  phi /[
                      w /[
			  sigma /[
                              no /[]]],
                      w /[
			  sigma /[
                              na /[]],
			  w/[
                              sigma /[
				  gheimh /[]],
                              sigma /[
				  readh /[]]]]]]]]).

fig5b(qi /[
	  i /[
              phi /[
		  sigma /[
                      is /[]],
		  w /[
                      sigma /[
			  cu /[]],
                      sigma /[
			  ma /[]]]],
              phi /[
		  phi /[
                      w /[
			  sigma /[
                              na /[]],
			  w/[
			      sigma /[
				  shamh /[]],
			      sigma /[
				  radh /[]]]],
		      w /[
			  sigma /[
                              e /[]]]],
		  phi /[
                      w /[
			  sigma /[
                              no /[]]],
                      w /[
			  sigma /[
                              na /[]],
			  w/[
                              sigma /[
				  gheimh /[]],
                              sigma /[
				  readh /[]]]]]]]]).


fig5c(qi /[
	  i /[
              phi /[
		  sigma /[
                      is /[]],
		  w /[
                      sigma /[
			  cu /[]],
                      sigma /[
			  ma /[]]]],
              phi /[
		  phi /[
                      w /[
			  sigma /[
                              na /[]],
			  w/[
			      sigma /[
				  shamh /[]],
			      sigma /[
				  radh /[]]]],
		      phi /[
			  w /[
			      sigma /[
				  no /[]]],
			  w /[
			      sigma /[
				  na /[]],
			      w/[
				  sigma /[
				      gheimh /[]],
				  sigma /[
				      readh /[]]]]]],
		  w /[
		      sigma /[
                          e /[]]]]]]).


head(X) :- member(X, [c,tm,v,a,lpred,pred,b]). 
phrase(XP) :- atom_chars(XP, L), last(L, p).
clause(tmp).

% WARNING, this def of gen is very simple, but may not find all derivations?
%  The python implementation stays very close to Engelfriet, Lilin, and Maletti 2009,
%  aiming for code with the completeness guarantees they establish
gen(T, Out) :- rule(T, Out).
gen(X/L, Out) :- maplist(gen,L,Subtrees), rule(X/Subtrees, Out).
gen(T,T).

rule(XP/[qw/[X0], qphi/[X1,X2]], qi/[ i/[phi/[X0, X1], X2]]) :-  clause(XP). % 9th rule in the paper MOVE1(is) landing
rule(XP/[qw/[X0], qphi/[X1]], qi/[ i/[X0, X1] ]) :- clause(XP).   % 9th rule in the paper

rule(X/[ Syll/[] ], qw/[ sigma/[Syll/[]] ]) :- head(X), Syll \= qe. % ~ 1st rule, violates match
rule(X/[ Syll/[] ], qw/[ w/[ sigma/[Syll/[]] ] ]) :- head(X), Syll \= qe. % ~ 1st rule from paper
rule(X/[ Syll1/[], Syll2/[] ], qw/[ w/[ sigma/[Syll1/[]], sigma/[Syll2/[]]]]) :- head(X). % ~ 1st rule

rule(_/[ Syll/[] ], qphi/[ w/[ sigma/[Syll/[]] ] ] ) :- Syll \= qe. % ~ 2nd rule, violates match, FOR e
rule(XP/[ Syll/[] ], qphi/[ phi/[ w/[ sigma/[Syll/[]] ] ] ] ) :- phrase(XP), Syll \= qe. % 2nd rule from paper
rule(XP/[ Syll1/[], Syll2/[] ], qphi/[ phi/[ w/[sigma/[Syll1/[]], sigma/[Syll2/[]]]]] ) :- phrase(XP). % ~ 2nd rule
rule(XP/[ Syll1/[], Syll2/[] ], qphi/[ w/[sigma/[Syll1/[]], sigma/[Syll2/[]]]] ) :- phrase(XP). % ~ 2nd rule, violates match

rule(X/[], qe/[]) :- head(X). % 3rd rule from paper

rule(_/[qw/[X0], qphi/[X1]], qphi/[ w/[X0, X1] ]).   % ~ 4th rule for each coordinate, violates match
rule(_/[qw/[X0], qphi/[X1]], qphi/[ phi/[X0, X1] ]).   % 4th rule from paper
rule(_/[qw/[X0], qphi/[X1]], qphi/[ X0, X1 ]).   % ~ 4th rule MOVE1 launch

rule(_/[qphi/[X0], qphi/[X1]], qphi/[ phi/[X0, X1] ]). % 5th rule from paper
rule(_/[qphi/[X0], qphi/[X1]], qphi/[ X0, X1 ]). % ~ 5th rule, MOVE launch pronoun for Fig5a,b
rule(_/[qphi/[X0], qphi/[X1, X2]], qphi/[ phi/[phi/[X0,X1], X2] ]). % ~ 5th rule, MOVE Fig5a landing pronoun
rule(_/[qphi/[X0], qphi/[X1, X2]], qphi/[ phi/[phi/[X1,X0], X2] ]). % ~ 5th rule, MOVE Fig5b landing pronoun
rule(_/[qphi/[X0], qphi/[X1]], qphi/[ phi/[X1, X0] ]). % ~ 5th rule for Fig5c, just reverses order

rule(_/[qe/[], T], T). % 6th rule from paper
rule(_/[T, qe/[]], T). % 7th rule
rule(_/[X], X).     % 8th rule

test(InPred,OutPred) :-

    InTerm =.. [InPred,Syntax],
    call(InTerm),
    %xbarSyll(Syntax),
    nl,write('Attempting to transduce this syntactic structure:'),
    nl, pptree(Syntax), nl,
    wish_tree(input/[Syntax]), sleep(1), % sleep is usually needed, wish is reading from file
    
    OutTerm =.. [OutPred,Target],
    call(OutTerm),

    Time1 is cputime,
    gen(Syntax, Root/Subtrees),

    atom_chars(Root, [q|_]), % require a state, but could check finality too

    %nl,write('Output prosodic structure:'), nl, pptree(Root/Subtrees), nl,

    (Target = Root/Subtrees ->
	 Time2 is cputime,
	 pptree(success/[Root/Subtrees]),
	 write('success!'),nl, % search for this structure
	 wish_tree(success/[Root/Subtrees])  % uncomment for tree display of candidates
    ;    fail, wish_tree(bad/[Root/Subtrees])  % uncomment for tree display of candidates
    ), 

    Time is Time2-Time1,
    nl, write(Time), write(' seconds'), nl.

%:- test(xbarSyll,fig1b).
:- test(xbarSyll,fig5a).
%:- test(xbarSyll,fig5b).
%:- test(xbarSyll,fig5c).
