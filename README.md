# star
Kleene-* extensions of MG, OT interface, implementing ideas from
[Stabler & Yu 2023 "Unbounded recursion in two dimensions"](https://scholarworks.umass.edu/scil/vol6/iss1/32/)

** preliminary implementations, in progress **

## prolog/ 

Simple, first SWI-Prolog 9.x implementation of rule R and a simple syntax to prosodic structure transducer that derives the prosodic structures in Figure 1b and Figure 5.

Besides a grammar that derives examples of Irish coordination in the Stabler & Yu paper, some other toy example grammars are provided, including and a grammar that derives the XX copy language. There is also an extension of the R implementation to handle the example in Appendix B.

## python/ 

Python 3.x CKY-like parser for *-MG, implementing rule K (without head movement). Some tiny example grammars are provided, including a grammar that derives examples of Irish coordination in the paper, and a grammar that derives the XX copy language. An implementation of unranked extended multi bottom-up tree transducers.

## haskell/

An implementation of *-merge and labeling.
This typed, higher order language is good for carefully assessing the anatomy of these processes.
This code provides the clearest formulation of what the *-extension really amounts to.

