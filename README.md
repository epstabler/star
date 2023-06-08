# star
Kleene-* extensions of MG, OT interface, implementing ideas from
[Stabler & Yu 2023 "Unbounded recursion in two dimensions"](https://scholarworks.umass.edu/scil/)

NB: This preliminary implementation aims only to provide 
an initial check on how some of the ideas in the paper work together,
and possibly a basis for further experimentation and development.

## prolog/ 

Simple, first SWI-Prolog 9.x implementation of rule R and a simple syntax to prosodic structure transducer that derives the prosodic structures in Figure 1b and Figure 5.

Besides a grammar that derives examples of Irish coordination in the paper, some other toy example grammars are provided, including and a grammar that derives the XX copy language. There is also an extension of the R implementation to handle the example in Appendix B.

## python/ 

Python 3.x CKY-like parser for *-MG, implementing rule K (without head movement). Some tiny example grammars are provided, including a grammar that derives examples of Irish coordination in the paper, and a grammar that derives the XX copy language.

An implementation of deterministic unranked extended multi bottom-up tree transducers.

