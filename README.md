# star
Kleene-* extensions of MG, OT interface, implementing ideas from
[Stabler & Yu 2023 "Unbounded recursion in two dimensions"](https://blogs.umass.edu/scil/scil-2023/)

NB: This preliminary implementation aims only to provide 
an initial check on how some of the ideas in the paper work together,
and possibly a basis for further experimentation and development.

## prolog/ 

Simple, first SWI-Prolog 9.x implementation of rule R and a simple syntax to prosodic structure transducer.
There is also an extension of the R implementation to handle the example in Appendix B.

Some tiny example grammars are provided,
including a grammar that derives examples of Irish coordination in the paper,
and a grammar that derives the XX copy language.

## python/ 

Python 3.x CKY-like parser for *-MG, implementing rule K (without head movement).
Some tiny example grammars are provided,
including a grammar that derives examples of Irish coordination in the paper,
and a grammar that derives the XX copy language.

An implementation of deterministic unranked extended multi bottom-up tree transducers.

