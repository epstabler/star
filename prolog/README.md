# Rule R and transductions in prolog

After installing swipl and tcl/tk, for r demo example, type:

```
# swipl r.pl
```

If tcl/tk is not installed, comment the Display example at the bottom of r.pl.
Then you can start swipl, and proceed as illustrated in ``r-session.txt``.

Rule R has a remarkably succinct and straightforward implementation in ``r.pl`` -- 11 clauses altogether.

Each of merge ``mrg``, labeling ``l``, mating ``m``, the ``&`` condition, and rule R are defined with a couple of clauses that quite directly translate the English-like presentation in the paper.

The comments in ``r.pl`` should aid understanding. And see ``r-session.txt`` for a first simple use.

``r.pl`` was tested on some other small grammars, provided here.

``rAppB.pl`` extends ``r.pl`` for adjuncts, as described in Appendix B, with the corresponding ``rAppB-session.txt``.

Uncommment ``verbose`` to get step-by-step details printed out.

The files ``genXbarToFig1b.pl`` and ``genXbar.pl`` have a similarly straightforward implementation of the transductions sketched in section 3 of the paper. The file ``genXbarToFig1b.pl`` comments the code in Appendix C. The code in ``genXbar.pl`` generates a syllabified version of Figure 1b, and also the 3 trees in Figure 5.
If you have SWI-Prolog and TCL/TK installed, try:

```
# swipl genXbarToFig1b.pl
# swipl genXbar.pl
```
In those files, it is easy to make modifications to run other examples. The definition of a more general (and possibly much simpler!) gen function that defines a larger space of trees -- based on linguistic investigation of what is actually found in human languages -- is left for future work.

It takes a little work to understand how prolog works, but with practice it becomes exceptionally intuitive. For quick prototyping of simple symbolic computations, it is hard to beat.

