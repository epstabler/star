# Tree transducers and *-MG parser

The *test* files in this directory have some long examples that
explain what has been implemented here.  (And runtime details about
steps of the computations is provided by editing the files to set
VERBOSE = True.)  Studying these examples, in order, is a good way to
become familiar with this code.  The code was tested with python3.7,
and will likely run on any python3.x.

## Unranked trees: utree.py

A class U of unranked trees is defined, with functions needed for our purposes.

For demo examples, type:

```
# python utree.py
```

See comments in that file and the many other examples there at the bottom of the file.

This module is based on the NLTK class Tree, but
match and instantiate operations are added, among other things.

NLTK is not needed to use this code, unless you want graphical tree
display. Conversion to and from NLTK Trees can be done via the
string representations, so NLTK graphical display is available with
this conversion.

Note that the very common representation of trees as data struactures
with a node label and a list of (0 or more) children is already an
unranked tree data structure. That is, there is no assumption that
each symbol has a unique rank.  This contrasts with successor-based
representations of trees, as for example in binary trees where every
internal node has rank 2, usually with a left constructor/function and
a right constructor/function.

## Unranked bottom-up transducers: ubutt.py

For demo examples, type:

```
# python ubutt.py
```

See comments in that file and the many other examples there at the bottom of the file.

See umbutts/ for example grammars, and umbutts/\*Star\* for *- and +- extended examples.
\*-extended U transducer rules can apply to any number of children.
This is in effect a kind of polymorphism: the function defined by
these rules can operate on nodes labeled X regardless of
arity. This *-extension is implemented by putting subtrees into a
variable which is tested in a Boolean "condition" on the rule. The
implementation is currently not enforcing restrictions on the
definition of those condiitons, but when conditions conform to the
standard definition of * or +, the ubutt is finite state.

The conditions are (string representations of)
boolean tests on bound variables in the rules,
conditions that use only builtin Python operations, so ``'True'`` is
the empty condition. With conditions as strings, we can
bind the variables, and then evaluate with
Python's `eval(conditions, bindings)`.

## OT via composition and pruning: ot.py

Given an ubutt and constraints in order of priority, if ubutt is not
deterministic, compose with next constraint and prune non-optimal
paths, until either there are no more constraints, or the ubutt is
deterministic. See comments in the file.

For demo examples, type:

```
# python ot.py
```

Edit the bottom of the file to run different examples.

*** in progress ***

## CKY parser for *-CFG

It is interesting to compare *-extension of more familiar context free
grammars. For demo example, type:

```
# python cfgstarckyp.py
```
For VERBOSE output, set ``VERBOSE = True`` in that file.

Notice the tedious redundancies in the context free grammar, and the
clunky processing required in the parser code. The analogous MGs are
(slightly) better.

## CKY parser for *-MG: mgstarckyp.py

For demo example, type:

```
# python mgstarckyp.py
```

For VERBOSE output, set ``VERBOSE = True`` in that file. CKY
chart-based bottom-up parsers for MGs have been discussed in many
places, beginning with
[Harkema 2001](https://linguistics.ucla.edu/people/stabler/paris12/Harkema01.pdf).
The *-extension is straightforward.
