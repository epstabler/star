# *-MG parser and tree transducers

The files in this directory have some long examples that test and explain what has been implemented here.
(And more detail about steps of the computations is provided by editing the files to set VERBOSE = True)
Studying the examples, in order, is a good way to become familiar with this code.
The code was tested with python3.7, and will likely run on any python3.x.

## CKY parser for *-CFG

This is interesting to compare *-extension of more familiar context free grammars. For demo example, type:

```
# python cfgstarckyp.py
```
For VERBOSE output, set ``VERBOSE = True`` in that file.

Notice the tedious redundancies in the grammar, and the clunky processing required in the parsing code.

## CKY parser for *-MG: mgstarckyp.py

For demo example, type:

```
# python mgstarckyp.py
```
For VERBOSE output, set ``VERBOSE = True`` in that file. CKY chart-based bottom-up parsers for MGs have been discussed in many places, beginning with [Harkema 2001](https://linguistics.ucla.edu/people/stabler/paris12/Harkema01.pdf). The *-extension is straightforward.

Note that the very common representation of trees as data structures
with a node label and a list of (0 or more) children is already an
unranked tree data structure, where there is no assumption that each
symbol has a unique rank. 
This contrasts with successor-based
representations of trees, as for example in binary trees where each
internal node has a left function and a right function.

## Unranked trees: utree.py

A class U of unranked trees is defined, with functions needed for our purposes.

This is based on the NLTK class Tree, but, for the transductions,
match and instantiate operations are added (among other things -- see comments in file).
Conversion to and from NLTK Trees can be done via their string representations,
so NLTK graphical display is available with this conversion.

## Deterministic, unranked, extended multi bottom-up tree transduction: duxmbutt.py

For demo examples, type:

```
# python duxmbutt.py
```

See comments in that file and the many other examples there.

See see umbutts/ for example grammars, and umbutts/\*Star\* for *- and +- extended examples.
\*-extended U transducer rules can apply to any number of children.
This is in effect a kind of polymorphism: the function defined by these rules can operate on 
nodes labeled X regardless of its arity. This *-extension is implemented by putting subtrees into a variable which is tested in a Boolean "condition" on the rule. The implementation is currently not enforcing restrictions on the definition of those condiitons, but when conditions conform to the standard definition of * or +, the duxmbutt is finite state.

Note how the examples are pretty-printed strings that are parsed into a list of
rules, where each rule is a 4-tuple (treeIn, treeOut, conditions, weight).

The conditions are boolean tests on bound label variables that use only builtin Python operations,
so ``'True'`` is the empty condition. The conditions are
represented as strings to facilitate binding of variables, 
and then they are evaluated with Python's eval(conditions, bindings). For details, see code and comments.

For VERBOSE output, set ``VERBOSE = True`` in ``duxmbutt.py``

None of the basic functions other than graphical display require NLTK. (if needed, ``pip install nltk``).

## OT via composition and pruning: ot.py

Given an umbutt and constraints in order of priority, if umbutt is not deterministic, compose with next constraint and prune non-optimal paths, until either there are no more constraints, or the umbutt is deterministic. See comments in the file.

For demo examples, type:

```
# python ot.py
```

*** in progress ***
