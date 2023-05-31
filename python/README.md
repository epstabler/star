# *-MG parser and tree transducers

## Unranked trees: utree.py

The class U of unranked trees is defined. 

This is based on the NLTK class Tree, but, for the transductions,
match and instantiate operations are added.
Conversion to and from NLTK Trees can be done via their string representations,
so NLTK graphical display is available with this conversion.

Making the trees unranked just means that a particular category can have different numbers of children.
In effect, instead of building trees from the "parent" (aka "successor") relation between every node
and each of its children,
the trees are built with the "has children" relation between every node and the (possibly empty)
list (aka "sequence") of its children. Those lists can have any finite length
(setting aside limits on your python implementation).

## CKY parser for *-MG: mgstarckyp.py

For demo example, type:

```
# python mgstarckyp.py
```
For VERBOSE output, set ``VERBOSE = True`` in that file. CKY chart-based bottom-up parsers for MGs have been discussed in many places, beginning with [Harkema 2001](https://linguistics.ucla.edu/people/stabler/paris12/Harkema01.pdf). The *-extension is straightforward.

## Deterministic, unranked, extended multi bottom-up tree transduction: duxmbutt.py

Transitions have the form (treeIn, treeOut, conditions, weight),
where conditions are either ``'True'`` or
boolean tests on bound label variables that use only builtin
Python operations. For details about variables and binding
see code and comments. See umbutts/\*Star\* for *- and +- extended examples.

For VERBOSE output, set ``VERBOSE = True`` in ``duxmbutt.py``

None of the basic functions other than graphical display require NLTK. (if needed, ``pip install nltk``).

For demo examples, type:

```
# python duxmbutt.py
```

The U transducer rules can define transitions that apply to any number of children -- 
so this is in effect a kind of polymorphism: the function defined by a rule can operate on 
a nodes labeled X with various arities. The *-extension is implemented by putting subtrees into a variable which is tested in a Boolean "condition" on the rule. The implementation is currently not enforcing restrictions on the definition of those condiitons, but when conditions conform to the standard definition of * or +, the duxmbutt is finite state.
See comments in that file and the many other examples there.

## OT via composition and pruning: ot.py

Given an umbutt and constraints in order of priority, if umbutt is not deterministic, compose with next constraint and prune non-optimal paths, until either there are no more constraints, or the umbutt is deterministic. See comments in the file.

*** in progress ***
