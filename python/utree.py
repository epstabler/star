# utree.py -- unranked tree
# This data structure is used by unranked tree transducers
#   in duxmbutt.py and ot.py

import sys, re

class U(list):
    r"""
    This U tree structure is a stripped down version of NLTK Tree,
    but with root(self) instead of label(self), and we add:

          children(self): returns children of self

          subtrees(self): returns all subtrees of self

          symbolSubtrees(self): returns all subtrees of self
             with input/output symbol at root

          labels(self): returns labels of every node in self

          match(self,tree,bindings): returns (augmented) bindings,

          instantiate(self,bindings): returns instantiated U

    U(root, children) constructs a new tree with the
    specified root label and list of children.

    As in NLTK Trees, in U trees the root labels are unranked.
    That is, there is no assumption that each symbol is uniquely
    ranked.

    NLTK need not be imported, but in case it is,
    you can draw a U t with Tree.fromstring(str(t)).draw()
    """
    def __init__(self, node, children=None):
        if children is None:
            raise TypeError(
                "%s: Expected a node value and child list " % type(self).__name__
            )
        elif isinstance(children, str):
            raise TypeError(
                "%s() argument 2 should be a list, not a "
                "string" % type(self).__name__
            )
        else:
            list.__init__(self, children)
            self._root = node
            self._children = children

    def __eq__(self, other):
        return self.__class__ is other.__class__ and (self._root, list(self)) == (
            other._root,
            list(other),
            )
    
    def root(self):
        return self._root

    def children(self):
        return self._children

    def subtrees(self):
        result = [self]
        for c in self._children: result.extend(c.subtrees())
        return result

    def symbolSubtrees(self):
        """ distinguishing states from symbols by requirement that state._root begins with 'q' """
        result = [self]
        if self._root and self._root[0] != 'q':
            result = [self]
            for c in self._children: result.extend(c.symbolSubtrees())
        return result

    def labels(self):
        labels = [self._root]
        for t in self._children:
            labels.extend(t.labels())
        return labels

    def leaves(self):
        leaves = []
        for child in self:
            if isinstance(child, U):
                leaves.extend(child.leaves())
            else:
                leaves.append(child)
        return leaves

    def __copy__(self):
        return self.copy()

    def __deepcopy__(self, memo):
        return self.copy(deep=True)

    def copy(self, deep=False):
        if not deep:
            return type(self)(self._root, self)
        else:
            return type(self).convert(self)

    # ////////////////////////////////////////////////////////////
    # Parsing from string
    # ////////////////////////////////////////////////////////////

    @classmethod
    def fromstring(
        cls,
        s,
        brackets="()",
        read_node=None,
        read_leaf=None,
        node_pattern=None,
        leaf_pattern=None,
        remove_empty_top_bracketing=False,
    ):
        if not isinstance(brackets, str) or len(brackets) != 2:
            raise TypeError("brackets must be a length-2 string")
        if re.search(r"\s", brackets):
            raise TypeError("whitespace brackets not allowed")
        # Construct a regexp that will tokenize the string.
        open_b, close_b = brackets
        open_pattern, close_pattern = (re.escape(open_b), re.escape(close_b))
        if node_pattern is None:
            node_pattern = rf"[^\s{open_pattern}{close_pattern}]+"
        if leaf_pattern is None:
            leaf_pattern = rf"[^\s{open_pattern}{close_pattern}]+"
        token_re = re.compile(
            r"%s\s*(%s)?|%s|(%s)"
            % (open_pattern, node_pattern, close_pattern, leaf_pattern)
        )
        # Walk through each token, updating a stack of trees.
        stack = [(None, [])]  # list of (node, children) tuples
        for match in token_re.finditer(s):
            token = match.group()
            # Beginning of a tree/subtree
            if token[0] == open_b:
                if len(stack) == 1 and len(stack[0][1]) > 0:
                    cls._parse_error(s, match, "end-of-string")
                root = token[1:].lstrip()
                if read_node is not None:
                    root = read_node(root)
                stack.append((root, []))
            # End of a tree/subtree
            elif token == close_b:
                if len(stack) == 1:
                    if len(stack[0][1]) == 0:
                        cls._parse_error(s, match, open_b)
                    else:
                        cls._parse_error(s, match, "end-of-string")
                root, children = stack.pop()
                stack[-1][1].append(cls(root, children))
            # Leaf node
            else:
                if len(stack) == 1:
                    cls._parse_error(s, match, open_b)
                if read_leaf is not None:
                    token = read_leaf(token)
                stack[-1][1].append(token)

        # check that we got exactly one complete tree.
        if len(stack) > 1:
            cls._parse_error(s, "end-of-string", close_b)
        elif len(stack[0][1]) == 0:
            cls._parse_error(s, "end-of-string", open_b)
        else:
            assert stack[0][0] is None
            assert len(stack[0][1]) == 1
        tree = stack[0][1][0]

        return tree

    @classmethod
    def _parse_error(cls, s, match, expecting):
        # Construct a basic error message
        if match == "end-of-string":
            pos, token = len(s), "end-of-string"
        else:
            pos, token = match.start(), match.group()
        msg = "%s.read(): expected %r but got %r\n%sat index %d." % (
            cls.__name__,
            expecting,
            token,
            " " * 12,
            pos,
        )
        # Add a display showing the error token itsels:
        s = s.replace("\n", " ").replace("\t", " ")
        offset = pos
        # ES
        #if len(s) > pos + 10:
        #    s = s[: pos + 10] + "..."
        #if pos > 10:
        #    s = "..." + s[pos - 10 :]
        #    offset = 13
        msg += '\n{}"{}"\n{}^'.format(" " * 16, s, " " * (17 + offset))
        raise ValueError(msg)

    # ////////////////////////////////////////////////////////////
    # Display -- tty or graphical tk
    # ////////////////////////////////////////////////////////////

    def __repr__(self):
        childstr = ", ".join(repr(c) for c in self)
        return "{}({}, [{}])".format(
            type(self).__name__,
            repr(self._root),
            childstr,
        )

    def __str__(self):
        return self.pformat()

    def pprint(self, **kwargs):
        """
        Print a string representation of U to 'stream'
        """
        if "stream" in kwargs:
            stream = kwargs["stream"]
            del kwargs["stream"]
        else:
            stream = None
        print(self.pformat(**kwargs), file=stream)

    def pformat(self, margin=70, indent=0, nodesep="", parens="()", quotes=False):
        # Try writing it on one line.
        s = self._pformat_flat(nodesep, parens, quotes)
        if len(s) + indent < margin:
            return s

        # If it doesn't fit on one line, then write it on multi-lines.
        if isinstance(self._root, str):
            s = f"{parens[0]}{self._root}{nodesep}"
        else:
            s = f"{parens[0]}{repr(self._root)}{nodesep}"
        for child in self:
            if isinstance(child, U):
                s += (
                    "\n"
                    + " " * (indent + 2)
                    + child.pformat(margin, indent + 2, nodesep, parens, quotes)
                )
            elif isinstance(child, tuple):
                s += "\n" + " " * (indent + 2) + "/".join(child)
            elif isinstance(child, str) and not quotes:
                s += "\n" + " " * (indent + 2) + "%s" % child
            else:
                s += "\n" + " " * (indent + 2) + repr(child)
        return s + parens[1]

    def _pformat_flat(self, nodesep, parens, quotes):
        childstrs = []
        for child in self:
            if isinstance(child, U):
                childstrs.append(child._pformat_flat(nodesep, parens, quotes))
            elif isinstance(child, tuple):
                childstrs.append("/".join(child))
            elif isinstance(child, str) and not quotes:
                childstrs.append("%s" % child)
            else:
                childstrs.append(repr(child))
        if isinstance(self._root, str):
            return "{}{}{} {}{}".format(
                parens[0],
                self._root,
                nodesep,
                " ".join(childstrs),
                parens[1],
            )
        else:
            return "{}{}{} {}{}".format(
                parens[0],
                repr(self._root),
                nodesep,
                " ".join(childstrs),
                parens[1],
            )

    # ////////////////////////////////////////////////////////////
    # Match
    # ////////////////////////////////////////////////////////////

    def match(self, t, bindings):
        """
          where self can contain label variables (LV0, LV1,.., SV0, SV1,...)
                                 and tree variables (TV0, TV1,...),
          appending any new bindings to bindingsIn,
          recursing through subtress and returning the result,
          if there is a match, else None

        This is a python-style fold through U.
        """
        # LV match with star
        if len(self._root[0:2]) > 1 and \
           self._root[0:2] == 'LV' and \
           self._root[2:].isdigit() and \
           len(self._children) == 1 and \
           self._children[0]._root == '*':  # label variable with star
            bindings.append((self._root, t._root))
            bindings.append(('SV0', [child._root for child in t._children]))
            if all([len(child._children)>0 for child in t._children]):
                bindings.append(('ST0', [child._children[0] for child in t._children]))
            return(bindings)

        # SV match with star
        elif self._root == t._root and \
           len(self._children) == 1 and \
           self._children[0]._root == '*': # star
            bindings.append(('SV0', [child._root for child in t._children]))
            if all([len(child._children)>0 for child in t._children]):
                bindings.append(('ST0', [child._children[0] for child in t._children]))
            return(bindings)

        # LV match
        elif len(self._root[0:2]) > 1 and \
           self._root[0:2] == 'LV' and \
           self._root[2:].isdigit() and \
           len(self._children) == len(t._children): # label variable
            bindings.append((self._root, t._root))
            try:
                for p in zip(self._children,t._children):
                    p[0].match(p[1], bindings)
                return(bindings)
            except:
                return(None)

        # TV match
        elif len(self._root[0:2]) > 1 and \
             self._root[0:2] == 'TV' and \
             self._root[2:].isdigit(): # tree variable
            bindings.append((self, t))
            return(bindings)

        # collect embedded matches
        else:
            if self._root == t._root and \
               len(self._children) == len(t._children):
                try:
                    for p in zip(self._children,t._children):
                        b = p[0].match(p[1], bindings)
                        if b == None:
                            return(None)
                    return(bindings)
                except:
                    return(None)
            else:
                return(None)

    # ////////////////////////////////////////////////////////////
    # Instantiate
    # ////////////////////////////////////////////////////////////

    def instantiate(self, bindings):
        """
          where self can contain variable roots (LV0, LV1,..)
                                   and subtrees (TV0, TV1,...),
          those variables are replaced by their bindings,
          recursing through subtress and returning the result.

        This is a python-style map through U.
        """
        if self._root[0:2] == 'LV' and self._root[2:].isdigit(): # label variable
            newRoot = val(self._root, bindings)
            return(U(newRoot, [t.instantiate(sbindingsDict) for t in self._children]))

        elif self._root[0:2] == 'TV' and self._root[2:].isdigit(): # tree variable
            newTree = val(self, bindings)
            return(newTree)

        elif len(self._children) == 1 and \
             self._children[0]._root[0:2] == 'ST' and \
             self._children[0]._root[2:].isdigit():  # star trees variable
            subTrees = val(self._children[0]._root, bindings)
            newTree = U(self._root, subTrees)
            return(newTree)

        else:
            return(U(self._root,
                      [t.instantiate(bindings) for t in self._children]))

def val(key,bindings):
    """ return binding for key

        This would be more efficient if bindings were a dict,
        but our keys and values are not hashable, and
        in most anticipated applications, the list of bindings is very short!
    """
    for b in bindings:
        if b[0] == key:
            return b[1]
    # else
    print('val ERROR -- key = %r \nbindings = %r' % (key, bindings))

def test0():
    """ simple test of labels """
    result = U("pred'",
               [U('pred', []),
                U('&P',
                  [U('PredP',
                     [U('Pred', [ U('na',[]) ]),
                      U('DP', [ U('shamhradh',[]) ])]),
                   U("&'",
                     [U('&', [ U('no',[]) ]),
                      U('PredP',
                        [U('Pred', [ U('na',[]) ]),
                         U('DP', [ U('gheimhreadh',[]) ]) ]) ]) ]) ]).labels()
    if result != ["pred'", 'pred', '&P', 'PredP', 'Pred', 'na', 'DP',
                  'shamhradh', "&'", '&', 'no', 'PredP',
                  'Pred', 'na', 'DP', 'gheimhreadh']:
        print('test0 ERROR:', result)

def test1a():
    """ simple test of match: no vars"""
    result = U('a', []).match(U('b', []),[])
    if result != None:
        print('test1a ERROR:', result)

def test1b():
    """ simple test of match: no vars"""
    result = U('a', []).match(U('a', []),[])
    if result != []:
        print('test1b ERROR:', result)

def test1c():
    """ simple test of match: no vars"""
    result = U('a', [U('a', [])]).match(U('a', [U('b', [])]), [])
    if result != None:
        print('test1c ERROR:', result)

def test1d():
    """ simple test of match: all vars"""
    result = U('TV0', [U('TV1', [])]).match(U('TV2', [U('TV3', [])]), [])
    if result != [(U('TV0', [U('TV1', [])]), U('TV2', [U('TV3', [])]))]:
        print('test1d ERROR:', result)

def test1e():
    """ simple test of match: all vars"""
    result = U('TV0', [U('TV0', [])]).match(U('TV3', [U('TV4', [])]), [])
    if result != [(U('TV0', [U('TV0', [])]), U('TV3', [U('TV4', [])]))]:
        print('test1e ERROR:', result)

def test2a():
    """ simple test of LV0 match """
    result = U('LV0', []).match(U('a', []),[])
    print('LV0 MATCH =',repr(result))
    if result != [('LV0', 'a')]:
        print('test2a ERROR:', result)

def test2b(): 
   """ simple test of root TV0 match """
   result = U('TV0', []).match(U('a', []),[])
   if result != [(U('TV0', []), U('a', []))]:
       print('test2b ERROR:', result)

def test2c():
    """ simple test of embedded TV0 match """
    bindings = []
    result = U('a', [U('TV0',[])]).match(U('a', [U('b',[])]), bindings)
    if bindings != [(U('TV0', []), U('b', []))]:
        print('result =', result)
        print('bindings =', bindings)

def test2d():
    """ simple test of embedded TV0 match """
    bindings = []
    result = U('a', [U('TV0',[]),U('TV1',[])]).match(U('a', [U('b',[]),U('c',[])]), bindings)
    if result != [(U('TV0', []), U('b', [])), (U('TV1', []), U('c', []))]:
        print('result =', result)
        print('bindings =', bindings)

def test2e():
    """ simple test of match and instantiate """
    bindings = U('a', [U('TV0',[]),U('TV1',[])]).match(U('a', [U('b',[]),
                                                               U('PredP',
                                                                 [U('Pred',
                                                                    [U('na',[])]),
                                                                  U('DP',
                                                                    [U('shamhradh',[])])])]), [])

    result = U('result', [U('TV0',[]),
                           U('TV1',[])]).instantiate(bindings)

    if result != U('result', [U('b', []),
                               U('PredP',
                                  [U('Pred',
                                      [U('na', [])]),
                                   U('DP',
                                      [U('shamhradh', [])])])]):
        print('test3 ERROR:', result)
    else:
        print('No errors in test 2e of U.match and U.instantiate')

if __name__ == '__main__':
    # Choose the desired example:
    test2e()
