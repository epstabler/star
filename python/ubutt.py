# ubutt.py -- bottom-up tree transducer class and related functions
import itertools
from utree import *

# **IN PROGRESS**
# TODO: Ubutt.nonempty() -- any final state reachable?
# TODO: Ubutt.transduce(tree) -- dTransduce w backtracking?
# TODO: generalize M.o(N) for *-extended N

VERBOSE = False
VERBOSE = True

VVERBOSE = True   # very verbose!
VVERBOSE = False

class Urule:
    r"""
    A bottom-up transducer rule is given by
       - a treeIn and treeOut
       - a boolean condition represented as a string
       - a numeric weight

    We let

       self._left = treeIn, the `left side' of transition

       self._right = treeOut, the `right side'

       self._conds = the conditions

       self._weight = the weight

    """
    def __init__(self, treeIn, treeOut, conds='True', weight=0):
        if not(isinstance(treeIn,U)) or not(isinstance(treeOut,U)):
            raise TypeError(
                "%s: Expected input and output utrees " % type(self).__name__
            )
        elif not(isinstance(conds,str)):
            raise TypeError(
                "%s: Expected input and output utrees with string(Boolean)" % type(self).__name__
            )
        elif not(isinstance(weight,(int,float))):
            raise TypeError(
                "%s: Expected input and output utrees with string(Boolean) \
                and numeric weight" % type(self).__name__
            )
        else:
            self._left = treeIn
            self._right = treeOut
            self._conds = conds
            self._weight = weight

    def __repr__(self):
        return str((self._left, self._right, self._conds, self._weight))

    def oneNormalizeLeft(self, states, stateTrees, butt):
        """
        convert the left side of a ubutt rule to 1-normal form
        using construction in Lemma 14 of Engelfriet&al'09 (p576)
        """
        lcnt = self._left.symbolCount()

        if lcnt < 2:
            if VVERBOSE: print('left done: %s\n' % self.prettyString())
            butt._rules.append(self)

        elif not(self._left.isState()): # left_root an input, fix non-state children
            if VVERBOSE: print('\n%d symbols on left of %s' % (lcnt, self.prettyString()))
            for c in self._left._children:

                if not(c.isState()):
                    newState = 'q'+str(len(states))
                    states.append(newState)
                    stateTree = U(newState,c.vars())

                    stateTrees.append(stateTree) #moved from both

                    l1 = Urule( U(c._root, c.varsAndStates()),
                                U(newState, stateTree),
                                self._conds,
                                0  # auxiliary rules get 0 weight
                               )

                    if VVERBOSE: print('left1: %s' % l1.prettyString())
                    l1.oneNormalizeLeft(states, stateTrees, butt)

                else:
                    # we should not reach this because symbolCount ignores states
                    raise RuntimeError('oneNormalizeLeft: state child of nonstate')
                    stateTree = c

            l2 = Urule( U(self._left._root, stateTrees),
                        self._right,
                        self._conds,
                        self._weight # final rule gets original weight
                       )

            if VVERBOSE: print('left2: %s' % l2.prettyString())

            l2.oneNormalizeLeft(states, stateTrees, butt)

        else: # left._root is a state
            raise RuntimeError('oneNormalizeLeft: left root is state, but not 1-normal?')

    def oneNormalizeRight(self, states, butt):
        """
        Convert the right side of a ubutt rule to 1 normal form
        using the construction in proof of Thm 15 in Engelfriet&al'09 (p577)
        """
        cnt = self._left.symbolCount() + self._right.symbolCount()
        if VVERBOSE: print('\n%d symbols in %s' % (cnt, self.prettyString()))
        if cnt < 2:
            if VVERBOSE: print('right done: %s\n' % self.prettyString())
            butt._rules.append(self)
        else:
            # r1
            newState = 'q'+str(len(states))
            states.append(newState) 

            newChildren = []
            chosen = -1
            for i,c in enumerate(self._right._children):
                if chosen < 0 and not(c.isVar()):
                    chosen = i
                    replacedChild = c
                    newChildren.extend(replacedChild._children)
                else:
                    newChildren.append(c)
            if chosen < 0:
                raise RuntimeError('oneNormalizeRight error: No non-var found')

            if VVERBOSE: print('replacing on right:',replacedChild)

            if len(newChildren) != len(self._right._children) + len(replacedChild._children) - 1:
                raise RuntimeError('oneNormalizeRight: Wrong number of args for: newState')

            r1 = Urule( self._left,
                        U(newState, newChildren),
                        'True',
                        0  # auxiliary rules get 0 weight
                       )
            if VVERBOSE: print('right1: %s' % r1.prettyString())

            # r2
            newVars = [U('TV'+str(j),[]) for j in range(len(newChildren))]

            newArgs = [U('TV'+str(j),[]) for j in range(chosen)]
            midpoint = chosen+len(replacedChild._children)
            newArgs.append( U(replacedChild._root,
                              [U('TV'+str(j),[]) for j in range(chosen, midpoint)])
                           )
            finalpoint = len(self._right._children)-1+len(replacedChild._children)
            newArgs.extend([U('TV'+str(j),[]) for j in range(midpoint, finalpoint)])

            if len(newArgs) != len(self._right._children):
                raise RuntimeError('oneNormalizeRight: Wrong number of args for: right._root')

            r2 = Urule( U(newState, newVars),
                        U(self._right._root, newArgs ),
                        'True',
                        self._weight # final rule gets original weight
                       )

            if VVERBOSE: print('right2: %s' % r2.prettyString())

            butt._rules.append(r2)

            r1.oneNormalizeRight(states, butt)

    def prettyString(self):
        if self._conds == 'True' and self._weight == 0:
            return '%s -> %s' % (str(self._left), str(self._right))
        elif self._conds == 'True':
            return '%s -> %s %s' % (str(self._left), str(self._right), 'x')
        elif w == 0:
            return '%s -> %s if %s' % (str(self._left), str(self._right), self._conds)
        else:
            return '%s -> %s if %s %s' % (str(self._left), str(self._right), self._conds, 'x')

class Ubutt:
    r"""
    A bottom up U-tree transducer is given by
       - a list of transitions and list of final states
    """
    def __init__(self, rules, finals):
        if any([not(isinstance(r,Urule)) for r in rules]) or any([not(isinstance(f,str)) for f in finals]):
            raise TypeError(
                "%s: Expected Urule list and list of final states " % type(self).__name__
            )
        else:
            self._rules = rules
            self._finals = finals

    def __repr__(self):
        return str((self._rules, self._finals))

    def states(self):
        # the states of fst are the roots of all rule right sides,
        #  -- there should be no useless states on left, they would be missed
        s = []
        for r in self._rules:
            s.extend(r._left.states())
            s.extend(r._right.states())
        return list(set(s))

    def statesArities(self):
        # the states of fst are the roots of all rule right sides,
        #  -- there should be no useless states on left, they would be missed
        sa = []
        for r in self._rules:
            sa.extend(r._left.statesArities())
            sa.extend(r._right.statesArities())
        return list(set(sa))

    def isOneNormal(self):
        """ return True iff uxmbutt is in 1-normal form """
        errors = {}
        for r in self._rules:
            symbols = r._left.symbolCount() + r._right.symbolCount()
            if symbols != 1:
                if symbols in errors.keys():
                    errors[symbols] += [r]
                else:
                    errors[symbols] = [r]
        if errors == {}:
            return True
        else:
            if VERBOSE:
                print('Not 1-normal:')
                for i in errors.items():
                    (syms, rules) = i
                    print('rules with %d symbols:' % syms)
                    for r in rules: print('   ', r.prettyString())
            return False

    def isSasOneNormal(uxmbutt):
        """ This variant of 1-normal form allows
            epsilon transitions
        """
        for r in uxmbutt:
            if r._left.symbolCount() + r._right.symbolCount() > 1:
                return False
        return True

    def oneNormalize(self, states):
        """ return the 1-normal form of uxmbutt,
            as defined in Engelfriet&al'09

        The states of self are also augmented in place,
        so they need not be recalculated at each step.

        TEMPORARILY, no * or + rules allowed in umbutt.
        """
        u1 = Ubutt([],self._finals)
        for r in self._rules:
            r.oneNormalizeLeft(states, [], u1)
        u2 = Ubutt([],self._finals)
        for r in u1._rules:
            r.oneNormalizeRight(states, u2)
        return u2

    def isDeterministic(self):
        """ given uxmbutt, return True iff xbutt is deterministic.

          Engelfriet, Lilin, and Maletti 2009 section 3 say (p567):
            "M is deterministic if
              (i) there do not exist two distinct rules
                  l1→r1 ∈ R and l2→r2 ∈ R,
                  a substitution θ: X → X, and w∈pos(l2) such that
                  l1θ = l2|w, and
              (ii) there does not exist an epsilon rule l → r ∈ R
                  such that l(ε) ∈ F.
            Note that, intuitively speaking, in a deterministic xmbutt
            there exist no useful states that contribute to a cycle of
            epsilon rules and thus epsilon rules can be removed in the
            standard manner from a deterministic xmbutt"

          For now, we check:

              no rule._left._root is a final state, and

              no 2 rules are such that the left side of one is
              an instance of any subtree of the left side of another,
              where the relevant subtrees ('symbolSubtrees')
              are those with a symbol at the root (not a state)
        """
        leftSideParts = []
        for r in self._rules:
            #left = r[0]
            if r._left._root in self._finals:
                return False
            for t in leftSideParts:
                ok = t.match(r._left, [])
                if ok != None:
                    return False
            leftSideParts.extend(r._left.symbolSubtrees())
        return True

    def step(self, treeIn):
        """ if any rule applies to treeIn, return result """
        for r in self._rules:
            bindings = r._left.match(treeIn, [])
            if VVERBOSE:
                print('\nrule = ' , r.prettyString())
                print('tree = ' , treeIn)
                print('bindings = %r' % bindings)
            if bindings != None and evalsToTrue(r._conds, bindings):
                result = r._right.instantiate(bindings)
                if VVERBOSE:
                    print("condition %r = 'True'" % r._conds)
                    print('result = %r' % result)
                if result != None:
                    return(result)
                else:
                    return treeIn
        if VVERBOSE: print('no rule applies')
        return treeIn

    def dTransduce(self, treeIn):
        """ self assumed deterministic,
            so whenever a rule applies, they are applied
            (and other rules that could have applied are ignored).
            Note that we do not *check* determinacy,
            since sometimes we apply this function even to
            non-deterministic transducers, interpreting
            the results accordingly.
        """
        if treeIn._children == [] or treeIn.isState():
            out = self.step(treeIn)
            if out == treeIn:
                return out
            else:
                return self.dTransduce(out)
        else:
            out = self.step(U(treeIn._root,[self.dTransduce(c) for c in treeIn._children]))
            if out == treeIn:
                return out
            else:
                return self.dTransduce(out)

    def o(self, N):
        """return: self o N,
            the composition of self with N,
            where self and N are 1-normal, linear mbutts.

        So M.oN(t) is the tree that results from
          applying N to t and then
          applying M to that result.

       See Def 19 of Engelfriet&al'09 (pp582f),
         which we generalize to the case where a transition
         can be marked as having a weight of 0 or 1,
         where 1 = dispreferred, a "violation".
         Linguists often indicate weight 1 with a star.
         To set the stage for OT successive pruning,
         weights in composed rules are only those of N;
         M weights are discarded.

        To compute compositions:

          For the calculation, we use the Engelfriet&al "unofficial"
            representation of composed states -- so the composed M.o(N) state 
            of a rule output is given by its M-root q and the N-roots
            of its children q1...qn.

          Then, we return (a variant of) the "official" notation in
            the returned rules.  For Engelfriet&al the composed states
            of the official notation are atoms that contain brackets:
            q<q1...qn>.

        The reason for converting to this "official" notation, where the
            state is an atom, is so that, even in repeated compositions,
            K.o(L.o(M.o(N...))),
            we never need to go deeper than depth 1 in the unofficial
            trees to identify the state.

       We assume self and N are linear, 1-normal-form transducers, and:
          - there are no * or + rules in self,
          - AND TEMPORARILY: no * or + rules in N either,

       We try to follow Engelfriet&al (p581) almost exactly, distinguishing 3 cases
        in the definition of the transducer M.o(N):
               R1:  It "consumes input by simulating M,
               R2:      it produces output by simulating N, and
               R3:      when M produces an output symbol, it feeds
                           that symbol immediately as input to N"
        """
        M = self # facilitating comparison with Engelfriet&al def
        rules = []

        for Mr in M._rules:  ### look for cases of R1
            if Mr._left.isSymbol():
                if VVERBOSE: print('computing R1 with Mr = %s\n' % Mr.prettyString())
                MleftVars = Mr._left.vars()
                # all len(MleftVar)-tuples of N states
                for v in itertools.product(*([N.statesArities()] * len(MleftVars))):
                    if VVERBOSE: print('v =', v)
                    # then partition vars according to state arities
                    vector, index = [], 0
                    for x in v:
                        vector.extend([U(x[0], MleftVars[index:index+x[1]])])
                        index += x[1]
                    bindings = list( zip(MleftVars, vector) )
                    if VVERBOSE: print('bindings =', bindings)

                    Mli = Mr._left.instantiate(bindings)
                    Mri = Mr._right.instantiate(bindings)

                    newRule = Urule(Mli, Mri, Mr._conds, 0)
                    if VERBOSE: print('R1(Mr o Nr) = %s' % newRule.prettyString())
                    rules.append(newRule)

            else: ### look for cases of R3
                for i,c in enumerate(Mr._right._children):

                    if c.isSymbol():
                        if VVERBOSE:
                            print('found emitted symbol in M rule: %s' % Mr.prettyString())
                            print('\nPreparing all M rule var bindings for composition...')
                            for v in itertools.product(*([N.statesArities()] * len(Mr._left.vars()))):
                                print('v =',v)
                            print()

                        for v in itertools.product(*([N.statesArities()] * len(Mr._left.vars()))):
                            # for binding, build a vector of properly numbered trees with vars
                            treeVector = []
                            varcnt = 0
                            for (s,a) in v:
                                subtrees = []
                                for _ in range(a):
                                    subtrees.append(U('TV'+str(varcnt),[]))
                                    varcnt += 1
                                treeVector.append(U(s,subtrees))

                            Mbindings = list( zip(Mr._left.vars(), treeVector) )
                            if VVERBOSE:
                                print('Mbindings:')
                                for b in Mbindings: print('    ',b)
                                print()

                            Mri = Mr._right.instantiate(Mbindings)
                            for Nr in N._rules:
                                Nbindings = Nr._left.match(Mri._children[i], [])
                                if Nbindings != None:
                                    if VVERBOSE:
                                        print('Nbindings:')
                                        for b in Nbindings: print('    ',b)
                                        print()
    
                                        print('with v =',v)
                                        print('found matching rule in N: %s\n' % Nr.prettyString())

                                    Mli = Mr._left.instantiate(Mbindings)
                                    Mri = Mr._right.instantiate(Mbindings)
                                    Minstance = Urule(Mli, Mri, Mr._conds, Mr._weight)
                                    if VVERBOSE:
                                        print('Minstance: %s\n' % Minstance.prettyString())

                                    Nri = Nr._right.instantiate(Nbindings)
                                    newSubtrees = Mri._children[:i]
                                    newSubtrees.append(Nri)
                                    newSubtrees.extend(Mri._children[i+1:])
                                    newR = U(Mri._root, newSubtrees)
                                    newRule = Urule(Mli, newR, composeBoolStr(Mr._conds, Nr._conds), Nr._weight)

                                    if VERBOSE: print('R3(Mr o Nr) = %s' % newRule.prettyString())
                                    rules.append(newRule)

        moreRules = []

        for Mr in rules:  ### look for cases of R2 -- beginning with already composed rules

            MatchingNrEpsilons = [ [Nr for Nr in N._rules if c.match(Nr._left, []) != None]
                                   for c in Mr._right._children ]

            if any([e != [] for e in MatchingNrEpsilons]): # if some N epsilon rule applies to some child
                if VVERBOSE: print('computing R2 with Mr = ', Mr.prettyString())

                # add None for no-substitution option in each position
                MatchingNrEpsilons = [ c+[None] for c in MatchingNrEpsilons]

                for v in itertools.product(*MatchingNrEpsilons):
                    if any([e != None for e in v]): # make at least 1 substitution
                        newLeft = Mr._right
                        newRightRoot = Mr._right._root
                        newRightChildren = []
                        for i,c in enumerate(Mr._right._children):
                            if v[i] == None: # None means 'leave this child unchanged'
                                newRightChildren.append(c)
                            else:
                                newRightChildren.append(v[i]._right.instantiate(v[i]._left.match(c, [])))
                        newRule = Urule(newLeft, U(newRightRoot, newRightChildren), Mr._conds, 0)
                        if VERBOSE: print('R2(Mr o Nr) = ', newRule.prettyString())
                        moreRules.append(newRule)

        rules.extend(moreRules)

        officialRules = [Urule(r._left.makeOfficial(), r._right.makeOfficial(), r._conds, r._weight)
                         for r in rules]
        officialFinals = [str(U(f,[U(ff,[])])) for f in M._finals for ff in N._finals]

        newButt = Ubutt(officialRules, officialFinals).epsilonsOut()
        return newButt.oneNormalize(newButt.states())

    def epsilonsOut(self):
        epsilons, newRules = [], []
        for r in self._rules:
            if r._left.symbolCount() == 0 and r._right.symbolCount() == 0:
                epsilons.append(r)
            else:
                newRules.append(r)

        if epsilons:
            if VVERBOSE:
                print('epsilon rules to be removed:')
                for e in epsilons:
                    print('  ',e.prettyString())
                print('epsilon states to be removed from:')
                for r in newRules:
                    print('  ',r.prettyString())
        else:
            return(self) # shortcut

        addRules = set([])
        removeRules = set([])

        for r in newRules:
            for e in epsilons:
                if [n for n in newRules if e._right._root == n._right._root]:
                    epsilonRightStateOnlyCreatedByEpsilonRules = False
                else:
                    epsilonRightStateOnlyCreatedByEpsilonRules = True
                if r._right.isState():
                    b = e._left.match(r._right, [])
                    if b != None:
                        newRule = Urule(r._left, e._right.instantiate(b), r._conds, r._weight)
                        addRules.add(newRule)
                        # is e._right produced by any non-epsilon rules?
                        for r in newRules:
                            if epsilonRightStateOnlyCreatedByEpsilonRules:
                                removeRules.add(r)

        for r in removeRules: newRules.remove(r)
        newRules.extend(addRules)
        return Ubutt(newRules, self._finals)

    def prettyRules(self):
        """ write the transitions in a pretty form """
        for r in self._rules: print(r.prettyString())

def uId(utree, rules=[], stateCtr=0, varCtr=0):
    """ given (utree, rules, stateCtr, varCtr) with rules initially empty,
          augmented rules in place for identity transduction of utree.

        Returns rules, stateTree, varTree, stateCtr --
          stateTree and varTree are used in the rule that tests this tree,
          and stateCntr is used for constructing any new states.

    The returned identity transducer is always an unextended
      linear, nondeleting, and deterministic butt.

    That means that the composition (uId(T1) o butt),
      when butt is linear, nondeleting, epsilon-free,
      will also be linear, nondeleting, epsilon-free.
      (See Thm 23 of Engelfriet&al'09, p577)
    """
    stateTrees = []
    varTrees = []
    cvarCtr = 0 # number child vars starting from 0
    for c in utree._children:
        _, cstate, cvar, stateCtr = uId(c, rules, stateCtr, cvarCtr)
        stateTrees.append(cstate)
        varTrees.append(cvar)
        cvarCtr += 1
    sofar = Ubutt(rules,'').dTransduce(utree)
    if sofar and sofar.isState(): # first, ck if utree is already recognized
        state = sofar._root
    else:                         # otherwise, add transition to new state
        state = 'q'+str(stateCtr)
        transition = Urule( U(utree._root, stateTrees),
                            U(state,[ U(utree._root, varTrees) ]),
                            'True',
                            0 # uId transition weights are all set to 0
                           )
        rules.append(transition)
        stateCtr += 1
    varTree = U('TV'+str(varCtr),[])
    stateTree = U(state,[ varTree ])
    return rules, stateTree, varTree, stateCtr

def evalsToTrue(c, bindings):
    """ 
    We can represent rules as strings c that denote
    regular, Boolean conditions on the variables,
    using the python eval.

    c must be a unary, boolean condition on vars bound to labels,
      so only label var (LV) and star var (SV) bindings are passed into eval.

    For now, we leave it to the user to stick to *regular* conditions.
    """
    if c == 'True':
        return True
    else:
        sbindings = [b for b in bindings if len(b[0]) > 1 and b[0][0:2] in ['LV','SV']]
        return eval(c, dict(sbindings))

def composeBoolStr(B1,B2):
    """ Compose the Boolean conditions of rules.
        We could keep redundant True's, but no need to. """
    if B1 == 'True':
        return B2
    elif B2 == 'True':
        return B1
    else:
        return B1 + ' and ' + B2

### Functions for reading and converting printed versions of transducers
def lines2rules(s):
    return [string2rule(x.strip()) for x in s.split('\n') if x.strip()]

# non-zero weights indicated by 1 or more x's at the end of the line
ruleWeight = re.compile("(.*)\s+(x+)\s*$")

def string2rule(sw):
    r = re.match(ruleWeight, sw)
    # first, check to see if rule is marked with weight, "x"
    if r:
        s,w = r.group(1).strip(), 1
    else:
        s,w = sw, 0
    # now parse the rule itself
    [s0, s1c] = s.split('->')
    s1cParts = s1c.split('if')
    if len(s1cParts) == 1:
        return Urule(U.fromstring(s0), U.fromstring(s1cParts[0]), 'True', w)
    elif len(s1cParts) == 2:
        return Urule(U.fromstring(s0), U.fromstring(s1cParts[0]), s1cParts[1].strip(), w)
    else:
        RuntimeError('string2rule error')
