"""duxmbutt.py

    Implementing deterministic, unranked, extended multi bottom-up
    tree transducers.  See e.g.
    
       Engelfriet, Lilin, and Maletti 2009
       https://link.springer.com/article/10.1007/s00236-009-0105-8}
    
    Each transducer is given as a list of rules.
    Rules have the form 
        (treeIn, treeOut, conditions, weight),
    where conditions are boolean tests on bound variables
    that use only builtin operators.
    For now, weight \in {0,1}, with weight 1 indicated in the
    pretty-printed rule by an "x" at the end of the line.
    See umbutts/ for examples.
    
    States all begin with 'q'.
    The rules can be 'extended' in the sense that the left side can
    contain more structure than the next node and state subtrees.
    Label variables and tree variables with conditions are allowed.

    NB: This implementation is very preliminary -- providing an
    initial check on how the ideas in the paper can work together, and
    providing a basis for further experimentation and development.

"""
import re
from utree import *
#from nltk.tree.tree import * # NLTK is optional, needed only for tree graphics

# for tests: examples from umbutts subdirectory
sys.path.insert(1, 'umbutts')
from propCalcUMBUTT import *
from propCalc import *
from propCalcId import *
from propCalcIdUMBUTT import *
from propCalcStar import *
from propCalcIdStar import *

VERBOSE = False
if VERBOSE: print('duxmbutt sets VERBOSE = True')

ruleWeight = re.compile("(.*)\s+(x+)\s*$")

def duxmbutt(rules, treeIn):
    """ Since the tranducer is here assumed to be deterministic,
        the first rules that *can* apply *are* applied.
    """
    if isinstance(treeIn,U):
        if treeIn.children() == []:
            result = step(rules, treeIn)
            if result == None:
                return(treeIn)
            else:
                return(result)
        else:
            newtree = U(treeIn.root(),[duxmbutt(rules,t) for t in treeIn.children()])
            result = step(rules, newtree)
            if result == None:
                return(newtree)
            else:
                return(result)
    else:
        print('ERROR - not a tree: %r' % treeIn)

def step(rules, treeIn):
    """ if any rule applies to treeIn, return result """
    for r in rules:
        bindings = r[0].match(treeIn, [])
        if VERBOSE:
            print('\nrule = ' , r)
            print('tree = ' , repr(treeIn))
            print('bindings = %r' % bindings)
        if bindings != None and condition(r[2], bindings):
            result = r[1].instantiate(bindings)
            if VERBOSE:
                print('%r = True' % r[2])
                print('result = %r' % result)
            return(result)
    else:
        if VERBOSE: print('no rule applies')

def condition(c, bindings):
    """ 
    c must be a unary, boolean condition on vars bound to labels,
      so only label var (LV) and star var (SV) bindings are passed into eval.
    """
    if c == 'True':
        return True
    else:
        sbindings = [b for b in bindings if len(b[0]) > 1 and b[0][0:2] in ['LV','SV']]
        result = eval(c, dict(sbindings))
        return(result)

### Functions for reading and converting printed versions of transducers
def lines2rules(s):
    return [string2rule(x.strip()) for x in s.split('\n') if x.strip()]

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
        return (U.fromstring(s0), U.fromstring(s1cParts[0]), 'True', w)
    elif len(s1cParts) == 2:
        return (U.fromstring(s0), U.fromstring(s1cParts[0]), s1cParts[1].strip(), w)
    else:
        RuntimeError('duxmbut string2rule error')

def rule2string(r):
    u1, u2, c, w = r
    if c == 'True' and w == 0:
        return '%s -> %s' % (str(u1), str(u2))
    elif c == 'True':
        return '%s -> %s %s' % (str(u1), str(u2), 'x')
    elif w == 0:
        return '%s -> %s if %s' % (str(u1), str(u2), str(c))
    else:
        return '%s -> %s if %s %s' % (str(u1), str(u2), str(c), 'x')

def prettyRules(rs):
    """ write the transitions in a pretty form """
    for r in rs: print(rule2string(r))

def drawRules(rs):
    """ Use NLTK TK graphics to draw the rules in a pretty form.
        Currently, weights are ignored.
    """
    print('drawing %d rules...' % len(rs))
    for i,r in enumerate(rs):
        nltk0 = Tree.fromstring(str(r[0]))
        nltk1 = Tree.fromstring(str(r[1]))
        if r[2] == 'True':
            Tree('rule%d' % i,[nltk0,Tree('->',[]),nltk1]).draw()
        else:
            Tree('rule%d' % i,[nltk0,Tree('->',[]),nltk1,Tree('if %s' % r[2],[])]).draw()

if __name__ == '__main__':
    #g,s,cat = (propCalcUMBUTT, U('P',[]) , 'q0')
    #g,s,cat = (propCalcUMBUTT, U('&', [U('P', []), U('Q', [])]) , 'q0')
    #g,s,cat = (propCalcUMBUTT, U('v', [U('P', []), U('Q', [])] ) , 'q1')
    #g,s,cat = (propCalcUMBUTT, U('&', [U('Q', []), U('not',[U('P', [])])]) , 'q1')
    #g,s,cat = (propCalcUMBUTT, U('&', [U('Q', [])]) , 'NONE')

    #g,s,cat = (propCalc, U('P',[]) , 'q0')
    #g,s,cat = (propCalc, U('&', [U('P', []), U('Q', [])]) , 'q0')
    #g,s,cat = (propCalc, U('v', [U('P', []), U('Q', [])] ) , 'q1')
    #g,s,cat = (propCalc, U('&', [U('Q', []), U('not',[U('P', [])])]) , 'q1')
    #g,s,cat = (propCalc, U('&', [U('Q', [])]) , 'NONE')

    #g,s,cat = (propCalcId, U('P',[]) , '(q0 id)')
    #g,s,cat = (propCalcId, U('v', [U('P', []), U('Q', [])] ) , '(q1 id)')
    #g,s,cat = (propCalcId, U('&', [U('Q', []), U('not',[U('P', [])])]) , '(q1 id)')
    #g,s,cat = (propCalcId, U('&', [U('Q', [])]) , 'NONE')

    #g,s,cat = (propCalcIdUMBUTT, U('P',[]) , 'q0')
    #g,s,cat = (propCalcIdUMBUTT, U('&', [U('P', []), U('Q', [])]) , 'q0')
    #g,s,cat = (propCalcIdUMBUTT, U('v', [U('P', []), U('Q', [])] ) , 'q1')
    g,s,cat = (propCalcIdUMBUTT, U('&', [U('Q', []), U('not',[U('P', [])])]) , 'q1')
    #g,s,cat = (propCalcIdUMBUTT, U('&', [U('Q', [])]) , 'NONE')

    #g,s,cat = (propCalcStar, U('P',[]) , '(q0 id)')
    #g,s,cat = (propCalcStar, U('v', [U('P', []), U('Q', [])] ) , '(q1 id)')
    #g,s,cat = (propCalcStar, U('&', [U('Q', []), U('not',[U('P', [])])]) , '(q1 id)')
    #g,s,cat = (propCalcStar, U('&', [U('Q', [])]) , 'NB: q1')
    #g,s,cat = (propCalcStar, U('&', []) , 'NB: q1')
    #g,s,cat = (propCalcStar, U('v', [U('Q', [])]) , 'NB: q1')
    #g,s,cat = (propCalcStar, U('v', []) , 'NB: q0')
    #g,s,cat = (propCalcStar, U('&', [U('Q', []),U('Q', []),U('Q', []),U('Q', []),U('Q', []),U('P', [])]) , 'NB: q0')
    #g,s,cat = (propCalcStar, U('&', [U('Q', []),U('Q', []),U('Q', []),U('Q', []),U('Q', []),U('Q', [])]), 'NB: q1')

    #g,s,cat = (propCalcIdStar, U('P',[]) , '(q0 id)')
    #g,s,cat = (propCalcIdStar, U('v', [U('P', []), U('Q', [])] ) , '(q1 id)')
    #g,s,cat = (propCalcIdStar, U('&', [U('Q', []), U('not',[U('P', [])])]) , '(q1 id)')
    #g,s,cat = (propCalcIdStar, U('&', [U('Q', [])]) , 'NB: q1')
    #g,s,cat = (propCalcIdStar, U('&', []) , 'NB: q1')
    #g,s,cat = (propCalcIdStar, U('v', [U('Q', [])]) , 'NB: q1')
    #g,s,cat = (propCalcIdStar, U('v', []) , 'NB: q0')
    #g,s,cat = (propCalcIdStar, U('&', [U('Q', []),U('Q', []),U('Q', []),U('Q', []),U('Q', []),U('P', [])]) , 'NB: q0')
    #g,s,cat = (propCalcIdStar, U('&', [U('Q', []),U('Q', []),U('Q', []),U('Q', []),U('Q', []),U('Q', [])]), 'NB: q1')

    gU = lines2rules(g)
    prettyRules(gU)
    print('\nAttempting to transduce this syntactic structure (to %s):\n' % cat)
    print(s)
    output = duxmbutt(gU, s)
    print('\nResult:\n')
    print(output)
