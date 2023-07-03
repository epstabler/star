"""  cfgckyp.py
     Given a context free grammar and an input string,
     compute matrix of derivable categories,
     and collect derivation trees rooted with START category, if any.

     For an example, type:

         python cfgckyp.py 

     Alternative nonempty expansions of any lhs category can be 
     indicated by separating them by | on the rhs.

     For any nonterminal C on rhs of any rule, C* and C+
     indicate (0 or more) and (1 or more) occurences.
     When the grammar g is read in, the empty option of C*
     is represented by having a gdict trigger where it is absent,
     and then every categories in gdict rules is represented by a pair,
         (C,''), (C,'*'), or (C,'+').
     When looking for either (C,*) or (C,+), if a C is found,
     we also continue looking for more Cs.

     Note that in any particular parse, there is a determinate
     number of Cs, and so C* or C+ never appear in the chart of
     derived constituents or in the derivation tree.

     Set VERBOSE = True to get (very) verbose output.

     NB: Of course, CKY parsing with Chomsky normal form CFGs
         is much more efficient: https://en.wikipedia.org/wiki/CYK_algorithm
         See the literature on grammar binarization.
         But here we aim for a flexible tool, for experiments on small problems.

     NB: Tree collection can fail to terminate on grammars with cycles.
"""
VERBOSE = False

START = 'S'

g1 = """
S -> NP VP | S+ &S
&S -> and S
NP -> Name | N' | N' CP | NP+ &NP
&NP -> and NP
N' -> AP* N | N'+ &N'
&N' -> and N'
N -> N N | Name N | N+ &N
&N -> and N
AP -> DegP A | A | AP+ &AP
&AP -> and AP
A -> A+ &A
&A -> and A
DegP -> Deg | DegP+ &DegP
&DegP -> and DegP
Deg -> Deg+ &Deg
&Deg -> and Deg
VP -> V NP | V | VP+ &VP
&VP -> and VP
V -> V+ &V
&V -> and V
CP -> Op C' | CP+ &CP
&CP -> and CP
C' -> C S/NP | C'+ &C'
&C' -> and C'
S/NP -> NP VP/NP | Op VP | S/NP+ &S/NP
&S/NP -> and S/NP
VP/NP -> V Op | VP/NP+ &VP/NP
&VP/NP -> and VP/NP

V -> see | laugh | help | bother | eat | drank | buffalo | police | char | bark | meow | dog | refuse | chase
N -> students | teachers | dogs | cats | buffalo | police | kind | saw | mice | rats | cheese | char
A -> courageous | gentle | compassionate | honest | alert | excited | kind | orange | black
Deg -> very | extremely | slightly | hardly | mostly | rarely | incredibly | monumentally | not at all
Name -> Massachusetts | Amherst | Los Angeles | Victoria | British Columbia | Buffalo | New York
C -> that
C -> 
Op -> 
"""

g2 = """
S -> A B
A -> a | a a
B -> b | a b
"""

def accepts(g, inputwords):
    """ search for derivation of inputwords,
        and if found, ask user to select a tree
    """
    empties, gdict = g2gdict(g)
    matrix, agenda = initialize(inputwords, empties)
    print('parsing: %s' % ' '.join(inputwords))
    closeMatrix(gdict, matrix, agenda)
    cornercats = [e[0] for e in matrix[0][len(inputwords)]]
    if VERBOSE: print('spanning whole string:', cornercats)
    success = START in cornercats
    print(success)
    if success:
        stack = [([(0,len(inputwords),'S')],[])]
        return(getTree(stack, matrix))

def initialize(inputWords, empties):
    """ put inputWords and empty categories into matrix and agenda """
    mLen = len(inputWords) + 1        # intuitively, we add position 0
    matrix = [ [ [] for i in range(mLen) ] for j in range(mLen) ]
    agenda = []
    for i in range(mLen):
        matrix[i][i].extend([(e,[]) for e in empties]) # empty source list, signalling lexical
        agenda.extend([(i,i,e) for e in empties])
    agenda.extend([(i,i,e) for e in empties for i in range(mLen)])
    for i,w in enumerate(inputWords):
        matrix[i][i+1].append((w,[]))  # empty source list, signalling lexical
        agenda.append((i,i+1,w))
    return matrix, agenda

def closeMatrix(gdict, matrix, agenda):
    """ close matrix wrt rules in gdict """
    matrixMax = len(matrix)
    while agenda:
        if VERBOSE: showMatrix(matrix)
        (i,j,x) = agenda.pop()
        if VERBOSE: print('\npopped from agenda:',(i,j,x))
        if x in gdict.keys():
            for pre,post,lhs in gdict[x]:
                if VERBOSE: print('wanting to make: %s' % lhs)
                leftEdges = findLefts(matrix, [(i,[[]])], pre)
                rightEdges = findRights(matrix, [(j,[[]])], post)
                for r,rsources in rightEdges:
                    for l,lsources in leftEdges:
                        prevlhs = [e for e in matrix[l][r] if e[0]==lhs]
                        sources = [a+[(i,j,x)]+b for a in lsources for b in rsources]
                        if not(prevlhs):
                            matrix[l][r].append((lhs,sources))
                            agenda.append((l,r,lhs))
                            if VERBOSE: print('found:',(l,r,lhs))
                        elif not(sources in prevlhs[0][1]): # update prev sources
                            ix = matrix[l][r].index(prevlhs[0])
                            matrix[l][r][ix][1].extend(sources)
                            if VERBOSE: print('updated:',(l,r,lhs))
                        elif VERBOSE: print('already:',(l,r,lhs))
        else:
            print('WARNING: closeMatrix: %r not in grammar' % x)
            #raise RuntimeError('closeMatrix: %r not in grammar' % x)

def findLefts(matrix, leftEdges, cats):
    """ find cats preceding leftEdges in matrix """
    i = len(cats)
    while i > 0:
        cat = cats[i-1][0]
        if VERBOSE: print('looking for %s on left' % cat)
        newLeftEdges = []
        for e,srcs in leftEdges:
            if VERBOSE: print('checking: %s' % ' '.join(['[%d][%d]' % (j,e) for j in range(e+1)]))
            for j in range(e+1):
                prevcats = [e for e in matrix[j][e] if e[0]==cat]
                if prevcats:
                    if VERBOSE: print('got one in [%d][%d]' % (j,e))
                    newSources = [[(j,e,cat)]+a for a in srcs]
                    newLeftEdges.append((j,newSources))
                    if cats[i-1][1] in ['+','*']: # keep looking!
                        newLeftEdges.extend(findLefts(matrix, [(j,newSources)], [cats[i-1]]))
        leftEdges = newLeftEdges
        i += -1
    return leftEdges

def findRights(matrix, rightEdges, cats):
    """ find cats following right edges in matrix """
    i = 0
    while i < len(cats):
        cat = cats[i][0]
        if VERBOSE: print('looking for %s on right' % cat)
        newRightEdges = []
        for e,srcs in rightEdges:
            for j in range(e,len(matrix)):
                if VERBOSE: print('checking: %s' % ' '.join(['[%d][%d]' % (e,j) for j in range(e,len(matrix))]))
                prevcats = [e for e in matrix[e][j] if e[0]==cat]
                if prevcats:
                    if VERBOSE: print('got one in [%d][%d]' % (e,j))
                    newSources = [a+[(e,j,cat)] for a in srcs]
                    newRightEdges.append((j,newSources))
                    if cats[i][1] in ['+','*']: # keep looking!
                        newRightEdges.extend(findRights(matrix, [(j,newSources)], [cats[i]]))
        rightEdges = newRightEdges
        i += 1
    return rightEdges

def getTree(stack, matrix):
    """ collect derivations with root START from matrix m, in list
        notation: ['S', ['NP', ['dogs']], ['VP', ['V', ['bark']]]],
        one at a time, returning one user selects (if any).
    """
    if VERBOSE: print('collecting trees...')
    if VERBOSE: print('stack =', stack)
    while stack:
        (todo,history) = stack.pop()
        if todo:
            if VERBOSE: print('todo[0] =', todo[0])
            i,j,cat = todo[0]
            sources = [e for e in matrix[i][j] if e[0]==cat][0][1]
            if sources:
                for s in sources:
                    item = (s+todo[1:],history+[(cat,len(s))])
                    # avoid possible redundancy in cases like A -> a a
                    if not(item in stack): stack.append(item)
            else:
                stack.append((todo[1:],history+[(cat,0)]))
            if VERBOSE: 
                for k,x in enumerate(reversed(stack)): # reverse b/c pop from end
                    print('  %d. %r' % (k,x))
        else:
            tree = lrr2tree(history,[])
            pptree(0, tree)
            response = input('Type return if ok. Type ";" or any character to continue search): ')
            if not(response):
                return tree
    print('no (more) trees')

def lrr2tree(h, stack):
    """ build tree from reversed(LR) traversal """
    if h:
        (c,a) = h.pop()
        if a:
            stack = [[c]+stack[:a]] + stack[a:]   # non-terminal
        else:
            stack = [[c]] + stack        # arity 0, i.e. terminal
        return lrr2tree(h, stack)
    else:
        if len(stack) != 1: raise RuntimeError('lrr2tree error')
        return stack[0]

### Functions for reading, displaying, converting data structures
def g2gdict(s):
    """ From multiline string s, return dict representation of grammar.
        Since parser is bottom-up, rules are represented
           by a dictionary from rhs elements to (pre,post,lhs)
              { rhs[i] -> [ (rhs[:i], rhs[i+1:], lhs) ] },
        and empty categories are returned separately
    """
    gdict = {}
    empties = []
    for line in s.split('\n'):
        if line.strip():
            ruleParts = [x.strip() for x in line.split('->') if x.strip()]
            if len(ruleParts) == 1: # empty category, no rhs
                lhs = ruleParts[0]
                if not((lhs,[]) in empties):
                    empties.append(lhs)
                if not(lhs in gdict.keys()):
                    gdict[lhs] = []
            elif len(ruleParts) == 2:
                lhs, rhsUnion =  ruleParts[0].strip(), ruleParts[1]
                rhsStrings = rhsUnion.split('|')
                for rhsString in rhsStrings:
                    rhs0 = [cp(c) for c in rhsString.split()]
                    # optionally leave out any starred categories
                    alternatives = [[]]
                    for e in rhs0:
                        new = []
                        for a in alternatives:
                            new.append(a+[e])
                            if e[1] == '*': new.append(a)
                        alternatives = new
                    for rhs in alternatives:
                        if rhs == []:
                            if not(lhs in empties):
                                empties.append(lhs)
                        else:
                            for i in range(len(rhs)):
                                key = rhs[i][0] # * and + not needed in key
                                val = (rhs[:i],rhs[i+1:],lhs)
                                if key in gdict.keys():
                                    if not(val in gdict[key]):
                                        gdict[key].append(val)
                                else:
                                    gdict[key] = [val]
                                if rhs[i][1]:
                                    # if rhs[i] is * or +, optionally keep in pre and post cats
                                    vals = [
                                        (rhs[:i],rhs[i:],lhs),
                                        (rhs[:i-1],rhs[i+1:],lhs),
                                        (rhs[:i-1],rhs[i:],lhs)
                                    ]
                                    gdict[key].extend(vals)
                if not(lhs in gdict.keys()):
                    gdict[lhs] = []
            else:
                raise RuntimeError('rules2gdict: %r has length %d' % (ruleParts, len(ruleParts)))
    return empties, gdict

def cp(c): # encode category as pair that separates * or +, if present
    if c[-1] in ['+','*']: return (c[:-1],c[-1])
    else: return (c,'')

def showMatrix(m):
    for row in m: print(row)

def pptree(n, t):
    """ pretty print t indented n spaces. E.g.
          pptree(0, ['TP', ['DP', ['John']], ['VP', ['V',['praises']], ['DP', ['Mary']]]])
        NB: In this list-based tree notation, non-initial list elements are subtrees,
             and so must be lists.
    """
    if isinstance(t, list) and len(t)>0:
        print('%s %s' % (n*' ', str(t[0]))) # print root
        for subtree in t[1:]:  # then subtrees indented by 4
            pptree(n+4, subtree)
    else:
        raise RuntimeError('pptree error')

if __name__ == '__main__':
    """ examples """
    #g,s = g2, ['a','a','b']  # ambiguous
    #g,s = g1, ['dogs']
    #g,s = g1, ['dogs','bark']
    #g,s = g1, ['alert','dogs','bark'],
    #g,s = g1, ['alert','excited','dogs','bark'],
    #g,s = g1, ['alert','excited','courageous','dogs','bark'],
    #g,s = g1, ['dogs','bark','dogs','bark','dogs','bark','and','dogs','bark'],
    #g,s = g1, ['dogs','bark','bark','bark','and','bark'],
    #g,s = g1, ['dogs','that','cats','bother']
    #g,s = g1, ['dogs','cats','bother']
    #g,s = g1, ['dogs','that','cats','bother','bark']
    #g,s = g1, ['dogs','cats','bother','bark']
    #g,s = g1, ['Los', 'Angeles', 'dogs','bark']
    #g,s = g1, ['Los', 'Angeles', 'dogs', 'that', 'cats','bother','bark']
    #g,s = g1, ['dogs', 'that', 'dogs', 'dog', 'dog', 'dogs']
    #g,s = g1, ['dogs', 'dogs', 'dog', 'dog', 'dogs']
    #g,s = g1, ['mice', 'that', 'cats', 'that', 'dogs', 'bother', 'chase', 'eat', 'cheese', 'that', 'rats', 'refuse']
    #g,s = g1, ['mice', 'cats', 'dogs', 'bother', 'chase', 'eat', 'cheese', 'rats', 'refuse']
    g,s = g1, ['cats', 'that', 'meow', 'that', 'eat', 'mice', 'and', 'that', 'chase', 'rats', 'bother', 'dogs']  # ambiguous
    #g,s = g1, ['buffalo', 'buffalo', 'buffalo', 'buffalo', 'buffalo']  # ambiguous!
    #g,s = g1, ['police', 'police', 'police', 'police', 'police']  # ambiguous!
    #g,s = g1, ['char', 'char', 'char', 'char', 'char']   # ambiguous!
    #g,s = g1, ['buffalo', 'that', 'buffalo', 'that', 'buffalo', 'buffalo', 'buffalo', 'buffalo', 'buffalo', 'that', 'buffalo', 'buffalo']
    #g,s = g1, ['buffalo', 'buffalo', 'buffalo', 'buffalo', 'buffalo', 'buffalo', 'buffalo', 'buffalo', 'buffalo']    # ambiguous!
    m = accepts(g, s)
