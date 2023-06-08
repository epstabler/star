"""  mgstarckyp.py

     CKY-like parser for *-MGs (with phrasal merge and move -- no head movement)

     On the command line
       > python mgstarckyp.py
     runs an example -- see last lines of this file

     As described in Stabler and Yu 2023, 
       *-MGs allow features f*, f+ in antecedents,
       indicating (0 or more f), (1 or more f) respectively.
       Some simple examples appear in the example grammar mgIrish.

     NB: f* is weakly equivalent to 0 or more f, but different derived structure: all sisters.
         And f+ is weakly equivalent to ff*, but has different derived structure: all sisters.
         Intuitively, antecedent f+ checks 1 or more consequent f features simultaneously.

     Function initializeMatrix(..) assumes lex items are either empty or 1 string.

     The parser constructs a table of items, representing all possible parses.
     items = (i,j):(neg,pos,simple,movers,sources) where sources is a list of:
        merge sources: ('*',selector_sources,selectee_sources)
        merge++ sources: ('+',selector_sources,selectee_sources)
        move sources: ('o',sources)
        lexical sources: (ph,neg_features,pos_features)

     NB: If parse infinitely ambiguous, tree collection can be nonterminating.

     A very simple user interface ui(grammar, input_string, final_category)
     is provided, which asks user to select a parse (if there is one)
     and returns that parse as a nested list
               with lexical items at the leaves,
               with internal merges unary, and
               with external merges n-ary (n > 1).
     Given that derivation d, various other formats are then easy to compute.
     To represent the derivation with internal merge as copy: d2copy(d)
     For U tree surface representation, for transduction: d2surface(d)
     For NLTK tree representation, for graphical display: d2nltk(d)
     And more ... see below.

     If you install nltk, graphical display with nltk.tree can be enabled 
     -- uncomment the 3 lines below that say "uncomment for graphical display"
"""
import sys
from time import time       # optional -- for tests
from utree import *         # optional -- for transductions
from nltk.tree import Tree # optional -- uncomment for graphical display
import re # optional -- uncomment for graphical display

# examples from mgs subdirectory
sys.path.insert(1, 'mgs')
from mg0 import * # a grammar, for tests
from mg1 import * # a grammar, for tests
from mgxx import * # a grammar, for tests
from mgTamil import * # a grammar, for tests
from mgBaker import * # a grammar, for tests
from mgCollins import * # a grammar, for tests
from mgIrish import * # a grammar, for tests

VERBOSE = False

def parse(g,input,cat):
    lexicon={}
    for (words,neg0,pos0) in g: # put g into dict
        neg,pos = ([fp(f) for f in neg0],[fp(f) for f in pos0])
        if tuple(words) in lexicon.keys():
            lexicon[tuple(words)].append((neg,pos))
        else:
            lexicon[tuple(words)]=[(neg,pos)]
    positions = len(input)+1
    a = [] # agenda
    m = [ [ [] for i in range(positions) ] for j in range(positions) ] # matrix
    o = [] # for other items (movement items), optional record for trace only
    closeMatrix(a,m,o,lexicon,input,len(input))
    if VERBOSE: printMatrix(m)
    return successfulMatrix(m,len(input),cat)

def fp(f): # encode feature as pair that separates * or +, if present
    if f[-1] in ['+','*']: return (f[:-1],f[-1])
    else: return (f,'')

def initializeMatrix(a, m, o, lexicon, input, i): # initialize column i
    if VERBOSE:
        sys.stdout.write('--- scanning (%r,%r) ' % (i,i))
        if i<len(input):
            sys.stdout.write('and then (%r,%r): %s\n' % (i,i+1,input[i]))
        else:
            sys.stdout.write('\n')
    if () in lexicon.keys():
        for neg,pos in lexicon[()]:
            addToMatrix(a, m, o, i, i, neg, pos, True, {}, ([],neg,pos))
    if i<len(input) and (input[i],) in lexicon.keys():
        for (neg,pos) in lexicon[(input[i],)]:
            addToMatrix(a, m, o, i, i+1, neg, pos, True, {}, ([input[i]],neg,pos))
    elif i<len(input):
        raise RuntimeError('parse error: '+input[i]+' not in lexicon')

def addToCell(a, cell, ij, i, j, neg, pos, simple, movers, src):
    newItem = True
    if ij:   # ij flag tells us whether to include i,j -- True for 'other' list, for movers
        item=(i, j, neg, pos, simple, movers, [src])
    else:
        item=(neg, pos, simple, movers, [src])
    for e in cell:
        if e[:-1]==item[:-1]:
            newItem = False
            if not(src in e[-1]):
                e[-1].append(src)
            return # break and return if item already there, otherwise:
    #if VERBOSE: printItemSrc(i, j, neg, pos, simple, movers, [src]) # if you want to see sources
    if VERBOSE: printItem(i, j, neg, pos, simple, movers) # if you don't want to see sources
    a.append((i, j, neg, pos, simple, movers, [src]))  # add to agenda
    cell.append(item)
    if neg and neg[0][1] == '*': # now also remove first neg feature if starred
        if ij:   # ij flag tells us whether to include i,j -- True for 'other' list o
            item=(i, j, neg[1:], pos, simple, movers, [src])
        else:
            item=(neg[1:], pos, simple, movers, [src])
        for e in cell:
            if e[:-1]==item[:-1]:
                newItem = False
                if not(src in e[-1]):
                    e[-1].append(src)
                return # break and return if item already there, otherwise:
        #if VERBOSE: printItemSrc(i, j, neg, pos, simple, movers, [src]) # if you want to see sources
        if VERBOSE: printItem(i, j, neg[1:], pos, simple, movers) # if you don't want to see sources
        a.append((i, j, neg[1:], pos, simple, movers, [src]))  # add to agenda
        cell.append(item)
        

def addToMatrix(a, m, o, i, j, neg, pos, simple, movers, src):
    if len(neg)==0 and len(pos)==0:
        raise RuntimeError('addToMatrix error')
    if neg and neg[0][0] in movers.keys(): # move over merge: apply move now
        #addToCell(a, o, True, i, j, neg, pos, simple, movers, src) # optional, for 'others' list o
        new0 = movers.copy()
        (mi,mj,mpos) = new0[neg[0][0]]
        del new0[neg[0][0]]
        if len(mpos) == 0:
            if mj == i: # IM1 - move to landing position
                addToMatrix(a, m, o, mi, j, neg[1:], pos, False, new0, ('o',[src]))
        elif len(mpos)>0: # IM2 - move through intermediate position
            (ok,new1)=smc_merge(new0, {}, [(mpos[0][0],(mi,mj,mpos[1:]))])
            if ok:
                addToMatrix(a, m, o, i, j, neg[1:], pos, False, new1, ('o',[src]))
        else:
            raise RuntimeError('addToMatrix move error')
    else: # if new, add to matrix cell m[i][j] and to agenda a
        addToCell(a, m[i][j], False, i, j, neg, pos, simple, movers, src)

def successfulMatrix(m, n, cat):
    (ok,trees) = (False,[])
    for (neg,pos,simple,movers,src) in m[0][n]:
        if neg==[] and len(pos)==1 and pos[0][0]==cat and list(movers.items())==[]:
            ok = True
            trees.extend(derivations(src))
    return(ok,trees)

def closeMatrix(a, m, o, lexicon, input, length):
    for i in range(length+1): # insert word[i] and close chart
        initializeMatrix(a, m, o, lexicon, input, i)
        while a != []:
            (si,sj,sneg,spos,ssimple,smovers,ssrc) = a.pop() # pop item from agenda
            if VERBOSE:
                sys.stdout.write('popped: ')
                printItem(si,sj,sneg,spos,ssimple,smovers)
            if sneg: # selector
                if ssimple: # EM1: selector is lexical, with right edge sj
                    for j in range(sj,i+1): # selected element must be to right
                        for (neg,pos,simple,movers,src) in m[sj][j]:
                            if neg==[] and len(pos)==1 and pos[0][0]==sneg[0][0]:
                                if sneg[0][1]: # EM1++
                                    addToMatrix(a, m, o, si, j, sneg[1:], spos, False, movers, ('+',ssrc,src))
                                    if si != j: # avoid empty cat -> loop
                                        addToMatrix(a, m, o, si, j, sneg, spos, False, movers, ('+',ssrc,src))
                                else:
                                    addToMatrix(a, m, o, si, j, sneg[1:], spos, False, movers, ('.',ssrc,src))
                else: # EM2 selector is derived
                    for j in range(si+1): # look for non-moving cat to left...
                        for (neg,pos,simple,movers,src) in m[j][si]:
                            if neg==[] and pos[0][0]==sneg[0][0] and len(pos)==1: # try merge2
                                (ok,new)=smc_merge(smovers,movers,[])
                                if ok:
                                    if sneg[0][1]: # EM2++
                                        addToMatrix(a, m, o, j, sj, sneg[1:], spos, False, new, ('+',ssrc,src))
                                        if j != sj: # avoid empty cat -> loop
                                            addToMatrix(a, m, o, j, sj, sneg, spos, False, new, ('+',ssrc,src))
                                    else:
                                        addToMatrix(a, m, o, j, sj, sneg[1:], spos, False, new, ('.',ssrc,src))
                # EM3: look for moving cat to left or right, adjacent or not
                for rgt in range(si+1): # to left
                    for lft in range(rgt+1):
                        for (neg,pos,simple,movers,src) in m[lft][rgt]:
                            if neg==[] and len(pos)>1 and pos[0][0]==sneg[0][0]:
                                (ok,new)=smc_merge(smovers,movers,[(pos[1],(lft,rgt,pos[2:]))])
                                if ok:
                                    addToMatrix(a, m, o, si, sj, sneg[1:], spos, False, new, ('.',ssrc,src))
                for lft in range(sj,i+1): # to right
                    for rgt in range(lft,i+1):
                        for (neg,pos,simple,movers,src) in m[lft][rgt]:
                            if neg==[] and len(pos)>1 and pos[0][0]==sneg[0][0]:
                                (ok,new)=smc_merge(smovers,movers,[(pos[1],(lft,rgt,pos[2:]))])
                                if ok:
                                    addToMatrix(a, m, o, si, sj, sneg[1:], spos, False, new, ('.',ssrc,src))
            if sneg==[]: # item popped from agenda is a category 
                if len(spos)>1: # a moving category, so EM3
                    for rgt in range(si+1): # to left
                        for lft in range(rgt+1):
                            for (neg,pos,simple,movers,src) in m[lft][rgt]:
                                if neg and neg[0][0]==spos[0][0]:
                                    (ok,new)=smc_merge(smovers,movers,[(spos[1],(si,sj,spos[2:]))])
                                    if ok:
                                        addToMatrix(a, m, o, lft, rgt, neg[1:], pos, False, new, ('.',src,ssrc))
                    for lft in range(sj,i+1): # to right
                        for rgt in range(lft,i+1):
                            for (neg,pos,simple,movers,src) in m[lft][rgt]:
                                if neg and neg[0][0]==spos[0][0]:
                                    (ok,new)=smc_merge(smovers,movers,[(spos[1],(si,sj,spos[2:]))])
                                    if ok:
                                        addToMatrix(a, m, o, lft, rgt, neg[1:], pos, False, new, ('.',src,ssrc))
                else: # len(spos)==1 
                    for j in range(si+1): # EM1: lexical selector to left
                        for (neg,pos,simple,movers,src) in m[j][si]:
                            if simple==True and neg and neg[0][0]==spos[0][0]:
                                if neg[0][1]:  # EM1++
                                    addToMatrix(a, m, o, j, sj, neg[1:], pos, False, smovers, ('+',src,ssrc))
                                    if j != sj: # avoid empty cat -> loop
                                        addToMatrix(a, m, o, j, sj, neg, pos, False, smovers, ('+',src,ssrc))
                                else:
                                    addToMatrix(a, m, o, j, sj, neg[1:], pos, False, smovers, ('.',src,ssrc))
                    for j in range(sj,i+1): # EM2 derived selector to right
                        for (neg,pos,simple,movers,src) in m[sj][i]:
                            if simple==False and neg and neg[0][0]==spos[0][0]:
                                (ok,new)=smc_merge(smovers,movers,[])
                                if ok:
                                    if neg[0][1]:  # EM2++
                                        addToMatrix(a, m, o, si, j, neg[1:], pos, False, new, ('+',src,ssrc))
                                        if si != j: # avoid empty cat -> loop
                                            addToMatrix(a, m, o, si, j, neg, pos, False, new, ('+',src,ssrc))
                                    else:
                                        addToMatrix(a, m, o, si, j, neg[1:], pos, False, new, ('.',src,ssrc))
def smc_merge(m1, m2, more):
    new = m1.copy()
    for (f,val) in list(m2.items())+more:
        if not (f[0] in new.keys()):
            new[f[0]]=val
        else:
            return(False,{})
    return(True, new)

def derivations(src):
    """ collect MG-style derivations as nested lists, 
            with lexical items at the leaves,
            with internal merges unary, and
            with external merges n-ary (n > 1).

        Rather than using U or Tree datastructures, this function
        represents derivations as nested lists, in the minimalist style.
        Note that we could have used sets instead of lists, since
        order does not matter, and heads identifiable by label.
        It is more convenent, though redundant, to use lists with the
        head always the first element.

        Conversion to a tree data structure, for later processing,
        is easy.
    """
    result = []
    if isinstance(src,list):
        for s in src:
            for t in derivations(s):
                result.append(t)
    else: #tuple
        if src[0]=='.':
            for s1 in src[1]:
                for s2 in src[2]:
                    for t1 in derivations(s1):
                        for t2 in derivations(s2):
                            result.append([t1,t2])
        # + is nonbinary case
        elif src[0]=='+':
            for s1 in src[1]:
                for s2 in src[2]:
                    for t1 in derivations(s1):
                        for t2 in derivations(s2):
                            if s1[0]=='+':
                                result.append([t1+[t2]])
                            else:
                                result.append([t1,t2])
        elif src[0]=='o':
            for s in src[1]:
                for t in derivations(s):
                    result.append([t])
        else:
            result.append(src)
    return result

# ////////////////////////////////////////////////////////////
# user interface
# ////////////////////////////////////////////////////////////
def ui(g, inputStr, cat):
    start_time = time()
    (ok,derivations) = parse(g, inputStr, cat)
    elapsed_time = time()-start_time
    if ok:
        for t in derivations:
            #print(repr(t))
            #print(t)
            print(prettyList(0,t))
            print('%.4f seconds\n' % elapsed_time)
            ans = input('\nok? (no or ; to search another, return for pretty derivation) ')
            if not(ans) or ans[0].lower()=='y':
                return t
            else:
                print('no (more) parses')
                print('%.4f seconds\n' % elapsed_time)
    else:
        print('no parse found')
        print('%.4f seconds\n' % elapsed_time)

def d2btfy(d): # convert derivation to one with pretty lexical items
    if isinstance(d,list):
        return [d2btfy(d[i]) for i in range(len(d))]
    else: return(btfyLexItem(d))

def d2u(d): # convert derivation to U tree, for transduction
    if isinstance(d,list):
        return U('.',[d2u(d[i]) for i in range(len(d))])
    else:
        #return(U(d,[]))
        return(U(btfyLexItem(d),[]))

def d2nltk(d): # convert derivation to nltk tree, for graphical display
    if isinstance(d,list):
        return Tree('.',[d2nltk(d[i]) for i in range(len(d))])
    else: # NLTK tree drawing gets confused by spaces inside labels
        lex = re.sub(' ','_',btfyLexItem(d))
        return(Tree(lex,[]))

# ////////////////////////////////////////////////////////////
# string formatting, printing
# ////////////////////////////////////////////////////////////
def btfyFeat(neg,pos): # features as pretty string
    if neg: return( ' -o '.join([' '.join([f+p for (f,p) in neg]),
                                 ' '.join([f+p for (f,p) in pos])]))
    else: return( ' '.join([f+p for (f,p) in pos]))

def btfyItem(i,j,neg,pos,simple,movers): # item as pretty string
    if simple: s='::'
    else: s=':'
    item = '(%r,%r) %s %s {%s}' % \
        (i,j,s,btfyFeat(neg,pos),
         ' '.join([prettyM(f,i,j,pos) for (f,(i,j,pos)) in movers.items()]))
    return(item)

def prettyM(f,i,j,pos): # mover as a pretty string
    return( '(%d,%d):%s' % (i,j,btfyFeat([], [f]+pos)) )

def btfyLexItem(li): # lexical item as pretty string
    (words,neg,pos) = li
    return( '::'.join([' '.join(words), btfyFeat(neg,pos)]) )

def prettyList(n, t): # derivation t, a nested list, as pretty string
    if isinstance(t, list) and len(t)>0:
        if isinstance(t[0], list):
            s = '%s[\n%s' % ((n*' '),prettyList(n+2, t[0]))
            for element in t[1:]: s += ',\n%s' % prettyList(n+2, element)
        else:
            s = '%s[%s' % (n*' ', str(t[0]))
            for element in t[1:]: s += ',\n%s' % prettyList(n+2, element)
        return(s + ']')
    else: return('%s%s' % (n*' ', str(t)))

def printMG(g): 
    for (words,neg0,pos0) in g:
        neg,pos = ([fp(f) for f in neg0],[fp(f) for f in pos0])
        print(btfyLexItem((words,neg,pos)))

def printItem(i,j,neg,pos,simple,movers):
    print(btfyItem(i,j,neg,pos,simple,movers))

def printItemSrc(i,j,neg,pos,simple,movers,src):
    sys.stdout.write('%s --- %r\n' % (btfyItem(i,j,neg,pos,simple,movers), src))

def printMatrix(m):
    for i in range(len(m)):
        for j in range(len(m)):
            items = m[i][j]
            for item in items: # i,j : neg,pos,simple,movers
                printItem(i,j,item[0],item[1],item[2],item[3]) 

# ////////////////////////////////////////////////////////////
# main
# ////////////////////////////////////////////////////////////
if __name__ == '__main__':
    # Uncomment the desired example:
    #g,s,cat = (mg0, ['the','queen'], 'D')
    #g,s,cat = (mg0, ['the','king','prefers','the','beer'], 'C')
    #g,s,cat = (mg0, ['the','queen','knows','which','beer','the','king','prefers'], 'C')
    #g,s,cat = (mg0, ['the','king','knows','which','wine','the','queen','knows','the','king','knows','the','queen','prefers'], 'C')
    #g,s,cat = (mg0, ['which','queen','prefers','the','wine'], 'C')
    #g,s,cat = (mg1, ['Marie','praises','Pierre'], 'C')
    #g,s,cat = (mg1, ['who','Marie','praises'], 'C')
    #g,s,cat = (mg1, ['who','praises','Marie'], 'C')
    #g,s,cat = (mg1, ['which','student','Marie','praises'], 'C')
    #g,s,cat = (mgxx, ['a','a'], 'T')
    #g,s,cat = (mgxx, ['a','b','a','b'], 'T')
    #g,s,cat = (mgxx, ['a','a','a','b','a','a','a','b'], 'T')
    #g,s,cat = (mgTamil, ['Marie','Pierre','praise','-s'], 'C')
    #g,s,cat = (mgBaker, ['Chris','hunger'], 'T')
    #g,s,cat = (mgBaker, ['Chris','hungry'], 'T')
    #g,s,cat = (mgCollins, ['John','seems','to','Mary','to','be','nice'], 'I')
    #g,s,cat = (mgIrish, ['na','shamhradh'], 'Pred')
    #g,s,cat = (mgIrish, ['is','cuma','e','na','shamhradh'], 'C')
    #g,s,cat = (mgIrish, ['is','cuma','e','na','shamhradh','no','na','gheimhreadh'], 'C')
    g,s,cat = (mgIrish, ['is','cuma','e','na','shamhradh','na','fhomhar','no','na','gheimhreadh'], 'C')

    print('\nusing grammar:\n'); printMG(g)
    print('\nparsing as %s: %s\n' % (cat,' '.join(s)))
    r = ui(g,s,cat) # displays actual parser list format
    if r: print('\nin nested list format:'); print(prettyList(0,d2btfy(r))); print()
    if r: print('\nin U tree printed format: (internal node . can have multiple arities)'); print(d2u(r))
    if r: Tree.fromstring(str(d2nltk(r))).draw() #  -- uncomment for graphical display
