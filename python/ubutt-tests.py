from ubutt import *
from nltk import Tree

import sys
sys.path.insert(1, 'umbutts')
from propCalcUMBUTT import *
from propCalc import *
from propCalcId import *
from propCalcIdUMBUTT import *
from propCalcStar import *
from propCalcIdStar import *
from example16 import *
from otExample1 import *

def example0():
    #TreeIn = U('Q', [])
    #TreeIn = U('&', [U('P', []), U('Q', [])] )
    #TreeIn = U('&', [U('P', []), U('P', [])] )
    print('\nGiven this tree:\n')
    print(TreeIn)
    print('\nWe calculate its identity transducer:\n')
    rules, stateTree, _, _ = uId(TreeIn)
    finals = [stateTree._root]
    fst = Ubutt(rules,finals)
    print('The identity transducer with final states {%s}:\n' % ', '.join(finals))
    fst.prettyRules()
    print('\nIs fst deterministic?', fst.isDeterministic())
    print('\nAttempting to transduce this syntactic structure:\n')
    print(TreeIn)
    output = fst.dTransduce(TreeIn)
    print('\nResult:\n',output)
    if output._root in finals:
        print('Success!')
    else:
        print('Failed, root not in',finals)

def example1():
    Tree1 = U('CP',
           [U('C', []),
            U('TMP',
               [U('TM',
                   [U('V', [ U('is',[])]),
                    U('TM', [])]),
                U('VP',
                   [U('V', []),
                    U('AP',
                       [U('A', [ U('cuma',[]) ]),
                        U('predP',
                           [U('DP', [ U('e',[]) ]),
                            U("pred'",
                               [U('pred', []),
                                U('&P',
                                   [U('PredP',
                                       [U('Pred', [ U('na',[]) ]),
                                        U('DP', [ U('earrach',[]) ])]),
                                    U('PredP',
                                       [U('Pred', [ U('na',[]) ]),
                                        U('DP', [ U('shamhradh',[]) ])]),
                                    U('PredP',
                                       [U('Pred', [ U('na',[]) ]),
                                        U('DP', [ U('fhomar',[]) ])]),
                                    U("&'",
                                       [U('&', [ U('no',[]) ]),
                                        U('PredP',
                                           [U('Pred', [ U('na',[]) ]),
                                            U('DP', [ U('gheimhreadh',[]) ])])])])])])])])])])

    print('\nUnranked Tree1 has a coordinate structure with 4 coordinates:\n')
    print(Tree1)

    rules, stateTree, _, _ = uId(Tree1)
    finals = [stateTree._root]
    fst = Ubutt(rules,finals)
    print('\nSo the identity transducer for Tree1 with finals {%s}:\n' % ', '.join(finals))
    fst.prettyRules()

    print('\nIs fst deterministic?', fst.isDeterministic())

    print('\nAttempting to transduce Tree1 with uID(Tree1):\n')
    output = fst.dTransduce(Tree1)
    print('\nResult:\n',output)
    if output._root in finals:
        print('Success!')
    else:
        print('Failed, root not in',finals)

    print('\noutput of uId(Tree1) == Tree1?', Tree1 == output._children[0], '\n')

    print('#### Now consider a different tree.\n\nUnranked Tree2 has 1 less coordinate:\n')
    Tree2 = U('CP',
           [U('C', []),
            U('TMP',
               [U('TM',
                   [U('V', [ U('is',[])]),
                    U('TM', [])]),
                U('VP',
                   [U('V', []),
                    U('AP',
                       [U('A', [ U('cuma',[]) ]),
                        U('predP',
                           [U('DP', [ U('e',[]) ]),
                            U("pred'",
                               [U('pred', []),
                                U('&P',
                                   [U('PredP',
                                       [U('Pred', [ U('na',[]) ]),
                                        U('DP', [ U('earrach',[]) ])]),
                                    U('PredP',
                                       [U('Pred', [ U('na',[]) ]),
                                        U('DP', [ U('fhomar',[]) ])]),
                                    U("&'",
                                       [U('&', [ U('no',[]) ]),
                                        U('PredP',
                                           [U('Pred', [ U('na',[]) ]),
                                            U('DP', [ U('gheimhreadh',[]) ])])])])])])])])])])
    print(Tree2)

    print('\nAttempting to transduce Tree2 with uID(Tree1):\n')
    output = fst.dTransduce(Tree2)
    print('\nResult:\n',output)
    if output._root in finals:
        print('Success!')
    else:
        print('Failed, root not in',finals)
    print('\nAs expected, the coordinate structure cannot be transduced:')
    print('       ** The root of the output tree is not a state. **')
    print('   The states in this result show how far transducer uId(Tree2)')
    print('   was able to get.\n')

def example2():
    print("This is Example 16 from Engelfriet&al'09:\n")
    gStr = example16
    gRules = lines2rules(gStr)
    g = Ubutt(gRules, ['q'])
    fst.prettyRules()
    print('\nExample 16 deterministic? %s\n' % str(fst.isDeterministic()))
    print('Let Tree1 =')
    Tree1 = U('sigma',[U('sigma',[U('sigma',[U('alpha',[])])])])
    print(Tree1)
    print("\nThe Engelfriet&al'09 Example 16 accepts Tree1, with the result:\n")
    Tree2 = fst.dTransduce(Tree1)
    print(Tree2)
    print('\nConverting Example 16 to 1-normal form...\n')
    fst1 = fst.oneNormalize(fst.states())
    print('1-normal form:')
    fst1.prettyRules()
    print('\nAssuming all states final, 1-normal(Example 16) deterministic? %s\n' % str(fst1.isDeterministic()))
    print("Unlike the duxmbutt Example 16, 1-normal(Example 16) is a dumbutt.")
    print("1-normal(Example 16) transduces Tree1 to:\n")
    Tree3 = fst1.dTransduce(Tree1)
    print(Tree3)
    print('\noutput of Example16(Tree1) == 1-normal(Example16)(Tree1)?', Tree2 == Tree3, '\n')

def example3():
    g,s,cat = (propCalcIdUMBUTT, U('&', [U('Q', []), U('not',[U('P', [])])]) , '(q1 )')
    print("Consider this grammar:\n")
    gU = lines2rules(g)
    fst = Ubutt(gU, ['q0','q1'])
    fst.prettyRules()
    print('\nIs that transducer deterministic? %s\n' % str(fst.isDeterministic()))
    Tree1 = s
    print('Let Tree1 = %s' % str(Tree1))
    print("\nThe Engelfriet&al'09 Example 16 accepts Tree1, with the result:\n")
    Tree2 = fst.dTransduce(Tree1)
    print(Tree2)
    print('\nConverting to 1-normal form...')
    fst1 = fst.oneNormalize(fst.states())
    print('\n1-normal form:\n')
    fst1.prettyRules()
    print('\n1-normal(transducer) deterministic? %s\n' % str(fst1.isDeterministic()))
    print("1n(transducer) transduces Tree1 to:\n")
    Tree3 = fst1.dTransduce(Tree1)
    print(Tree3)
    print('\noutput of transducer(Tree1) == 1-normal(transducer)(Tree1)?', Tree2 == Tree3, '\n')

def exampleMG1():
    g = """
(predP (DP (q0 (TV0 ) ) ) (PredP (q1 (TV1 )) (q2 (TV2 ) ) ) ) -> (q3 (phi (phi (TV0 ) (TV1 ) ) (TV2 ) ) )
"""
    print("An example from the Stabler&Yu'23 SCiL paper: Figure 5, left.")
    print("The rule is shown in more detail in the SCiL presentation: slides 7 and 17.")
    print("And is shown in full detail Stabler&Yu'23 MG+1: slide 5\n")
    gU = lines2rules(g)
    fst = Ubutt(gU, ['q3'])
    fst.prettyRules()
    print('\nConverting to 1-normal form...')
    fst1 = fst.oneNormalize(fst.states())
    print()
    fst1.prettyRules()

def exampleR1():
    print('Example of R1: M o N\n')
    print('Let transducer M =')

    Mstring = "(sigma (q1 (TV0 ) (TV1 )) (q2 )) -> (q (TV1 ))"
    Mrules = lines2rules(Mstring)
    M = Ubutt(Mrules, ['q'])
    M.prettyRules()

    print('\nAnd let N have these rules, with states of various arities,')
    print('  to illustrate how composition affects the various cases.')
    print('N =')
    Nstring = """
(sigma (qD )) -> (qA )
(sigma (qD )) -> (qB (TV0 ))
(sigma (qD )) -> (qC (TV0 ))
(sigma (qD )) -> (qD (TV0 ) (TV1 ))
"""
    Nrules = lines2rules(Nstring)
    N = Ubutt(Nrules, ['qB','qC'])
    N.prettyRules()
    if not(VERBOSE): print('\n(Set VERBOSE = True to see results in unofficial form.)')
    print()
    officialForm = M.o(N)
    #officialForm.prettyRules()
    if officialForm.isOneNormal():
        print('This composition is 1-normal:')
        officialForm.prettyRules()

def exampleR3():
    print('Example of R3, where transducer M =')
    Mstring = """
(q1 (TV0 ) (TV1 )) -> (q (gamma (TV1 )))
"""
    Mrules = lines2rules(Mstring)
    M = Ubutt(Mrules, ['q'])
    M.prettyRules()

    print('\nAnd let N have these rules:')
    Nstring = """
(gamma (qA (TV0 ) (TV1 ))) -> (qB (TV1 ))
"""
    Nrules = lines2rules(Nstring)
    N = Ubutt(Nrules, ['qB'])
    N.prettyRules()
    if not(VERBOSE): print('\n(Set VERBOSE = True to see results in unofficial form.)')
    print()
    officialForm = M.o(N)
    #officialForm.prettyRules()
    if officialForm.isOneNormal():
        print('This composition is 1-normal:')
        officialForm.prettyRules()

def exampleR2():
    print('Example of (R1 and then) R2.\n')

    print('Let transducer M =')
    Mstring = """
(sigma (q1 (TV0 ) (TV1 )) (q2 )) -> (q (TV0 ) (TV1 ))
"""
    Mrules = lines2rules(Mstring)
    M = Ubutt(Mrules, ['q'])
    M.prettyRules()

    print('Let transducer N =')
    Nstring = """
(qA (TV0 ) (TV1 )) -> (qC (TV0 ) (TV1 ))
(qA ) -> (qB (delta))
"""
    Nrules = lines2rules(Nstring)
    N = Ubutt(Nrules, ['qB'])
    N.prettyRules()

    if not(VERBOSE): print('\n(Set VERBOSE = True to see results in unofficial form.)')
    print()
    officialForm = M.o(N)
    #officialForm.prettyRules()
    if officialForm.isOneNormal():
        print('This composition is 1-normal:')
        officialForm.prettyRules()

def example6():
    print("In their Example 20 (p24), Engelfriet&al'09 compose 2 xmbutts.\n")
    print("Their first xmbutt M is our oneNormal(example4):\n")
    M = """
(alpha ) -> (q1 )
(q2 ) -> (q (alpha ))
(sigma (q1 )) -> (q2 )
(q3 (TV0 ) (TV1 )) -> (q (gamma (TV0 ) (TV1 )))
(q4 (TV0 )) -> (q3 (TV0 ) (alpha ))
(sigma (q (TV0 ))) -> (q4 (TV0 ))
"""
    gM = Ubutt(lines2rules(M), ['q'])
    gM.prettyRules()
    print('\nAssuming all states final, is M deterministic? %s\n' % str(gM.isDeterministic()))
    print("\nTransducer M maps this tree, Tree1,\n")
    Tree1 = U('sigma',[U('sigma',[U('alpha',[])])])
    print(Tree1)
    print("\nto this tree, Tree2:\n")
    Tree2 = U.fromstring('(gamma (alpha ) (alpha ))')
    print(Tree2)
    print("\nAnd M also maps this tree, Tree11,\n")
    Tree11 = U('sigma',[U('sigma',[U('sigma',[U('alpha',[])])])])
    print(Tree11)
    print("\nto this tree, Tree12:\n")
    Tree12 = U.fromstring('(gamma (gamma (alpha ) (alpha )) (alpha ))')
    print(Tree12)
    print("\nTheir second xmbutt N:\n")
    N = """
(alpha ) -> (qh (alpha ))
(qh (TV0 )) -> (qh (delta (TV0 )))
(gamma (qh (TV0 )) (qh (TV ))) -> (qg (gamma (TV0 ) (TV1 )))
"""
    gN = Ubutt(lines2rules(N), ['qg'])
    gN.prettyRules()

    print("\nNotice that N is non-finitary, since that second rule")
    print("    can add any number of delta's.")
    print("And notice that N accepts M(Tree1)=Tree2 but not M(Tree11)=Tree12.")
    print("N maps Tree2 to the infinite set:")
    print("   {gamma(delta^i(alpha),delta^j(alpha)) | i,j natural numbers}.")
    print("The last rules has 2 symbols, so...")
    print("    N is 1-normal?", gN.isOneNormal())
    print("\nEngelfriet&al define M o N for 1-normal M, but do not require that for N.")

    print("\nSo we calculate the composition (M o N):\n")

    officialForm = gM.o(gN)
    print()
    if officialForm.isOneNormal():
        print('This composition is 1-normal:')
        officialForm.prettyRules()

    print("Setting the stage for iterated compositions, though, 1-normal form is better.")
    print('\nConverting N to 1-normal form...\n')
    N1 = gN.oneNormalize(gN.states())
    print('1-normal form:')
    N1.prettyRules()
    print(N1.isOneNormal())

    print("\nSo we calculate the composition (M o 1n(N)):\n")

    officialForm = gM.o(N1)
    print()
    if officialForm.isOneNormal():
        print('This composition is 1-normal:')
        officialForm.prettyRules()

if __name__ == '__main__':
    #example0()   # id-transduce, deterministic, transduce
    #example1()   # id-transduce, deterministic, transduce
    #example2()   # convert to 1-n form (EL&M Example 16), deterministic, transduce
    #example3()   # convert to 1-n form (Prop Calc example), deterministic, transduce
    #exampleMG1() # convert to 1-n form (Stabler&Yu example)
    #exampleR1()  # compose
    #exampleR2()  # compose
    #exampleR3()  # compose
    example6()  # compose
