from utree import *

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

    # here, the bindings returned by match is a list of pairs of trees
    bindings = U('a', [U('TV0',[]),U('TV1',[])]).match(U('a', [U('b',[]),
                                                               U('PredP',
                                                                 [U('Pred',
                                                                    [U('na',[])]),
                                                                  U('DP',
                                                                    [U('shamhradh',[])])])]), [])

    # now apply those bindings to this simple tree with the 2 variables in it
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
