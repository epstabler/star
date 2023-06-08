# example6.py
# This is used by example6 in ../ot.py

example6input1 = '(& (P ) (Q ) (R ))' # ternary branching &
example6input2 = '(& (P ) (Q ))' # binary branching &
example6input3 = '(& (& (P ) (Q )) (& (P ) (Q )))' # embedded binary branching &
example6input4 = '(& (& (P ) (Q )))' # unary branching &
example6input5 = '(& (& (P ) (Q ) (R )))' # more complex unary branching &

# rule 1 is an identity on unary
# rule 2 eliminates unary branch
# rule 3 is an identity on binary
# rule 4 is an identity on ternary
# rule 6 moves TV0 to adjoin to TV1
# note the these rules are recursive -- 1 state --
#   so, for example, example6input3 is accepted
example6gen = """
(& (TVO )) -> (q (& (TVO )))
(& (TVO )) -> (q (TVO ))
(& (TVO ) (TV1)) -> (q (& (TVO ) (TV1)))
(& (TV0 ) (TV1 ) (TV2 )) -> (q (& (TV0 ) (TV1 ) (TV2 )))
(& (TV0 ) (TV1 ) (TV2 )) -> (q (& (& (TV0 ) (TV1 )) (TV2 )))
(P ) -> (q (P ))
(Q ) -> (q (Q ))
(R ) -> (q (R ))
"""

# non-binary branching has positive weight
# note the these rules are recursive -- 1 state --
#   so, for example, example6input3 is accepted
binaryBranching = """
(& (TVO )) -> (q (& (TVO ))) x
(& (TVO )) -> (q (TVO ))
(& (TVO ) (TV1)) -> (q (& (TVO ) (TV1)))
(& (TV0 ) (TV1 ) (TV2 )) -> (q (& (TV0 ) (TV1 ) (TV2 ))) x
(& (TV0 ) (TV1 ) (TV2 )) -> (q (& (& (TV0 ) (TV1 )) (TV2 )))
(P ) -> (q (P ))
(Q ) -> (q (Q ))
(R ) -> (q (R ))
"""

# non-identity steps have positive weight
# again, note the these rules are recursive -- 1 state.
faithful = """
(& (TVO )) -> (q (& (TVO )))
(& (TVO )) -> (q (TVO )) x
(& (TVO ) (TV1)) -> (q (& (TVO ) (TV1)))
(& (TV0 ) (TV1 ) (TV2 )) -> (q (& (TV0 ) (TV1 ) (TV2 )))
(& (TV0 ) (TV1 ) (TV2 )) -> (q (& (& (TV0 ) (TV1 )) (TV2 ))) x
(P ) -> (q (P ))
(Q ) -> (q (Q ))
(R ) -> (q (R ))
"""
