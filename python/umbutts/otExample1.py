# ot1.py
# This is an example for ../ot.py

ot1input1 = '(& (P ) (Q ) (R ))' # ternary branching &
ot1input2 = '(& (P ) (Q ))' # binary branching &
ot1input3 = '(& (& (P ) (Q )) (& (P ) (Q )))' # embedded binary branching &
ot1input4 = '(& (& (P ) (Q )))' # unary branching &
ot1input5 = '(& (& (P ) (Q ) (R )))' # more complex unary branching &

# rule 1 is an identity on unary
# rule 2 eliminates unary branch
# rule 3 is an identity on binary
# rule 4 is an identity on ternary
# rule 5 moves TV0 to adjoin to TV1
# note the these rules are recursive -- 1 state --
#   so, for example, ot1input3 is accepted
ot1gen = """
(& (TVO )) -> (q (& (TVO )))
(& (TVO )) -> (q (TVO ))
(& (TVO ) (TV1)) -> (q (& (TVO ) (TV1)))
(& (TV0 ) (TV1 ) (TV2 )) -> (q (& (TV0 ) (TV1 ) (TV2 )))
(& (TV0 ) (TV1 ) (TV2 )) -> (q (& (& (TV0 ) (TV1 )) (TV2 )))
(P ) -> (q (P ))
(Q ) -> (q (Q ))
(R ) -> (q (R ))
"""

# To mark non-binary branching, it suffices to see
# only the output tree, so all rules are identity rules,
# but non-binary branching gets a violation mark
binaryBranching = """
(& (TVO )) -> (q (& (TVO ))) x
(& (TVO ) (TV1)) -> (q (& (TVO ) (TV1)))
(& (TV0 ) (TV1 ) (TV2 )) -> (q (& (TV0 ) (TV1 ) (TV2 ))) x
(P ) -> (q (P ))
(Q ) -> (q (Q ))
(R ) -> (q (R ))
"""

# Match and other IO-faithfulness constraints
# require access to the input and output.
# When it suffices to track the correspondence
# in Gen itself, we can have a tier or weight that marks
# non-conforming rules of gen, in a form that
# is preserved by later compositions.
# See e.g. Riggle'04 and references cited there
faithfulGen = """
(& (TVO )) -> (q (& (TVO )))
(& (TVO )) -> (q (TVO )) x
(& (TVO ) (TV1)) -> (q (& (TVO ) (TV1)))
(& (TV0 ) (TV1 ) (TV2 )) -> (q (& (TV0 ) (TV1 ) (TV2 )))
(& (TV0 ) (TV1 ) (TV2 )) -> (q (& (& (TV0 ) (TV1 )) (TV2 ))) x
(P ) -> (q (P ))
(Q ) -> (q (Q ))
(R ) -> (q (R ))
"""
