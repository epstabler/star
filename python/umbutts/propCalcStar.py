# propCalcStar.py - propositional calculus acceptor/evaluator with *-transitions

# The *-transitions allow both disjunctions to have 0 or more constituents.

# Tests of constituent root labels are moved simply into the conditions by
#   binding variable SV0 to the constituent labels.

# As usual, the empty disjunction is false, and
#           the empty conjunction is true.

propCalcStar = """
(P ) -> (q0 )
(Q ) -> (q1 )
(not (q0 )) -> (q1 )
(not (q1 )) -> (q0 )
(& (* )) -> (q1 ) if all([t=='q1' for t in SV0])
(& (* )) -> (q0 ) if any([t=='q0' for t in SV0]) and  all([t[0]=='q' for t in SV0])
(v (* )) -> (q1 ) if any([t=='q1' for t in SV0]) and  all([t[0]=='q' for t in SV0])
(v (* )) -> (q0 ) if all([t=='q0' for t in SV0])
"""
