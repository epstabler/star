# propCalcIdStar.py - propositional calculus transducer/evaluator with *-transitions

# The *-transitions allow disjunctions and conjunctions with 0 or more constituents.

# Tests of constituent root labels are moved simply into the conditions by
#   binding variable SV0 to the constituent labels.

# As usual, the empty disjunction is false, and the empty conjunction is true.

# NB: tree variables = 'TV0', 'TV1', 'TV2',...
#     label variables = 'LV0', 'LV1', 'LV2',...

propCalcIdStar = """
(P ) -> (q0 (P ))
(Q ) -> (q1 (Q ))
(not (q0 (TV0 ))) -> (q1 (not (TV0 )))
(not (q1 (TV0 ))) -> (q0 (not (TV0 )))
(& (* )) -> (q1 (& (ST0 ))) if all([t=='q1' for t in SV0])
(& (* )) -> (q0 (& (ST0 ))) if any([t=='q0' for t in SV0]) and  all([t[0]=='q' for t in SV0])
(v (* )) -> (q1 (v (ST0 ))) if any([t=='q1' for t in SV0]) and  all([t[0]=='q' for t in SV0])
(v (* )) -> (q0 (v (ST0 ))) if all([t=='q0' for t in SV0])
"""
