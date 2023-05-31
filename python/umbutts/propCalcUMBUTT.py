# propCalcUMBUTT.py

# This UMBUTT version has no vars and the left side of each
#   rule is not extended with extra structure --
#    i.e. subtrees (if any) are states

propCalcUMBUTT = """
(P ) -> (q0 )
(Q ) -> (q1 )
(not (q0 )) -> (q1 )
(not (q1 )) -> (q0 )
(& (q0 ) (q0 )) -> (q0 )
(& (q0 ) (q1 )) -> (q0 )
(& (q1 ) (q0 )) -> (q0 )
(& (q1 ) (q1 )) -> (q1 )
(v (q0 ) (q0 )) -> (q0 )
(v (q0 ) (q1 )) -> (q1 )
(v (q1 ) (q0 )) -> (q1 )
(v (q1 ) (q1 )) -> (q1 )
"""
