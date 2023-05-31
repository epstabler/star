# propCalcIdUMBUTT.py

# This UMBUTT version has
#      no label vars,
#      no conditions, and
#      left side of each rule has at most 1 input symbol

propCalcIdUMBUTT = """
    (P ) -> (q0 (P ))
    (Q ) -> (q1 (Q ))
    (not (q0 (TV0 ))) -> (q1 (not (TV0 )))
    (not (q1 (TV0 ))) -> (q0 (not (TV0 )))
    (& (q0 (TV0)) (q0 (TV1))) -> (q0 (& (TV0 ) (TV1 )))
    (& (q0 (TV0)) (q1 (TV1))) -> (q0 (& (TV0 ) (TV1 )))
    (& (q1 (TV0)) (q0 (TV1))) -> (q0 (& (TV0 ) (TV1 )))
    (& (q1 (TV0)) (q1 (TV1))) -> (q1 (& (TV0 ) (TV1 )))
    (v (q0 (TV0)) (q0 (TV1))) -> (q0 (& (TV0 ) (TV1 )))
    (v (q0 (TV0)) (q1 (TV1))) -> (q1 (& (TV0 ) (TV1 )))
    (v (q1 (TV0)) (q0 (TV1))) -> (q1 (& (TV0 ) (TV1 )))
    (v (q1 (TV0)) (q1 (TV1))) -> (q1 (& (TV0 ) (TV1 )))
"""
