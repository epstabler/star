# propCalc.py

# NB: tree variables = 'TV0', 'TV1', 'TV2',...
#     label variables = 'LV0', 'LV1', 'LV2',...

propCalc = """
(P ) -> (q0 )
(Q ) -> (q1 )
(not (q0 )) -> (q1 )
(not (q1 )) -> (q0 )
(& (q1 ) (q1 )) -> (q1 )
(& (LV0 ) (LV1 )) -> (q0 ) if LV0 == 'q0' or LV1 == 'q0'
(v (q0 ) (q0 )) -> (q0 ) if True
(v (LV0 ) (LV1 )) -> (q1 ) if LV0 == 'q1' or LV1 == 'q1'
"""
