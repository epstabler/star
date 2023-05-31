# propCalcId.py identity transducer and evaluator

# NB: tree variables = 'TV0', 'TV1', 'TV2',...
#     label variables = 'LV0', 'LV1', 'LV2',...

propCalcId = """
    (P ) -> (q0 (P ))
    (Q ) -> (q1 (Q ))
    (not (q0 (TV0 ))) -> (q1 (not (TV0 )))
    (not (q1 (TV0 ))) -> (q0 (not (TV0 )))
    (& (q1 (TV0 )) (q1 (TV1 ))) -> (q1 (& (TV0 ) (TV1 )))
    (& (LV0 (TV0 )) (LV1 (TV1 ))) -> (q0 (& (TV0 ) (TV1 ))) if LV0 == 'q0' or LV1 == 'q0'
    (v (q0 (TV0 )) (q0 (TV1 ))) -> (q0 (v (TV0 ) (TV1 )))
    (v (LV0 (TV0 )) (LV1 (TV1 ))) -> (q1 (v (TV0 ) (TV1 ))) if LV0 == 'q1' or LV1 == 'q1'
    """
