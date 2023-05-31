"""        file: mgxx.ml
   Grammar for the copy language {XX| X\in{a,b}*}
   This grammar has lots of local ambiguity, and does lots of movement
   (more than in any human language, I think)
   so it gives the parser a good workout.
"""
mgxx = [([],[],['T','r','l']), 
        ([],['T','r','l'],['T']),
        (['a'],['T','r'],['A','r']), 
        (['b'],['T','r'],['B','r']), 
        (['a'],['A','l'],['T','l']),
        (['b'],['B','l'],['T','l']) 
        ] 
