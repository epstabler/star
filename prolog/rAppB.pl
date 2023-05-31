%% rAppB.pl -- this extends rule R with adjunction, as sketched in Appendix B.
% The only change is to redefine &, and load some relevant examples

:- [r].

% examples
:- [exAppB].

:- write('*Warning: Redefining & so it allows both coordination and adjunction*\n').

'&'(_,_,X,X,[]).
% for plus rule, which coordinators have
'&'(true,F,X0,X,[C|Cs]) :-  % Cs compatible with coordination
    select(C,X0,X1),
    l(C,[]-[F]-[]),
    '&'(true,F,X1,X,Cs).
% for adjunction
'&'(fail,F,X0,X,[C|Cs]) :-  % Cs compatible with adjunction
    adjunct(F,G),
    select(C,X0,X1),
    l(C,[]-[G]-[]),
    '&'(fail,F,X1,X,Cs).

adjunct(n,c). % just a placeholder for better treatment, but enough for examples
adjunct(n,a).
