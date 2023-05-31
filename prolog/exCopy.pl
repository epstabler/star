% exCopy.pl
% the copy language {xx| x\in{a,b}*}

:- write('exXX2, exXX4, exXX8 use a simple 6 item grammar to illustrate parses\n\c
     of the copy language {xx| x in {a,b}*}.\n\c
     It is a good test for MG parses -- it uses all the rules and has many movements.\n\c
     Since numeration order does not matter, it is not obvious that this is the\n\c
     copy language until you linearize the derivations with rule K.!\n\c
     The whole grammar is given as the numeration in exXX4.\n\c
     As usual, type ; to display alternative parses...\n\n').

exXX2 :-
    N0 = [[a]-[a,l]-[c,l], [a]-[c,r]-[a,r],
          []-[]-[c,r,l], []-[c,r,l]-[c]],
    write('Attempting to find derivations for this numeration:'), nl,
    nl, printlist(N0), nl,
    Time1 is cputime,
    r(N0, N),
    Time2 is cputime,
    Time is Time2-Time1,
    write('success!'), nl,
    nl, printlist(N), nl,
    nl, write(Time), write(' seconds'), nl.

exXX4 :-
    N0 = [[a]-[a,l]-[c,l], [a]-[c,r]-[a,r],
          [b]-[b,l]-[c,l], [b]-[c,r]-[b,r],
          []-[]-[c,r,l], []-[c,r,l]-[c]],
    write('Attempting to find derivations for this numeration:'), nl,
    nl, printlist(N0), nl,
    Time1 is cputime,
    r(N0, N),
    Time2 is cputime,
    Time is Time2-Time1,
    write('success!'), nl,
    nl, printlist(N), nl,
    nl, write(Time), write(' seconds'), nl.

exXX8 :-
    N0 = [[a]-[a,l]-[c,l], [a]-[c,r]-[a,r],
          [b]-[b,l]-[c,l], [b]-[c,r]-[b,r],
          [a]-[a,l]-[c,l], [a]-[c,r]-[a,r],
          [b]-[b,l]-[c,l], [b]-[c,r]-[b,r],
          []-[]-[c,r,l], []-[c,r,l]-[c]],
    write('Attempting to find derivations for this numeration:'), nl,
    nl, printlist(N0), nl,
    Time1 is cputime,
    r(N0, N),
    Time2 is cputime,
    Time is Time2-Time1,
    write('success!'), nl,
    nl, printlist(N), nl,
    nl, write(Time), write(' seconds'), nl.
