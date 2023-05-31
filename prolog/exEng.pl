% exEng.pl
:- [printlist].
% optional, for graphical display:
:- [nestedList2tree]. 
    
:- write('exEn1 uses a simple 9 item grammar to illustrate a simplified wh-movement,\n\c
   in which the moved element, which food, occurs in two sets.\n\c
   As usual, type ; to display alternative parses...\n\n').

exEn1 :-
    N0 = [ [which]-[n]-[d,w], [food]-[]-[n], []-[v,w]-[c],
          [the]-[n]-[d], [cat]-[]-[n], [likes]-[d,d]-[v],
          []-[v]-[c], [jo]-[]-[d], [knows]-[c,d]-[v]  ],
    write('Attempting to find derivations for this numeration:'), nl,
    nl, printlist(N0), nl,
    Time1 is cputime,
    r(N0, N),
    Time2 is cputime,
    Time is Time2-Time1,
    write('success!'), nl,
    nl, printlist(N), nl,
    nl, write(Time), write(' seconds'), nl.

exEn1Display :-
    N0 = [ [which]-[n]-[d,w], [food]-[]-[n], []-[v,w]-[c],
          [the]-[n]-[d], [cat]-[]-[n], [likes]-[d,d]-[v],
          []-[v]-[c], [jo]-[]-[d], [knows]-[c,d]-[v]  ],
    write('Attempting to find derivations for this numeration:'), nl,
    nl, printlist(N0), nl,
    Time1 is cputime,
    r(N0, N),
    Time2 is cputime,
    Time is Time2-Time1,
    write('success!'), nl,
    nl, printlist(N), nl,
    nl, write(Time), write(' seconds'), nl,
    nestedList2tree(N,T),
    wish_tree(T).
