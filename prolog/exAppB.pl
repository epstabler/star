% exAppB.pl
% Irish example from Appendix B of Stabler & Yu 2023

:- [printlist].

:- write('For the suggestion of Appendix B:
   files exIrish3, exIrish4 use a simple grammar to illustrate parses\n\c
   of adjoined relative clauses and adjectives in Irish Irishelic.\n\c
   Any number of adjuncts is OK!\n\c
   As usual, type ; to display alternative parses...\n\n').

exIrish3 :-
    N0 = [
	[na]-[n]-[d],
	[sigoai]-[]-[n],
	[alainn]-[]-[a],
	[alainn]-[]-[a],
	[alainn]-[]-[a]
	 ],
    write('Attempting to find derivations for this numeration:'), nl,
    nl, printlist(N0), nl,
    Time1 is cputime,
    r(N0, N),
    Time2 is cputime,
    Time is Time2-Time1,
    write('success!'), nl,
    nl, printlist(N), nl,
    nl, write(Time), write(' seconds'), nl.

exIrish4 :-
    N0 = [
	[]-[tm,op]-[c],
	[]-[v]-[tm],
	[]-[]-[d,op],
	[ghoid]-[d,d]-[v],
	[na]-[n]-[d],
	[sigoai]-[]-[n],
	[an]-[n]-[d],
	[ghirseach]-[]-[n]
	 ],
    write('Attempting to find derivations for this numeration:'), nl,
    nl, printlist(N0), nl,
    Time1 is cputime,
    r(N0, N),
    Time2 is cputime,
    Time is Time2-Time1,
    write('success!'), nl,
    nl, printlist(N), nl,
    nl, write(Time), write(' seconds'), nl.
