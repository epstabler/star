% exIrish.pl
:- [printlist].
% optional, for graphical display:
:- [nestedList2tree]. 

:- write('exIrish0, exIrish00, exIrish000, exIrish1, exIrish2, use a simple grammar to illustrate parses\n\c
   of unbounded flat coordination in Irish Gaelic, inspired by Bennett, Elfner and McCloskey 2016.\n\c
   Any number of coordinates is OK!\n\c
   As usual, type ; to display alternative parses...\n\n').

exIrish0 :-
    N0 = [
	[]-[tm]-[c],
	[]-[v]-[tm],
	[is]-[a]-[v],
	[cuma]-[lpred]-[a],
	[shamhradh]-[]-[d],
	[e]-[]-[d],
	[]-[pred,d]-[lpred],
	[na]-[d]-[pred]
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

exIrish00 :-
    N0 = [
	[no]-[X,p(X)]-[X],
	[shamhradh]-[]-[d],
	[gheimhreadh]-[]-[d]
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

exIrish000 :-
    N0 = [
	[no]-[X,p(X)]-[X],
	[shamhradh]-[]-[d],
	[gheimhreadh]-[]-[d],
	[fhomar]-[]-[d]
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

exIrish1 :-
    N0 = [
	[]-[tm]-[c],
	[]-[v]-[tm],
	[is]-[a]-[v],
	[cuma]-[lpred]-[a],
	[]-[pred,d]-[lpred],
	[na]-[d]-[pred],
	[shamhradh]-[]-[d],
	[na]-[d]-[pred],
	[gheimhreadh]-[]-[d],
	[no]-[X,p(X)]-[X],
	[e]-[]-[d]
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

exIrish2 :-
    N0 = [
	[]-[tm]-[c],
	[]-[v]-[tm],
	[is]-[a]-[v],
	[cuma]-[lpred]-[a],
	[]-[pred,d]-[lpred],
	[na]-[d]-[pred],
	[shamhradh]-[]-[d],
	[na]-[d]-[pred],
	[fhomar]-[]-[d],
	[na]-[d]-[pred],
	[gheimhreadh]-[]-[d],
	[no]-[X,p(X)]-[X],
	[e]-[]-[d]
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

exIrish2Display :-
    N0 = [
	[]-[tm]-[c],
	[]-[v]-[tm],
	[is]-[a]-[v],
	[cuma]-[lpred]-[a],
	[]-[pred,d]-[lpred],
	[na]-[d]-[pred],
	[shamhradh]-[]-[d],
	[na]-[d]-[pred],
	[fhomar]-[]-[d],
	[na]-[d]-[pred],
	[gheimhreadh]-[]-[d],
	[no]-[X,p(X)]-[X],
	[e]-[]-[d]
	 ],
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
