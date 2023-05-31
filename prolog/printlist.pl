%% printlist.pl

:- module(printlist,[printlist/1]).

printlist(S) :- numbervars(S, 0, _), printlist(S, 0), write('.'), nl, fail ; true.

printlist(S, Column) :-
    (  S=[Child|Children] ->
       tab(Column), 
       write('['),
       printlists([Child|Children], Column)
    ;  tab(Column), 
       write(S)
    ).

dotted([]).
dotted([A,B|L]) :- !, write(A), write('.'), dotted([B|L]).
dotted([A]) :- !, write(A).

spaced([]).
spaced([A,B|L]) :- !, write(A), write(' '), spaced([B|L]).
spaced([A]) :- !, write(A).

printlists([],_) :-  write(' ]').
printlists([List|Lists], Column) :-
	NextColumn is Column+4,
	nl, printlist(List, NextColumn),
	printrest_lists(Lists, NextColumn).

printrest_lists([], _) :- write(' ]').
printrest_lists([List|Lists], Column) :-
	write(','),
	nl, printlist(List, Column),
	printrest_lists(Lists, Column).
