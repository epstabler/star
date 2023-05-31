:- module(pptree,[nestedList2tree/2]).
:- use_module(library(apply),[maplist/3]).

nestedList2tree([H|T], '.'/L) :- !, maplist(nestedList2tree,[H|T],L).
nestedList2tree(E, E/[]).
