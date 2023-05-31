/* r.pl

  For tcl/tk tree display, uncomment the 3 lines that say
    "uncomment for tree display"
*/
:- [printlist].    % for printing lists with indents, for readability

% examples -- uncomment any you want
:- [exEng].
:- [exCopy].
:- [exIrish].

% optional, for tree display:
:- use_module('tree_display/wish_tree'). % uncomment for tree display
:- use_module('tree_display/latex_tree'). % uncomment for tree display

verbose :- fail.   % for comments, comment this line and uncomment next line
%verbose.

% Elements of numerations are lexical items Phon-Neg-Pos, or trees [A,B].

% Features of lexical items and trees are
%      NegFeatures-PosFeatures-EmbeddedLabels,
% where each element of EmbeddedLabels has the structure PosFeatures-Tree

% r(X0, A) iff r reduces X0 to completed constituent A
r([A], A) :- l(A, []-[_]-[]).   % success!
r(X0, X) :-
    select(A, X0, X1), l(A, [F0|AN]-AP-AC0), % select a potential head
    ( nonvar(F0), F0=p(F) -> P=true ; F0=F, P=fail), % ck if feature has plus
    ( select([F|BP]-B, AC0, AC) -> X2 = X1, BC = []  % select for internal merge 
    ; select(B, X1, X2), l(B, []-[F|BP]-BC), AC=AC0  % select for external merge
    ),
    m(B, [F|AN]-AP-AC, []-[F|BP]-BC, ABF),  % compute new label
    '&'(P, F, X2, X3, Cs),                  % select 0 or more additional Cs
    mrg(A, B, Cs, ABCs),                    % merge
    ( verbose ->
      write('merged':ABCs), nl, 
      write('with features':ABF), nl, nl,
      write('numeration:'),nl, printlist([ABCs|X3]), nl, nl
    ; true
    ),
    r([ABCs|X3], X).                        % recurse


'&'(_,_,X,X,[]).
'&'(true,F,X0,X,[C|Cs]) :-  % Cs compatible with coordination
    select(C,X0,X1),
    l(C,[]-[F]-[]),
    '&'(true,F,X1,X,Cs).

% mrg(A, B, [A,B]) iff [A,B] is result of merging A,B.
%   We do not check to make sure A\=B since that is already guaranteed by features.
%   The head is always detectable as the unique element with a negative feature.
%   So it is convenient but not necessary to order the head first, as we do here.
% (This is not named "merge", since that name is already used by a Prolog predicate)
mrg(A, B, Cs, [A,B|Cs]).

% l(A,F) iff A has features F of the form: NegF-PosF-EmbeddedLabels
l(_-A-B, A-B-[]).
l([A,B|_], ABF) :- l(A, AF), l(B, BF), m(B, AF, BF, ABF).

% m(B, AF, BF, ABF) iff AF and BF mate to yield 
%   new label ABF = Neg-Pos-[LabelsOfEmbeddedConstituentsWithPosFeaturesOnly]
%   NB: ABF is computed from A-features AF and
%                            B-Features BF, and also
%                            B itself (in case e3)
m(B, [F0|AN]-AP-AC, BF, AN-AP-ABC) :-
    ( nonvar(F0), F0=p(F) -> true ; F0=F),
    ( select([F|BP]-B0, AC, AC1) -> B0=B,                   % move-over-merge
      ( BP = [] -> AC1 = ABC                                % i1
      ; BP = [G|BP1], smc([[G|BP1]-B0|AC1], [], ABC, [])    % i2
      )
    ; BF = []-[F]-BC, smc(AC, BC, ABC, [])                  % e1, e2
    ; BF = []-[F,G|BP]-BC, smc([[G|BP]-B|AC], BC, ABC, [])  % e3
    ).

% smc(C, D, CD, Fs) iff CD is append of list C,D of embedded labels C,D,
%   where no two elements put same F into Fs
smc([], D, D, _).
smc([[F|C]-A|L], M, [[F|C]-A|N], Fs) :-
    ( \+member(F, Fs) -> smc(L, M, N, [F|Fs])
    ; write('smc violation, feature: '), write(F), nl, fail
    ).

%:- exEn1.
%:- exEn1Display.   % uncomment for tree display
%:- exIrish2.
:- exIrish2Display.
