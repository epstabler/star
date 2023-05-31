:- [draw_tree,latex_tree,wish_tree].

go :-
    T=doc/[intro/[preamble/[]],sec1/['1.1'/[],'1.2'/[]],sec2/['2.1'/[],'2.2'/[]]],
    latex_tree(T),
    wish_tree(T).
