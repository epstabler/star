:- [latex_tree,wish_tree].

go1 :- T='S'/[
	     'NP'/['(you)'/[]],
	     'VP'/[
		 'V'/[take/[]],
		 'NP'/[me/[]],
		 'PP'/[
		     'P'/[to/[]],
		     'NP'/[
			 'Name'/[
			 'twenty second street'/[]]]]]],
       wish_tree(T),
       latex_tree(T).

go2 :- T='S'/[
	     'NP'/['(you)'/[]],
	     'VP'/[
		 'V'/[take/[]],
		 'NP'/[me/[]],
		 'PP'/[
		     'P'/[to/[]],
		     'NP'/[
			 'NP'/['NUM'/[twenty/[]],
			       'NP'/['N'/[second/[]]]],
			 'N'/['street'/[]]]]]],
       wish_tree(T),
       latex_tree(T).

go3 :- T='S'/[
	     'NP'/['(you)'/[]],
	     'VP'/[
		 'V'/[take/[]],
		 'NP'/[me/[]],
		 'NP'/[
			 'NUM'/[two/[]],
			 'NUM'/[twenty/[]]]],
	     'NP'/[
		 'Name'/[
		     'second street'/[]]]],
       wish_tree(T),
       latex_tree(T).

go4 :- T='S'/[
	     'NP'/['(you)'/[]],
	     'VP'/[
		 'V'/[take/[]],
		 'NP'/[me/[]]],
	     'ADV'/[too/[]],
	     'NP'/[
		 'Name'/[
		     'twenty second street'/[]]]],
       wish_tree(T),
       latex_tree(T).
