trocatodos([],_,_,[]).
trocatodos([V|R],N,V,LN) :- trocatodos(R,N,V,LR), !, LN = [N|LR].
trocatodos([X|R],N,V,LN) :- trocatodos(R,N,V,LR), !, LN = [X|LR].