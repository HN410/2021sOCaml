ancestor(X, Y) :- ancestor(Z, Y), parent(X, Z).
ancestor(X, Y) :- parent(X, Y).