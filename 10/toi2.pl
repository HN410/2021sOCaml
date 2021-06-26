add(z, Y, Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

mult(z, Y, z).
mult(s(z), Y, Y).
mult(s(X), Y, Z) :- mult(X, Y, W), add(W, Y, Z).

