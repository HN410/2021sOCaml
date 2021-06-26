inreverse([], Z, Z).
inreverse([A|X], Y, Z) :- inreverse(X, [A|Y], Z).
reverse(X, Y) :- inreverse(X, [], Y).

inconcat([], Z, Z).
inconcat([A|X], Y, Z) :- inconcat(X, [A|Y], Z).
concat([X|[Y|[]]], Z) :- inconcat(W, Y, Z), reverse(X, W).