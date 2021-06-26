nat(z).
nat(s(N)) :- nat(N).
nat_list([]).
nat_list([N|X]) :- nat(N), nat_list(X).
