% a, bを〇，×とその手番とする
% 盤面はリストで表現

% 3*3行列であるか
is33Matrix(B) :- getThreeElement(B, A1, A2, A3), getThreeElement(A1, A11, A12, A13), getThreeElement(A2, A21, A22, A23), getThreeElement(A3, A31, A32, A33).

%3要素ゲット (3*3用)
getThreeElement([A|[B|[C|[]]]], A, B, C).

% Pの石が3つ並んでいるか
hasThreeLine(P, B) :- hasThreeLineH(P, B).
hasThreeLine(P, B) :- hasThreeLineV(P, B).
hasThreeLine(P, B) :- hasThreeLineD(P, B).

% Pの石が縦に3つ並んでいるか
hasThreeLineV(P, B) :- getThreeElement(B, E1, E2, E3), inHasThreeLineV(P, E1, E2, E3).

inHasThreeLineV(P, [P|E1], [P|E2], [P|E3]).
inHasThreeLineV(P, [A1|E1], [A2|E2], [A3|E3]) :- inHasThreeLineV(P, E1, E2, E3).

% Pの石が横に3つ並んでいるか
hasThreeLineH(P, [A|X]) :- getThreeElement(A, P, P, P).
hasThreeLineH(P, [A|X]) :- hasThreeLineH(P, X).


% Pの石が斜めに並んでいるか
hasThreeLineD(P, B) :- getThreeElement(B, E1, E2, E3), hasThreeLineD1(P, E1, E2, E3).
hasThreeLineD(P, B) :- getThreeElement(B, E1, E2, E3), hasThreeLineD2(P, E1, E2, E3).
hasThreeLineD1(P, E1, E2, E3) :- getThreeElement(E1, P, E12, E13), getThreeElement(E2, E21, P, E23), getThreeElement(E3, E31, E32, P).
hasThreeLineD2(P, E1, E2, E3) :- getThreeElement(E1, E11, E12, P), getThreeElement(E2, E21, P, E23), getThreeElement(E3, P, E32, E33).



win(P, B).

/*
[[a, b, a],
 [b, b, b], 
 [a, a, b]]
[[b, a, b],
 [a, a, b],
 [b, a, a]
]
[[b, a, b],
 [a, b, b],
 [b, a, a]
]
*/