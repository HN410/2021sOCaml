% a, bを〇，×とその手番とする.空白はx
% 盤面はリストで表現

%自分の手番を入力，相手の手番を出力
isDifferentTurn(a, b).
isDifferentTurn(b, a).

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

% BでPが何らかの手を指した後の局面がC
isNextBoard(P, B, C) :- inIsNextBoard(P, B, C, 0).

inIsNextBoard(P, [], [], 1).
inIsNextBoard(P, [B1|B2], [C1|C2], 0) :- changeOnePiece(P, B1, C1), inIsNextBoard(P, B2, C2, 1).
inIsNextBoard(P, [B1|B2], [C1|C2], X) :- getThreeElement(B1, A1, A2, A3), getThreeElement(C1, A1, A2, A3), inIsNextBoard(P, B2, C2, X).

changeOnePiece(P, B, C) :- getThreeElement(B, x, B2, B3), getThreeElement(C, P, B2, B3).
changeOnePiece(P, B, C) :- getThreeElement(B, B1, x, B3), getThreeElement(C, B1, P, B3).
changeOnePiece(P, B, C) :- getThreeElement(B, B1, B2, x), getThreeElement(C, B1, B2, P).

%Pの手番で必勝か
win(P, B):- hasThreeLine(P, B).
win(P, B):- isDifferentTurn(P, Q), \+ hasThreeLine(Q, B), isNextBoard(P, B, C), lose(Q, C).

%現在Pの相手の手番で，Pが必敗か
lose(P, B) :- isDifferentTurn(P, Q), hasThreeLine(Q, B).
lose(P, B) :- hasEmpty(B), isNightmare(P, B).

% PがBの盤面でどう指そうが，相手の必勝になる not(\or not) = \and
isNightmare(P, B) :- \+inIsNightmare(P, B).
inIsNightmare(P, B) :- isDifferentTurn(P, Q), isNextBoard(P, B, C), \+ win(Q, C).

%まだ空きマスがあるか
hasEmpty([B|X]) :- inHasEmpty(B).
hasEmpty([B|X]) :- hasEmpty(X).

inHasEmpty([x|X]).
inHasEmpty([B|X]) :- inHasEmpty(X).



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
[[a, x, x],
 [b, x, x],
 [x, x, x]
]
[[x, x, x],
 [b, a, x],
 [x, x, x]
]
[[x, a, x],
 [b, x, x],
 [x, x, x]
]
[[x, x, x],
 [x, x, x],
 [x, x, x]
]
*/