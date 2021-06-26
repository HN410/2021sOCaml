

isIncident(A, B, [A|[B|[]]]).
/*頂点２つがその辺で結合しているか*/

hasEdge(A, B, [C | X]) :-isIncident(A, B, C).
hasEdge(A, B, [C | X]) :- hasEdge(A, B, X).
/*頂点A, Bがを結ぶ辺があるか*/

isAdjacent2(B, C, E, A, A, C) :- hasEdge(A, B, E).
isAdjacent2( B, C, E, A, B, A) :- hasEdge(C, A, E).
/*端点(B, C)と接続する頂点がAか，
入力　A
    　B     端点
      C     端点
      E     辺リスト
出力　存在する場合はY, Zに新たな端点を返す*/

inHamilton([], [], B, C, E, T).
inHamilton([], Y, B, C, E, 0) :- inHamilton(Y, [], B, C, E, 1).
inHamilton([A|X], Y, B, C, E, T) :- isAdjacent2(B, C, E, A, F, G), inHamilton(X, Y, F, G, E, 0) .
inHamilton([A|X], Y,  B, C, E, T) :- inHamilton(X, [A|Y], B, C, E, T).
/*頂点リスト，探索済み頂点リスト, 端点１，端点２，辺リスト, ループ済フラグ
 端点とつながるような頂点があるか調べる*/


inHamiltonInit([A|X], Y, B, E) :- hasEdge(A, B, E), inHamilton(X, Y, A, B, E, 0).
inHamiltonInit([A|X], Y, B, E) :- hasEdge(B, A, E), inHamilton(X, Y, B, A, E, 0).
inHamiltonInit([A|X], Y, B, E) :- inHamiltonInit(X, [A|Y], B, E).

hamilton([], E).
hamilton([A], E).
hamilton([X|Z], E) :- inHamiltonInit(Z, [], X, E).