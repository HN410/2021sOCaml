open Game

type aimove
val piece_count         : int64 -> int
(*ある石の盤とみなして駒の数を数える *)
val get_evaluation_value: board -> int -> int ->  int 
 (*盤面を受け取り，その盤面でのある手番から見た
 　評価値を返す
   入力: board, turn, moves*)
val isPass              : aimove -> bool 
(* aimoveがPassか *)
val isMv                : aimove -> bool 
val isGiveup            : aimove -> bool 
val getMv               : aimove -> int * int 

