open Game

type aimove
val piece_count         : int64 -> int
(*ある石の盤とみなして駒の数を数える *)
val get_evaluation_value: board64 -> int -> int ->  int 
 (*盤面を受け取り，その盤面でのある手番から見た
 　評価値を返す
   入力: board, turn, moves*)
val is_pass              : aimove -> bool 
(* aimoveがPassか *)
val is_mv                : aimove -> bool 
val is_giveup            : aimove -> bool 
val get_mv               : aimove -> int * int 

val get_ai_move           : int array array -> int -> int -> aimove
(* 元の形式のboard, color, timeを受け取ってaimoveを返す*)
(* timeは残り時間ms *)