open Game
open Int64 

(* val board64_to_board     : board64 -> board 
オリジナルのボードから既定のボードへ *)

val board_to_board64        : int array array  -> board64 
val color_to_mycolor        : int -> int 
val mv_to_mymove            : int -> int -> int64 
val mymove_to_mv            : int64 -> int * int 