type board

val first_board : board 
val board_size  : int
val corumn_n    : int 
val black_mark  : string
val white_mark  : string
val none_mark   : string 
val black_turn  : int 
val white_turn  : int 


val make_board          : int64 -> int64 -> board 
(* boardをint64２つから作成 *)
val get_board_tuple     : board -> int64 * int64
(* make_boardの逆 *)
val print_board         : board -> unit
(* boardを表示 *)
val print_only_board    : int64 -> int -> unit 
(* turnの手番の駒だけ表示 *)
val get_legal_move      : board -> int -> int64
(* turnにとって合法なマス一覧となるint64を返す *)
val flip                : board -> int64 -> int -> board 
  (*boardにnewoneを置いたときの盤面を返す*)
