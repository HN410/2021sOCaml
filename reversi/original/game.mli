type board64

val first_board : board64
val board_size  : int
val corumn_n    : int 
val black_mark  : string
val white_mark  : string
val none_mark   : string 
val black_turn  : int 
val white_turn  : int 

val get_another_turn    : int -> int 
(* 相手のターンに変換 *)
val make_board          : int64 -> int64 -> board64
(* boardをint64２つから作成 *)
val get_board_tuple     : board64 -> int64 * int64
(* make_boardの逆 *)
val print_board         : board64 -> unit
(* boardを表示 *)
val print_only_board    : int64 -> int -> unit 
(* turnの手番の駒だけ表示 *)
val get_legal_move      : board64 -> int -> int64
(* turnにとって合法なマス一覧となるint64を返す *)
val flip                : board64 -> int64 -> int -> board64
  (*boardにnewoneを置いたときの盤面を返す*)
