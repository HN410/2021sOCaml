open Int64

type board = Board of int64 * int64;;
(* 順に黒，白 *)

let board_size = 64
let corumn_n = 8
let black_mark = "○"
let white_mark = "●"
let none_mark =  "-"
let first_board = Board(0x1008000000L, 0x810000000L)

let rec print_board_in black white now = 
  (if now mod corumn_n == 0 then print_newline ());
  if now = 0 then ()
  else
    let a = Int64.logand Int64.one (Int64.shift_right_logical black (now-1) )in 
    let b = Int64.logand Int64.one (Int64.shift_right_logical white (now-1) )in 
    (if (Int64.compare a Int64.one) == 0 then print_string black_mark
    else if (Int64.compare b Int64.one) == 0 then print_string white_mark
    else print_string none_mark);
    print_board_in black white (now -1)

let print_board (board: board) = 
  (* boardをプリントする *)
  let Board(b, w) = board in 
  print_board_in b w (board_size)

  

