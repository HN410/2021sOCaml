open Int64

type board = Board of int64 * int64;;
(* 順に黒，白 *)

let board_size = 64
let corumn_n = 8
let black_mark = "○"
let white_mark = "●"
let none_mark =  "-"
let black_turn = 0
let white_turn = 1

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

let print_only_board board turn = 
  let b = (if(turn == black_turn) then Board(board, 0L)
            else Board(0L, board)) in 
            print_board b

let legal_shift_n = 6
let horizontal_legal_mask = 0x7e7e7e7e7e7e7e7eL
let vertical_legal_mask = 0x00ffffffffffff00L
let diagonal_legal_mask = 0x007e7e7e7e7e7e00L

let rec get_legal_move_left_in mine mine_mask num shift_n = 
  if(num == 0) then mine 
  else 
    let new_mine = Int64.logor mine (Int64.logand mine_mask 
            (Int64.shift_left mine shift_n)) in 
    get_legal_move_left_in new_mine mine_mask (num-1) shift_n;;
let rec get_legal_move_right_in mine mine_mask num shift_n = 
  if(num == 0) then mine 
  else 
    let new_mine = Int64.logor mine (Int64.logand mine_mask 
            (Int64.shift_right_logical mine shift_n)) in 
    get_legal_move_right_in new_mine mine_mask (num-1) shift_n;;


let get_legal_move_left other_board my_board = 
(* 左方向だけの合法マス my_board側の手番の人がおける場所*)
  let blank = Int64.lognot (Int64.logor my_board other_board) in 
  let mine_left_mask = Int64.logand my_board horizontal_legal_mask in 
  let left_mine = Int64.logand (Int64.shift_left other_board 1)
   mine_left_mask in 
   let new_left_mine = get_legal_move_left_in left_mine mine_left_mask 
     (legal_shift_n -1) 1 in 
      Int64.logand (Int64.shift_left new_left_mine 1) blank 

let get_legal_move_right other_board my_board = 
(* 右方向だけの合法マス my_board側の手番の人がおける場所*)
  let blank = Int64.lognot (Int64.logor my_board other_board) in 
  let mine_right_mask = Int64.logand my_board horizontal_legal_mask in 
  let right_mine = Int64.logand (Int64.shift_right_logical other_board 1)
   mine_right_mask in 
   let new_mine = get_legal_move_right_in right_mine mine_right_mask 
     (legal_shift_n -1) 1 in 
      Int64.logand (Int64.shift_right_logical new_mine 1) blank 

    
let get_legal_move_up other_board my_board = 
(* 上方向だけの合法マス my_board側の手番の人がおける場所*)
  let blank = Int64.lognot (Int64.logor my_board other_board) in 
  let mine_mask = Int64.logand my_board vertical_legal_mask in 
  let mine = Int64.logand (Int64.shift_left other_board corumn_n)
   mine_mask in 
   let new_mine = get_legal_move_left_in mine mine_mask 
     (legal_shift_n -1) corumn_n in 
      Int64.logand (Int64.shift_left new_mine corumn_n) blank 

let get_legal_move_down other_board my_board = 
(* 下方向だけの合法マス my_board側の手番の人がおける場所*)
  let blank = Int64.lognot (Int64.logor my_board other_board) in 
  let mine_mask = Int64.logand my_board vertical_legal_mask in 
  let mine = Int64.logand (Int64.shift_right_logical other_board corumn_n)
   mine_mask in 
   let new_mine = get_legal_move_right_in mine mine_mask 
     (legal_shift_n -1) corumn_n in 
      Int64.logand (Int64.shift_right_logical new_mine corumn_n) blank 

let get_legal_move_du other_board my_board plus = 
(* 斜め下方向だけの合法マス用 my_board側の手番の人がおける場所*)
  let blank = Int64.lognot (Int64.logor my_board other_board) in 
  let mine_mask = Int64.logand my_board diagonal_legal_mask in 
  let mine = Int64.logand (Int64.shift_right_logical other_board (corumn_n+plus))
   mine_mask in 
   let new_mine = get_legal_move_right_in mine mine_mask 
     (legal_shift_n -1) (corumn_n+plus) in 
      Int64.logand (Int64.shift_right_logical new_mine (corumn_n+plus)) blank 

let get_legal_move_dd other_board my_board plus = 
(* 斜め上方向だけの合法マス用 my_board側の手番の人がおける場所*)
  let blank = Int64.lognot (Int64.logor my_board other_board) in 
  let mine_mask = Int64.logand my_board diagonal_legal_mask in 
  let mine = Int64.logand (Int64.shift_left other_board (corumn_n+plus))
   mine_mask in 
   let new_mine = get_legal_move_left_in mine mine_mask 
     (legal_shift_n -1) (corumn_n+plus) in 
      Int64.logand (Int64.shift_left new_mine (corumn_n+plus)) blank 

let get_legal_move_d other_board my_board = 
  let lu = get_legal_move_du other_board my_board 1 in 
  let ru = get_legal_move_du other_board my_board (-1) in 
  let ld = get_legal_move_dd other_board my_board (-1) in
  let rd = get_legal_move_dd other_board my_board 1 in 
  Int64.logor (Int64.logor lu ru) (Int64.logor ld rd)
   
  

let get_legal_move (board: board) turn = 
  (* turnにとって合法なマス一覧となるint64を返す *)
  let Board(other_board, my_board) = 
    (if(turn = 1) then board else 
    let Board(a, b) = board in Board(b, a)) in 
  let left = get_legal_move_left other_board my_board in 
  let right = get_legal_move_right other_board my_board in 
  let up = get_legal_move_up other_board my_board in 
  let down = get_legal_move_down other_board my_board in 
  let d = get_legal_move_d other_board my_board in 
  Int64.logor (Int64.logor left right) (Int64.logor (Int64.logor up down) d)

let test_white = 0x10787820301000L
let test_black = 0x2002045c0c0000L

  

