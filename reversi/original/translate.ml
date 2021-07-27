open Int64
open Game


type board = int array array
exception  MvTransError

let none     = 0
let white_p    = 1 
let black_p    = 2
let sentinel = 3 

let rec board_to_board64_in_in (board_row: int array) num black white = 
  if(num > corumn_n) then (black, white) 
  else 
    let newb = Int64.shift_left black 1 in  
    let neww = Int64.shift_left white 1 in 
    let nowpiece = Array.get board_row num in 
      let ansb = if(nowpiece = black_p) then (Int64.succ newb)
                else newb in 
      let answ = if(nowpiece = white_p) then (Int64.succ neww)
                else neww in 
      board_to_board64_in_in board_row (num + 1) ansb answ

let rec board_to_board64_in (board: board) num black white= 
  if(num > corumn_n) then make_board black white
  else 
    let board_row = Array.get board num in 
    let (newb, neww) = board_to_board64_in_in board_row 1 black white in 
    board_to_board64_in board (num + 1) newb neww
    
    
let board_to_board64 (board: board ) = 
  board_to_board64_in board 1 Int64.zero Int64.zero
  
let color_to_mycolor color = 2-color 

let mv_to_mymove i j = 
  let shift_i = corumn_n - i in 
  let shift_j = corumn_n - j in 
    Int64.shift_left Int64.one (shift_i*corumn_n + shift_j)

let move_trans_mask = 0xffL

let rec mymove_to_mv_in_in move i j = 
  let move_mask = Int64.logand move Int64.one in
   if((Int64.compare move_mask Int64.zero) = 0) then 
     let shift_move = Int64.shift_right_logical move 1 in 
     mymove_to_mv_in_in shift_move i (j+1)
   else 
     (corumn_n -i, corumn_n - j)
let rec mymove_to_mv_in move mask i = 
  let move_mask = Int64.logand move mask in 
  if((Int64.compare move_mask Int64.zero) = 0) then 
    let shift_move = (Int64.shift_right_logical move corumn_n) in 
    mymove_to_mv_in shift_move mask (i+1)
  else 
    mymove_to_mv_in_in move i 0

let mymove_to_mv (move: int64) = 
  if((Int64.compare move Int64.zero) = 0) then raise MvTransError
  else 
    mymove_to_mv_in move move_trans_mask 0
  
