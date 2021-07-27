open Int64
open Game 

type board = int array array

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
  