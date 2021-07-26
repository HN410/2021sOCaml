open Int64
open Game

let pc_mask0 = 0x5555555555555555L
let pc_mask1 = 0x3333333333333333L
let pc_mask2 = 0x0f0f0f0f0f0f0f0fL
let pc_mask3 = 0x7fL

let final_border = 50

let eval_v0_factor = -5


let piece_count board = 
  let a0 = Int64.sub board (Int64.logand 
  (Int64.shift_right_logical board 1) pc_mask0) in 
  let a1 = Int64.add (Int64.logand a0 pc_mask1) 
                (Int64.logand (Int64.shift_right_logical a0 2) 
                    (pc_mask1)) in 
  let a2 = Int64.logand 
    (Int64.add a1 (Int64.shift_right_logical a1 4)) pc_mask2 in 
  let a3 = Int64.add a2 (Int64.shift_right_logical a2 8 ) in 
  let a4 = Int64.add a3 (Int64.shift_right_logical a3 16) in 
  let a5 = Int64.add a4 (Int64.shift_right_logical a4 32) in 
  Int64.to_int (Int64.logand a5 pc_mask3)

let get_evaluation_value (board: board) (turn: int ) (moves: int) = 
  (* 評価値 *)
  let (myboard, otherboard) = 
    if turn = black_turn then get_board_tuple board 
    else let (a, b) = get_board_tuple board in (b, a) in
  let v0 = if turn > final_border then 0 
    (* 自分の着手可能数- 相手の着手可能数 *)
           else (piece_count (get_legal_move board turn)) - 
                (piece_count (get_legal_move board (get_another_turn turn ))) in
  let v1 = (piece_count my_board) - (piece_count other_board) in 
    (* 自分の駒数 - 相手の駒数 *)
  
  let ans = v0 * eval_v0_factor in ans



