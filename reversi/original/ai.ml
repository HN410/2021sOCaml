open Int64
open Game
open Translate

type aimove = Mv of int * int | Pass | GiveUp
exception AimoveTransException

let pc_mask0 = 0x5555555555555555L
let pc_mask1 = 0x3333333333333333L
let pc_mask2 = 0x0f0f0f0f0f0f0f0fL
let pc_mask3 = 0x7fL

let bp_mask0 = 0x8100000000000081L
let bp_mask1 = 0x4281000000008142L
let bp_mask2 = 0x0042000000004200L
let bp_mask3 = 0x2400810000810024L
let bp_mask4 = 0x1800248181240018L
let bp_mask5 = 0x003c424242423c00L
let bp_mask6 = 0x0000182424180000L

let bp_fac0 = 100
let bp_fac1 = -40
let bp_fac2 = -80
let bp_fac3 = 20 
let bp_fac4 = 5
let bp_fac5 = -1
let bp_fac6 = 1

let eval_v0_fac = 10 
let eval_v1_fac = 5
let eval_v2_fac = 6


let final_border = 50

let minimum_score = -1111

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

let get_board_position_value my_board = 
  let v0 = bp_fac0 * (piece_count (Int64.logand my_board bp_mask0)) in 
  let v1 = bp_fac1 * (piece_count (Int64.logand my_board bp_mask1)) in 
  let v2 = bp_fac2 * (piece_count (Int64.logand my_board bp_mask2)) in 
  let v3 = bp_fac3 * (piece_count (Int64.logand my_board bp_mask3)) in 
  let v4 = bp_fac4 * (piece_count (Int64.logand my_board bp_mask4)) in 
  let v5 = bp_fac5 * (piece_count (Int64.logand my_board bp_mask5)) in 
  let v6 = bp_fac6 * (piece_count (Int64.logand my_board bp_mask6)) in 
  v0 + v1 + v2 + v3 + v4 + v5 + v6 

  

let get_evaluation_value (board: board64) (turn: int ) (moves: int) = 
  (* 評価値 *)
  let (my_board, other_board) = 
    if turn = black_turn then get_board_tuple board 
    else let (a, b) = get_board_tuple board in (b, a) in
  let v0 = if turn > final_border then 0 
    (* 自分の着手可能数- 相手の着手可能数 *)
           else (piece_count (get_legal_move board turn)) - 
                (piece_count (get_legal_move board (get_another_turn turn ))) in
  let v1 = (piece_count my_board) - (piece_count other_board) in 
    (* 自分の駒数 - 相手の駒数 *)
  let v2 = (get_board_position_value my_board) - (get_board_position_value other_board) in 
    (* 盤面の位置依存 *)
  let ans = v0 * eval_v0_fac +  v1 * eval_v1_fac + v2 * eval_v2_fac 
    in ans 

let is_pass aimove = 
  match aimove with 
    | Pass -> true 
    | _ -> false 

let is_mv aimove = 
  match aimove with 
    | Mv (i, j) -> true 
    | _ -> false 

let is_giveup aimove = 
  match aimove with 
    | GiveUp -> true 
    | _ -> false 

let get_mv aimove = 
  match aimove with 
    |Mv (i, j) -> (i, j)
    | _ -> raise AimoveTransException

let dc_mask = 0xffffffffL

let rec get_ai_move_in board64 mycolor time = 
  let legal_moves = get_legal_move board64 mycolor in 
  if((Int64.compare legal_moves Int64.zero) = 0) then Int64.zero
  else 
    let (candidate, score) = search_ai_move_dc board64 legal_moves mycolor dc_mask 32 Int64.zero minimum_score time in 
    candidate
  
  and 
  search_ai_move_dc board64 legal_moves mycolor mask shift_n candidate candidate_score time = 
  (* 分割統治法で探索 *)
    if((Int64.compare legal_moves Int64.zero) = 0) then (candidate, candidate_score) 
    else
      if(shift_n = 0) then 
        (*候補手発見*)
        (legal_moves, 0)
      else 
        (let shifted_mask = Int64.shift_left mask shift_n in 
        let legal_a = Int64.logand legal_moves shifted_mask in 
        let legal_b = Int64.logand legal_moves mask in 
          let new_mask_a = Int64.logand shifted_mask (Int64.shift_right_logical shifted_mask (shift_n / 2)) in 
          let new_mask_b = Int64.logand mask (Int64.shift_right_logical mask (shift_n / 2)) in 
          let (a_c, a_s) = search_ai_move_dc board64 legal_a mycolor new_mask_a (shift_n/2) candidate candidate_score time in 
          let (b_c, b_s) = search_ai_move_dc board64 legal_b mycolor new_mask_b (shift_n/2) candidate candidate_score time in 
          if (a_s > b_s) then (a_c, a_s)
          else (b_c, b_s))
          
        

let get_ai_move (board: int array array) (color: int) (time: int) = 
  let board64 = board_to_board64 board in 
  let mycolor = color_to_mycolor color in 
  let mymove = get_ai_move_in board64 mycolor time in 
  if((Int64.compare mymove Int64.zero) = 0) then 
    Pass
  else 
    let (i, j) = mymove_to_mv mymove in 
    Mv (i, j)
