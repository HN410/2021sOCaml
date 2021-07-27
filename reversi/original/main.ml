open Int64
open Game
open Ai
open Translate 


let test_white = 0x10787820301000L
let test_black = 0x2002045c0c0000L
let test_white_1 = 0x007e7e7e2c7e7e00L
let test_black_1 = 0xff818181428181ffL;;
let test_board = make_board test_black test_white;;

let new_black = 0x8000000000L
let new_black_1 = 0x10000000L ;;

(print_board (test_board));
print_newline();
print_only_board (get_legal_move test_board 0) 0;
print_int (piece_count test_white);
print_newline();
print_int (get_evaluation_value test_board black_turn 20);

type color = int 
type board = color array array
let none     = 0
let white    = 1 
let black    = 2
let sentinel = 3 

let init_board () =
  let board = Array.make_matrix 10 10 none in
    for i=0 to 9 do
      board.(i).(0) <- sentinel ;
      board.(i).(9) <- sentinel ;
      board.(0).(i) <- sentinel ;
      board.(9).(i) <- sentinel ;
    done;
    board.(4).(4) <- white;
    board.(5).(5) <- white;
    board.(4).(5) <- black;
    board.(5).(4) <- black;
    board
;;
print_board (board_to_board64 (init_board ()));;
print_only_board (mv_to_mymove 8 1) 1 ;;
let (a, b) = mymove_to_mv ((mv_to_mymove 8 1)) in 
  print_int a ;
  print_newline ();
  print_int b;
  print_newline ();
;;
let (c,d ) = get_mv (get_ai_move (init_board ()) 1 100 ) in 
  print_int c;
  print_newline();
  print_int d;
  print_newline ();