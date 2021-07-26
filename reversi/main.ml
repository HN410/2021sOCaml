open Int64
open Game
open Ai

let test_white = 0x10787820301000L
let test_black = 0x2002045c0c0000L
let test_white_1 = 0x007e7e7e2c7e7e00L
let test_black_1 = 0xff818181428181ffL

let new_black = 0x8000000000L
let new_black_1 = 0x10000000L ;;

(print_board (make_board test_black test_white))