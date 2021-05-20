type 'a searchR = Ok of 'a | Err of string;;

(*ディクショナリlからkをキーにもつ要素を探す* *)
let rec keySearch l k = 
  match l with 
    |[] -> Err "Not Found"
    |(a, b) :: xs ->
      if a = k then Ok b
      else keySearch xs k


(*'aのタプルのリスト（メモに対応）を受け取りそれと'aのタプルを返す *)
type 'a m = ('a* 'a) list -> ('a * (('a * 'a) list ));;

(** (>>=) : 'a m -> ('a -> 'b m) -> 'b m *)
let (>>=) (x : 'a m) (f : 'a -> 'b m) = 
  fun init -> ( 
    let (a, b) = x init in 
      let (c, d) = (f a) b in 
      (c, d ))  
    
(** return : 'a -> 'a m *)
let return x = fun init -> (x, init)

(** memo : (int -> int m) -> int -> int m *)
let memo (f : int -> int m) n = 
  fun x -> 
    (*受け取ったディクショナリの中に目的の返り値があるか検索 *)
    (let res = keySearch x n in 
      match res with 
        |Ok a -> (a, x)
        |Err a ->( 
          (*なければ計算してディクショナリに追加 *)
          match (f n) x with 
            |(c, d) -> (c, (n, c) ::d)))
    

(* runMemo : 'a m -> 'a *)
let runMemo (x : 'a m) = match  x [] with 
  |(a, b) -> a

let rec fib n =
  if n <= 1 then
    return n
  else
    (memo fib (n-2)) >>= (fun r1 ->
    (memo fib (n-1)) >>= (fun r2 ->
      return (r1 + r2)))

let _ =
  if runMemo (fib 80) = 23416728348467685 && runMemo (fib 10) = 55 then
    print_string "ok\n"
  else
    print_string "wrong\n"
