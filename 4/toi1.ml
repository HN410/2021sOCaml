type 'a myErr = Ok of 'a | Err of string;;

let rec eLookup key xs = 
  match xs with 
  | [] -> Err "Not Found"
  | ((k, v) :: rest ) -> 
    if key = k then Ok v
      else eLookup key rest;;

(* eLookup 3 [(2, "fuga"); (3, "hoge"); (1, "poyo")];;
eLookup 3 [(2, "fuga"); (0, "hoge"); (1, "poyo")];; *)

let myDiv x y = 
  if y = 0 then raise (Failure "Div by Zero")
    else x / y;;

let (>>=) x f = match x with 
  |Ok a -> f a 
  |Err s -> raise (Failure s);;

let lookupDiv x y table = 
  eLookup x table >>= (fun a -> 
  eLookup y table >>= (fun b -> 
    myDiv a b));;

(* lookupDiv "fuga" "poyo" [("poyo", 2); ("hoge", 0); ("fuga", 6)];; *)
  

