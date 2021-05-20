(* type test = None | Some of int;;

let (>>=) x f = match x with
  | None -> None
  | Some k -> f k

let rec myLookup key xs = 
  match xs with 
    |[] -> None
    | ((k, v) :: rest) -> 
      if key = k then Some v
        else myLookup key rest ;;

let myLookup3 k1 t1 t2 = 
  myLookup k1 t1 >>= (fun k2 -> 
  myLookup k2 t2 );; *)

let (>>=) x f = List.concat (List.map f x );;

let return x = [x];;

let guard b = if b then return () else [];;


let sat f = 
  [true; false] >>= (fun x -> 
  [true; false] >>= (fun y -> 
  [true; false] >>= (fun z -> 
  (guard (f x y z) >>= (fun _ -> 
    return (x, y, z))))))
  
