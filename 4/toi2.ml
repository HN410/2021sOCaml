let (>>=) x f = List.concat (List.map f x );;

let return x = [x];;

let guard b = if b then return () else [];;

let numList = [0;1;2;3;4;5;6;7;8;9];;
let numListNZ = [1;2;3;4;5;6;7;8;9];;

let bananaFun ba na si mo nn = 
  if ((2*na) mod 10 = nn ) && 
    ((2*na) / 10 + ((2*na) mod 10) = mo) && 
    (((2*na) / 10 + (2*ba) mod 10) = na) &&
    ((2*ba) / 10 = si) then true
  else false;;

let maskCalc1 = 
  numList >>= (fun ba -> 
  numList >>= (fun na -> 
  numList >>= (fun si -> 
  numList >>= (fun mo -> 
  numList >>= (fun nn -> 
  (guard (bananaFun ba na si mo nn) >>= (fun _ -> 
    return [("ba", ba); ("na", na); ("si", si); ("mo", mo);
      ("nn", nn)])))))));;

let moneyFun s e n d m o r y = 
  if ((d + e)mod 10 = y) && 
     (((d+e)/10 + (n + r) mod 10) = e) &&
     (((n+r)/10 + (e + o) mod 10) = n) &&
     (((e+o)/10 + (s + m) mod 10) = o) &&
     ((s+m)/10 = m) then true
    else false;;
  
let rec equalAny a list = 
  match list with 
    | [] -> false
    | x :: xs -> if x = a then true 
      else equalAny a xs;;

let maskCalc2 = 
  numListNZ >>= (fun s -> 
  numList >>= (fun e -> 
  if equalAny e [s] then [] else
  numList >>= (fun n ->
  if equalAny n [s; e] then [] else
  numList >>= (fun d ->
  if equalAny d [s; e; n] then [] else 
  numListNZ >>= (fun m -> 
  if equalAny m [s; e; n; d] then [] else
  numList >>= (fun o -> 
  if equalAny o [s; e; n; d; m] then [] else
  numList >>= (fun r -> 
  if equalAny r [s; e; n; d; m; o] then [] else
  numList >>= (fun y -> 
  if equalAny y [s; e; n; d; m; o; r] then [] else
  (guard (moneyFun s e n d m o r y) >>= (fun _ -> 
    return [("s", s); ("e", e); ("n", n); ("d", d);
      ("m", m); ("o", o); ("r", r); ("y", y)]))))))))));;