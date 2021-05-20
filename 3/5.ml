exception WRONG_LEN_FOR_ARRAY;;(*行列の要素数が不正 *)
exception WRONG_ARGUMENT_COMBINATION;;(*引数が不正(行列＋ベクトルなど) *)
exception INTERNAL_ERROR;;(*内部でのエラー *)

module type SEMIRING = sig
  type t
  val add : t -> t -> t
  val mul : t -> t -> t
  val unit : t
  val zero : t
end

module type ARRAYVECTORCALC = 
  functor (T:SEMIRING) -> 
    sig 
      type t
      val vec : T.t list -> t
      val array : (T.t list) list -> t
      val addL : t -> t -> t
      val innerP : t -> t-> T.t
      val mulL : t -> t -> t
      val transpose : t -> t
      val getListV : t -> T.t list
      val getListArray : t -> (T.t list) list
    end

module ArrayVectorCalc : ARRAYVECTORCALC = 
  functor (T : SEMIRING) -> struct 
    type t = Vec of (int * (T.t list))
          |Array of ((int * int) * ((T.t list) list))

    let rec getLenSub a n = (match a with 
        |[] -> n
        |b :: bs -> (getLenSub bs (n+1) ))
    let getLen x = 
    (*リストの長さを調べる *)
      getLenSub x 0
      
    let vec x = Vec(getLen x, x)
    let rec arraySub x y m n = match y with
      | [] -> Array((m, n), x)
      | z :: zs -> 
        if n = getLen z then 
          arraySub x zs (m+1) n
        else raise WRONG_LEN_FOR_ARRAY

    let rec array x =  match x with 
      | y::ys -> arraySub x ys 1 (getLen y)
      | _ -> raise INTERNAL_ERROR

    let rec addV ls1 ls2 = 
      match (ls1, ls2) with 
        |([], []) -> []
        |(x::xs, y ::ys) -> (T.add x y) :: addV xs ys
        |_ -> raise INTERNAL_ERROR
    
    let rec addA ls1 ls2 = 
      match (ls1, ls2) with 
        |([], []) -> []
        |(x::xs, y::ys) -> addV x y :: addA xs ys
        |_ -> raise INTERNAL_ERROR

    let addL x y = 
      (*要素数の合ったベクトル．行列同士の足し算のみ定義 *)
      match (x, y) with 
        |(Vec (n1, ls1), Vec(n2, ls2)) when n1 = n2 -> Vec (n1, addV ls1 ls2)
        |(Array((m1,n1), ls1), Array((m2,n2), ls2)) 
          when m1 = m2 && n1 = n2 -> Array((m1,n1), addA ls1 ls2)
        | _ -> raise WRONG_ARGUMENT_COMBINATION

   
    let rec lookupIn x i now= 
      match x with 
         |[] -> raise INTERNAL_ERROR
         |y ::ys -> 
           if i = now then y
           else lookupIn ys i (now+1)
    let lookup x i = lookupIn x i 0 (*リストxのi番目を得る *)

    let rec transposeSub1 ls i = 
      match ls with 
        |[] -> []
        |x ::xs -> (lookup x i) ::transposeSub1 xs i

    let rec transposeSub2 ls i n  = 
      if i == n then []
      else (transposeSub1 ls i) :: transposeSub2 ls (i+1) n
       
    let transpose x = 
      (*行列の転置 *)
      match x with 
        |Array ((m, n), ls) -> Array((n, m), transposeSub2 ls 0 n)
        |_-> raise WRONG_ARGUMENT_COMBINATION

    let rec innerPSub ls1 ls2 res = 
      match (ls1, ls2) with 
        | ([], []) -> res
        | (x::xs, y::ys) -> innerPSub xs ys (T.add res (T.mul x  y))
        |_-> raise INTERNAL_ERROR

    let innerP v1 v2 = 
      (*ベクトル同士の内積 *)
      match (v1, v2) with 
        |(Vec (n1, ls1), Vec(n2, ls2)) when n1 = n2 -> innerPSub ls1 ls2 T.zero
        |_-> raise WRONG_ARGUMENT_COMBINATION

    let rec mulSub1 vec arr = 
      match arr with 
        |[] -> []
        |x :: xs -> innerPSub vec x T.zero :: (mulSub1 vec xs)
    let rec mulSub2 ls1 ls2 = 
      match ls1 with 
        |[] -> []
        |x :: xs -> mulSub1 x ls2 :: (mulSub2 xs ls2)

    let mulL x y = 
      (*行列同士の積を定義．どちらも適切な要素数出ないとエラー． *)
      match (x, y) with 
        |(Array ((m,n1), ls1), Array ((n2, l), ls2)) 
          when n1 = n2 -> Array((m, l), mulSub2 ls1 (transposeSub2 ls2 0 l))
        |_ -> raise WRONG_ARGUMENT_COMBINATION

    let getListV t = 
      match t with 
        |Vec(i, ls) -> ls
        |_ -> raise WRONG_ARGUMENT_COMBINATION
    
    let getListArray t = 
      match t with 
        |Array(i, ls) -> ls
        |_ -> raise WRONG_ARGUMENT_COMBINATION
    end

module BoolRing = 
struct 
  type t = bool
  let add x y = x || y
  let mul x y = x && y
  let unit = true
  let zero = false
end

module MinPlusRing = 
struct
  type t = VInt of int | Inf
  let add x y = 
    match (x, y) with 
      |(VInt a, VInt b) -> if a < b then VInt a else VInt b
      |(Inf, z) -> z
      |(z, Inf) -> z
  let mul x y = 
    match (x, y) with
      |(VInt a, VInt b) -> VInt (a + b)
      |(Inf, z) -> Inf
      |(z, Inf) -> Inf
  let unit = VInt 0
  let zero = Inf
end

module BRingArray = 
  ArrayVectorCalc(BoolRing)
module MPRingArray = 
  ArrayVectorCalc(MinPlusRing)

let testArrayB1 = [[true;false;false];[false;true;true]];;
let testArrayB2 = [[false];[true];[true]];;
let testVecM1 = [MinPlusRing.VInt 3;MinPlusRing.Inf];;
let testVecM2 = [MinPlusRing.VInt 4; MinPlusRing.VInt 1];;


let ans1 = (BRingArray.getListArray (BRingArray.mulL
  (BRingArray.array testArrayB1) (BRingArray.array testArrayB2)));;
let ans2 = (MPRingArray.getListV (MPRingArray.addL 
  (MPRingArray.vec testVecM1) (MPRingArray.vec testVecM2 )));;