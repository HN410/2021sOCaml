type name = string

type expr =
  | EConstInt  of int
  | EConstBool of bool
  | EVar       of name 
  | EAdd       of expr * expr
  | ESub       of expr * expr
  | EMul       of expr * expr
  | EDiv       of expr * expr
  | EEq        of expr * expr
  | ELt        of expr * expr		 
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | EFun       of name * expr
  | EApp       of expr * expr
  | ELetRec    of name * name * expr * expr
  | EPair      of expr * expr
  | ENil
  | ECons      of expr * expr


type value = 
  | VInt    of int
  | VBool   of bool
  | VFun    of name * expr * env
  | VRecFun of name * name * expr * env
  | VPair   of value * value 
  | VNil    
  | VCons   of value * value 
  and env = (name * value ) list

type pattern = 
  | PInt of int 
  | PBool of bool
  | PVar of name 
  | PPair of pattern * pattern
  | PNil 
  | PCons of pattern * pattern


let rec find_match (p: pattern) (v: value) = 
  (* パターンと値を照合 
  　　あっていたら環境のSome型，間違ってたらNoneを返す *)
  match p with 
    |PInt a -> 
      (match v with 
        | VInt b -> if a = b then Some []
                    else None 
        | x -> None )
    |PBool a -> 
      (match v with 
        | VBool  b -> if a = b then Some []
                    else None
        | x -> None 
      )
    |PVar a -> 
      Some [(a, v)]
    |PPair (a0, a1) -> 
      (match v with 
        |VPair (b0, b1) -> find_match_2var a0 a1 b0 b1 
        |x -> None )
    |PNil -> 
      (match v with 
        |VNil -> Some []
        |x -> None )
    |PCons (a0, a1) -> 
        (match v with 
            |VCons (b0, b1) -> find_match_2var a0 a1 b0 b1 
            |x -> None )
    and find_match_2var a0 a1 b0 b1 = 
      let ans0 = find_match a0 b0 in 
          let ans1 = find_match a1 b1 in 
          (match (ans0, ans1) with 
            | (None, x) -> None
            | (x, None) -> None 
            | (Some x0, Some x1) -> 
              Some (x0@x1))