type tyvar = Var of int (*idで管理 *)

type ty = 
  | TypeInt
  | TypeBool
  | TypeFun of ty * ty
  | TypeVar of tyvar

type subst = (tyvar * ty) list (*インデックスに重複はないとする *)

exception NotFoundVarError
exception ImplementationError

        
let isEqualVar (var1 : tyvar) (var2 : tyvar ) = 
  (*変数が同じものかを比較 *)
  let (Var a, Var b) = (var1, var2) in 
   a = b

let rec hasIndexInSubst (sub: subst) (var: tyvar) = 
  (*subでvarのマッピングが存在するか *)
  match sub with 
    |[] -> false
    |(varIn, ty) :: res -> 
      if isEqualVar varIn var then true 
        else hasIndexInSubst res var 

let rec ty_subst_var (sub: subst) (typ : ty) = 
  (*変数専用型代入* *)
  match sub with 
    | [] -> typ
    | (var , ty) :: res -> 
       (match  (var, typ)  with 
         |(Var a, TypeVar (Var b)) -> 
           if a = b then ty else ty_subst_var res typ 
         | x -> raise ImplementationError)
    
let rec ty_subst (sub : subst) (typ : ty) = 
  (* 型代入と型を受け取り，tへ適用した結果を返す*)
  match typ with 
    |TypeInt -> TypeInt 
    |TypeBool -> TypeBool
    |TypeFun (a, b)-> 
      TypeFun ((ty_subst sub a), (ty_subst sub b))
    |TypeVar a -> 
      (ty_subst_var sub typ)


let rec compose1 (sub1: subst) (sub2: subst) = 
  (*代入の合成の第一段階 sub2内に定義されているマッピングについて合成
  返り値は合成結果*)
  match sub2 with 
    | [] -> [];
    | (var, ty) :: res -> 
      (var , ty_subst sub1 ty) :: (compose1 sub1 res)

let rec compose2 (sub1: subst) (sub2: subst) = 
  (*代入の第二段階 sub2に定義されていないがsub1に定義されているマッピング
  を追加*)
  match sub1 with 
    | [] -> sub2 
    | (var, ty) :: res -> 
      if hasIndexInSubst sub2 var then compose2 res sub2
        else (var, ty) :: (compose2 res sub2)

let compose (sub1: subst) (sub2: subst) = 
  (*代入を合成 sub1 \circ sub2 *)
  (*とりあえず代入のインデックスに重複はないとする *)
  let res1 = compose1 sub1 sub2 in 
    compose2 sub1 res1   

  
