type tyvar = Var of int (*idで管理 *)

type ty = 
  | TypeInt
  | TypeBool
  | TypeFun of ty * ty
  | TypeVar of tyvar

type subst = (tyvar * ty) list

exception NotFoundVarError
exception ImplementationError

let rec ty_subst_var (sub: subst) (typ : ty) = 
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

    
  
