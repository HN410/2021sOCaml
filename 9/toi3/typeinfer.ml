
open Syntax
open Type

exception TypeNotFound
exception UnboundedValue
exception TypeError

let rec searchTyEnv name env = 
  match env with
    | (name1, ty) ::res -> if name1 = name then ty else searchTyEnv name res 
    | _ -> raise TypeNotFound

let getUnionEnv env1 env2 = env1 @ env2

let rec  infer_expr (typeEnv : tyenv) (expr : expr) = 
  (match expr with 
  | EConstInt  i -> (TypeInt, [])
  | EConstBool i -> (TypeBool, [])
  | EVar       i -> 
    (try ( let iType = searchTyEnv i typeEnv in 
      (instantiate iType, [])  )
     with Not_found -> raise UnboundedValue)
  | EAdd       (e1, e2) -> infer_expr_doubleInt e1 e2 typeEnv
  | ESub       (e1, e2) -> infer_expr_doubleInt e1 e2 typeEnv
  | EMul       (e1, e2) -> infer_expr_doubleInt e1 e2 typeEnv
  | EDiv       (e1, e2) -> infer_expr_doubleInt e1 e2 typeEnv
  | EEq        (e1, e2) -> 
    let (t1, c1) = infer_expr typeEnv e1 in 
    let (t2, c2) = infer_expr typeEnv e2 in 
      (TypeBool, (t1, t2) :: (getUnionEnv c1 c2))
  | ELt        (e1, e2) -> 	 
    let (t1, c1) = infer_expr typeEnv e1 in 
    let (t2, c2) = infer_expr typeEnv e2 in
       
      ( TypeBool, (t1, t2) :: ((t1, TypeInt):: ( (t2, TypeInt) :: (getUnionEnv c1 c2))))
  | EIf        (e1, e2, e3) -> 
    let (t1, c1) = infer_expr typeEnv e1 in 
    let (t2, c2) = infer_expr typeEnv e2 in 
    let (t3, c3) = infer_expr typeEnv e3 in 
      (t2, (t1, TypeBool) :: ((t2, t3) :: getUnionEnv c1 (getUnionEnv c2 c3)))
  | ELet       (name, e1, e2) -> 
    let (t1, c1) = infer_expr typeEnv e1 in 
      let sigma = ty_unify c1 in 
      let s1 = ty_subst sigma t1 in 
      let delta = tyenv_subst sigma typeEnv in 
        let newEnv = 
        (name, TypeSchema (generalize_delete delta (get_typevars s1), s1))
           :: delta in 
          let (t2, c2) = infer_expr newEnv e2 in 
            (t2, getUnionEnv c1 c2)
  | EFun       (name, e1) -> 
    let alpha = TypeVar (new_tyvar () )in 
      let env_ = (name, Type alpha) :: typeEnv in 
        let (t, c) = infer_expr env_ e1 in
          ((TypeFun(alpha, t)), c)
  | EApp       (e1, e2) -> 
    let (t1, c1) = infer_expr typeEnv e1 in 
    let (t2, c2) = infer_expr typeEnv e2 in 
      let alpha = TypeVar (new_tyvar ()) in 
        (alpha, (t1, TypeFun(t2, alpha)) :: getUnionEnv c1 c2)
  | ELetRec    (name1, name2, expr1, expr2)  -> 
    let alpha = TypeVar (new_tyvar ()) in 
    let beta = TypeVar (new_tyvar ()) in
      let env_ = (name1, Type (TypeFun(alpha, beta))) :: typeEnv in
        let (t1, c1) = infer_expr ((name2, Type alpha) :: env_) expr1 in 
          let sigma = ty_unify c1 in 
          let s1 = ty_subst sigma t1 in 
          let funType = TypeFun(ty_subst sigma alpha , s1) in 
          let delta = tyenv_subst sigma typeEnv in 
            let newEnv = 
            (name1, TypeSchema (generalize_delete delta (get_typevars funType), funType))
              :: delta in 
              let (t2, c2) = infer_expr newEnv expr2 in 
                (t2, getUnionEnv c1 c2))
  and 
  infer_expr_doubleInt e1 e2 typeEnv = 
    let (t1, c1) = infer_expr typeEnv e1 in 
    let (t2, c2) = infer_expr typeEnv e2 in 
      (TypeInt, (t1, TypeInt) :: ((t2, TypeInt) :: getUnionEnv c1 c2))
let infer_cmd (typeEnv : tyenv) cmd = 
  try (
    match cmd with 
      | CExp e -> 
        let (t, c) = infer_expr typeEnv e  in 
          let newTy = ty_subst (ty_unify c) t in 
            (Type newTy, typeEnv)
      | CDecl (name, e) -> 
        let (t, c) = infer_expr typeEnv e in 
          let sigma = ty_unify c in 
          let s = ty_subst sigma t in 
          let delta = tyenv_subst sigma typeEnv in 
            let newType = TypeSchema (
              generalize_delete delta (get_typevars s), s) in 
              (newType, (name, newType) :: delta)

      | CRecDecl (name1, name2, e) -> 
      print_string name1;
        let alpha = TypeVar (new_tyvar ()) in 
        let beta = TypeVar (new_tyvar ()) in 
          let env_ = (name1, Type(TypeFun(alpha, beta))) :: (((name2, Type alpha) ::  typeEnv)) in 
            let (t, c ) = infer_expr env_ e in 
              let sigma = ty_unify c in 
              let s = ty_subst sigma t in 
              let funType = TypeFun(ty_subst sigma alpha , s) in 
              let delta = tyenv_subst sigma typeEnv in 
                let newType = TypeSchema (generalize_delete delta (get_typevars funType), funType) in 
                let newEnv =  (name1, newType) :: delta in 
                  (newType, newEnv)

  )with 
   |UnboundedValue -> raise UnboundedValue
   |ImplementationError -> raise ImplementationError
   (* |TypeNotFound -> raise TyError *)
   |UnifyFailError -> raise TyError 
     (*TypeNotFound, TyErrorなら，TypeErrorと表示して実行を継続できる *)


