open Syntax


exception Unbound
exception NotFound
exception ConsNotMatch
exception MatchFailure

let empty_env = []
let empty_type_env = []

let extend x v env = (x, v) :: env


let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound


exception EvalErr


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

let rec eval_expr env e =
  match e with
  | EConstInt i ->
    VInt i
  | EConstBool b ->
    VBool b
  | EVar x ->
    (try
       lookup x env
     with
     | Unbound -> raise EvalErr)
  |ELet (e1, e2, e3) -> 
    let t = eval_expr env e2 in 
    eval_expr (extend e1 t env) e3  
  | EAdd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 + i2)
     | _ -> raise EvalErr)
  | EMul (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 * i2)
     | _ -> raise EvalErr)
  | ESub (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 - i2)
     | _ -> raise EvalErr)
  | EDiv (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 / i2)
     | _ -> raise EvalErr)
  | EEq (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 = i2)
     | _ -> raise EvalErr)
  | ELt (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 < i2)
     | _ -> raise EvalErr)
  | EIf (e1,e2,e3) ->
    let v1 = eval_expr env e1 in
    (match v1 with
     | VBool b ->
       if b then eval_expr env e2 else eval_expr env e3
     | _ -> raise EvalErr)
  | EFun (x, e) -> VFun (x, e, env)
  | EApp (e1, e2) -> 
    let v1 = eval_expr env e1 in 
      let v2 = eval_expr env e2 in
        (match v1 with 
          | VFun (x, e, oenv) -> 
            eval_expr (extend x v2 oenv) e
          | VRecFun (f, x, e, oenv) -> 
            let env' = 
              extend x v2 (extend f (VRecFun(f, x, e, oenv)) oenv)
              in eval_expr env' e
          | _ -> raise  EvalErr)
  | ELetRec (f, x, e1, e2) -> 
    let env' = extend f (VRecFun(f, x, e1, env)) env
      in eval_expr env' e2
  | EPair (e0, e1) -> 
    VPair (eval_expr env e0, eval_expr env e1)
  | ENil -> VNil 
  | ECons (e0, e1) -> 
    (let v0 = eval_expr env e0 in 
    let v1 = eval_expr env e1 in 
      match v1 with 
        |VNil -> VCons(v0, v1)
        |VCons (a0, a1) -> VCons(v0, v1)
        | x -> raise ConsNotMatch)
  | EMatch (e, list) -> 
     let v0 = eval_expr env e in 
     eval_pattern_match_in env v0 list 
  and 
  eval_pattern_match_in  (env: env) (v: value) (list: (pattern * expr) list  ) = 
    match list with 
      | [] -> raise MatchFailure
      | (p, e) :: res -> 
        (let match_ans = find_match p v in 
          match match_ans with 
            | Some newEnv -> 
              eval_expr (newEnv @ env) e
            | None -> eval_pattern_match_in env v res)
    
  
let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (name,e) -> 
    let eVal = eval_expr env e in 
      let newEnv = (extend name eVal env) in
        (name, newEnv, eval_expr newEnv e)
  | CRecDecl (name1, name2, e) -> 
    let env' = extend name1 (VRecFun(name1, name2, e, env)) env in
      (name1, env', (VRecFun(name1, name2, e, env)))
    
