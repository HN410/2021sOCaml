open Syntax

type tyvar = Var of int (*idで管理 *)

type ty = 
  | TypeInt
  | TypeBool
  | TypeFun of ty * ty
  | TypeVar of tyvar
  | TypePair of ty * ty 
  | TypeList of ty


type type_schema = 
  | TypeSchema of tyvar list * ty 
  | Type of ty 
  

type tyenv = (name * type_schema) list


type subst = (tyvar * ty) list (*インデックスに重複はないとする *)
(* 
type tyenv = (name * ty) list *)

exception NotFoundVarError
exception ImplementationError
exception UnifyFailError
exception TyError

let is_equal_var (var1 : tyvar) (var2 : tyvar ) = 
  (*変数が同じものかを比較 *)
  let (Var a, Var b) = (var1, var2) in 
   a = b

let rec has_index_in_subst (sub: subst) (var: tyvar) = 
  (*subでvarのマッピングが存在するか *)
  match sub with 
    |[] -> false
    |(varIn, ty) :: res -> 
      if is_equal_var varIn var then true 
        else has_index_in_subst res var 

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
    |TypePair (a, b) -> 
      TypePair ((ty_subst sub a), (ty_subst sub b))
    |TypeList a -> 
      (TypeList (ty_subst sub a)) 


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
      if has_index_in_subst sub2 var then compose2 res sub2
        else (var, ty) :: (compose2 res sub2)

let compose (sub1: subst) (sub2: subst) = 
  (*代入を合成 sub1 \circ sub2 *)
  (*とりあえず代入のインデックスに重複はないとする *)
  let res1 = compose1 sub1 sub2 in 
    compose2 sub1 res1   

let rec const_subst (const: (ty * ty) list) (sub : subst) =
  (*制約に代入を適用する *)
  match const with 
    |[] -> []
    |(a,b) :: res -> 
      (ty_subst sub a, (ty_subst sub b)) 
        :: const_subst res sub

let rec has_var_in_type (typ: ty) (var: tyvar) = 
  (*typ のなかにtyvarを含んでいるか *)
  match typ with 
    |TypeInt -> false
    |TypeBool -> false
    |TypeFun (a, b) -> 
      has_var_in_type a var  || has_var_in_type b var 
    |TypeVar a -> is_equal_var a var
    |TypePair (a, b) -> 
      has_var_in_type a var  || has_var_in_type b var 
    |TypeList a-> 
      has_var_in_type a var

let rec ty_unify (const: (ty * ty) list) = 
  (*制約を受け取って単一化 *)
  match const with 
    |[] -> []
    |first :: res -> 
      (match first with 
      | (a, b) when a = b -> ty_unify res
      | (TypeFun (a,b), TypeFun (c, d)) -> 
        ty_unify ((a, c) :: ((b,d) :: res))
      | (TypeVar a, b) -> ty_unify_var a b res
      | (b, TypeVar a) -> ty_unify_var a b res
      | (TypePair (a, b), TypePair(c,d) )-> 
        ty_unify ((a, c) :: ((b,d) :: res))
      | ((TypeList a), (TypeList b)) -> 
        ty_unify ((a, b) :: res)
      | x -> raise UnifyFailError)
  and 
  ty_unify_var (a: tyvar) (b: ty) (const : (ty * ty) list) = 
  (*型変数がある時の処理 *)
    if (has_var_in_type b a) then raise UnifyFailError 
    else 
      let abSubst = [(a, b)] in 
      compose (ty_unify (const_subst const abSubst)) abSubst  

let rec get_typevars (typ: ty) = 
  (*型typ内にある型変数のリストを返す *)
  match typ with 
    |TypeInt -> []
    |TypeBool -> []
    |TypeFun (a, b) -> (get_typevars a) @ (get_typevars b)
    |TypeVar a -> [a]
    |TypePair (a, b) -> (get_typevars a) @ (get_typevars b)
    |TypeList a -> (get_typevars a)



let new_tyvar = 
  (*新しい変数をかぶらないように得る *)
  let used = ref 0 in 
  fun (unit : unit) -> 
    let now = !used in 
      used := now +1; Var now;;

let print_var (var: tyvar) = 
  (*型変数を表示 *)
    let (Var i) = var in 
      print_string "a";
      print_int i

let rec print_type (typ: ty) = 
  (*型を表示 *)
  match typ with 
  | TypeInt -> print_string "int "
  | TypeBool -> print_string "bool "
  | TypeFun (a, b) -> 
    print_string "(fun "; 
    print_type a;
    print_string "-> ";
    print_type b;
    print_string ") "
  | TypeVar c -> 
    print_var c
  | TypePair (a, b) -> 
    print_string "(";
    print_type a;
    print_string ",";
    print_type b;
    print_string ")"
  | TypeList a -> 
    print_type  a;
    print_string " list "



    

let rec print_typevars (typevars: tyvar list) = 
  (*型変数のリストを表示 *)
  match typevars with 
    | [] -> ()
    | var :: res -> 
      print_var var; 
      print_string " ";
      print_typevars res
      

let print_typeschema (schema: type_schema) = 
  (*型スキーマを表示 *)
  print_string "(";
  (match schema with 
    |TypeSchema (typeVars, typ) -> 
      print_string "∀ " ; 
      print_typevars typeVars ;
      print_string ". ";
      print_type typ 
    |Type ty -> print_type ty 
  );
  print_string ")"

let rec has_type_in_tyenv (env: tyenv) (tyVar: tyvar) = 
  (*型環境内にある型変数が出現するか *)
  match env with 
    | [] -> false
    | (name, ty) :: res -> 
      let typ =  
        (match ty with 
          |Type t -> t
          |TypeSchema (a, t) -> TypeInt (*要注意 *)
        )
      in 
        if has_var_in_type typ tyVar then true 
        else has_type_in_tyenv res tyVar 
        
let rec generalize_delete (env: tyenv) (typeList: tyvar list) = 
  (*typeListのうち，型環境内に現れるものを除く*)
  match typeList with 
    |[] ->  []
    |tyVar :: res -> 
      if has_type_in_tyenv env tyVar then generalize_delete env res 
      else tyVar :: generalize_delete env res 

let generalize (env: tyenv) (typ: ty) = 
  (*型を型スキームに一般化する 
    typのうち，envには現れない型変数を型変数の集合とする*)
    let typList = get_typevars typ in
      generalize_delete env typList 


let rec  instantiate_in_1 (tyvarList: tyvar list) (typ: ty) = 
  (* schemaの自由変数を一つずつ新しい型変数に入れ替える*)
    match tyvarList with 
      | [] -> typ
      | oldVar :: res -> 
        let newType = TypeVar  (new_tyvar ()) in 
          ty_subst [(oldVar, newType)] typ

let instantiate (schema: type_schema) = 
  (*型スキーマを型へ *)
  match schema with 
    |TypeSchema (tyvarList, typ) ->  
      instantiate_in_1 tyvarList typ
    |Type ty -> ty



let rec subst_setminus (sigma: subst) (varList: tyvar list) = 
  (* varListに含まれる型変数の置換を消す*)
  match sigma with 
    |[] -> []
    |(var, typ) ::res -> 
      if List.mem var varList then subst_setminus res varList 
      else (var, typ) :: (subst_setminus res varList)

let tyschema_subst (sigma: subst) (schema: type_schema) = 
  (*型スキーマに対して置換を行う *)
  match schema with 
    |TypeSchema (varList, typ) ->  
      let newSigma = subst_setminus sigma varList in 
      TypeSchema (varList, ty_subst newSigma typ)
    |Type ty -> 
      Type (ty_subst sigma ty)


let rec tyenv_subst (sigma: subst) (env: tyenv)  = 
  (*型環境に対して置換を行う *)
  match env with 
    |[] -> []
    |(name, schema) ::res -> 
      (name, tyschema_subst sigma schema) 
        :: tyenv_subst sigma res
      
