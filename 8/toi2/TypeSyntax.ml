type typevar

type ty = 
  | TypeInt
  | TypeBool
  | TypeFun of ty * ty
  | TypeVar of typevar