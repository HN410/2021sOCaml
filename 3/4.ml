type order = LT | EQ | GT
(*LT ... x < y*)

exception GET_VALUE_OF_NONE;;

module type ORDERED_TYPE = 
sig 
  type t
  val compare : t -> t -> order 
end

module type CONTENT_TYPE = 
sig
  type t
end

module type ASSOCIATIVEARRAY = 
  functor (T : ORDERED_TYPE) -> 
    functor (Tc : CONTENT_TYPE) ->
        sig 
        type t 
        (*lookupの結果として返される型.元の型のまま返すと見つからなかった
        場合に困る*)
        type res
        val empty : t 
        val add    : (T.t * Tc.t) -> t -> t 
        val remove : T.t -> t -> t 
        val lookup  : T.t -> t -> res 
        val getKeySet : t -> T.t list
        val getValue : res -> Tc.t
        end 

module AssoacitveArray : ASSOCIATIVEARRAY =
  functor (T : ORDERED_TYPE) ->
    functor (Tc : CONTENT_TYPE) -> 
        struct 
        type t = (T.t * Tc.t) list
        type res = None | Value of Tc.t
        let rec remove a xs = 
        match xs with 
        | [] -> []
        | (key, content) :: ys ->
        (match T.compare a key with 
            | LT -> (key, content) :: ys
            | EQ -> ys 
            | GT -> (key, content) :: remove a ys)
        let empty = []
        (*すでにあるキーを持つ組が追加された場合は更新する *)
        let rec add (key, content) xs =
        match xs with
        | [] -> [(key, content)]
        | (yk, yc) :: ys ->
        (match T.compare key yk with
        | LT -> (key, content) :: (yk, yc) :: ys
        | EQ -> (key, content) :: ys
        | GT -> (yk, yc) :: add (key, content) ys)
        let rec lookup key array = 
        match array with
        |[] -> None
        |(yk, yc) :: ys -> 
            (match T.compare key yk with 
            |LT -> None
            |EQ -> Value (yc)
            |GT -> lookup key ys)
        let getValue x = 
        match x with 
        |None -> raise GET_VALUE_OF_NONE
        |Value a -> a
        let rec getKeySet a = 
        match a with 
            |[] -> []
            |(yk, yc) :: ys -> yk :: (getKeySet ys)
        end


module OrderedString =
struct
  type t = string
  let compare x y = 
    let r = Pervasives.compare x y in
      if      r > 0 then GT 
      else if r < 0 then LT 
      else               EQ
end 

module StringAssoArray =
  AssoacitveArray(OrderedString) (OrderedString)