type order = LT | EQ | GT
(*LT ... x < y*)

module type ORDERED_TYPE = 
sig 
  type t
  val compare : t -> t -> order 
end


module type MULTISET2 = 
  functor (T : ORDERED_TYPE) -> 
    sig 
      type t 
      val empty : t 
      val add    : T.t -> t -> t 
      val remove : T.t -> t -> t 
      val count  : T.t -> t -> int 
    end 

module Multiset2 : MULTISET2 =
  functor (T : ORDERED_TYPE) -> struct 
    type t = Leaf
             |Node of (T.t * t*t)

    (*二分木の削除用.木のうち最小の値とそれを削除した木を返す*)
    let rec getMinAndTree tree = match tree with
      |Node (b, x, y) -> 
        match x with 
          |Leaf -> (match y with 
            |Leaf  -> (b, Leaf)
            |Node (c, x1, y1) -> (b, Node (c, x1, y1)))
          |Node (c, x1, y1) -> (match getMinAndTree x with
             |(d, e) -> (d, Node (b, e, y)))
    
    let rec remove a xs = 
      match xs with 
	| Leaf -> Leaf
	| Node (b, x, y)-> 
	  (match T.compare a b with 
	    | LT -> Node (b, remove a x, y)
	    | EQ -> (match y with 
          |Leaf -> (match x with 
            |Leaf -> Leaf
            |Node (c, x1, y1) -> Node (c, x1, y1))
          |Node (c, x1, y1) -> match getMinAndTree y with
            |(min, tree) -> Node (min, x, tree))
	    | GT -> Node (b, x, remove a y))
    let empty = Leaf
    let rec add a xs =
      match xs with
      | Leaf -> Node (a, Leaf, Leaf)
      | Node (b, x, y) -> 
	 (match T.compare a b with
	  | LT -> Node (b, (add a x), y)
      (*すでに入っている要素を追加する場合はGTと同じとする *)
	  | _ -> Node (b, x , (add a y)))
    let rec count_sub a xs k =
      match xs with 
	  Leaf     -> k 
	| Node (b, x, y)  -> 
	   (match T.compare a b with
	    | GT -> count_sub a x k
	    | EQ -> count_sub a y (k+1)
	    | LT -> count_sub a y k)
    let count a xs = count_sub a xs 0 
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

module StringMultiset =
  Multiset2 (OrderedString)
