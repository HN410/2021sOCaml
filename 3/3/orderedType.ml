module type OrderedType = 
sig
  type t
  type order = LT | EQ | GT
  val compare : t -> t -> order 
end