  type 'a t
  val empty : 'a t
  val pop : 'a t -> ('a * 'a t)
  val push : 'a -> 'a t -> 'a t
  val size : 'a t -> int
