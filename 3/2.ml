exception Pop_empty_stack;;

module Stack = 
struct 
  type 'a t = 'a list
  let empty = []
  let pop st = match st with
    |[] -> raise Pop_empty_stack
    |x :: xs -> (x, xs)
  let push a st = a::st
  let rec size st = match st with
    |[] -> 0
    |x ::xs -> 1 + size xs
end

module type STACK = 
sig
  type 'a t
  val empty : 'a t
  val pop : 'a t -> ('a * 'a t)
  val push : 'a -> 'a t -> 'a t
  val size : 'a t -> int
end

module IntStack : STACK = Stack;;
