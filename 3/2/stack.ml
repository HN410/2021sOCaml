  exception Pop_empty_stack;;
  type 'a t = 'a list
  let empty = []
  let pop st = match st with
    |[] -> raise Pop_empty_stack
    |x :: xs -> (x, xs)
  let push a st = a::st
  let rec size st = match st with
    |[] -> 0
    |x ::xs -> 1 + size xs
