  type t = string
  type order = LT | EQ | GT
  let compare x y = 
    let r = Pervasives.compare x y in
      if      r > 0 then GT 
      else if r < 0 then LT 
      else               EQ