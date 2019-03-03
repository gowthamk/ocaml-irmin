let n = 1099511600L

let rec loop i acc = 
  if Int64.compare i n >= 0 then acc
  else loop (Int64.succ i) (acc+1);;

loop 0L 0;;
