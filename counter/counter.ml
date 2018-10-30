module Make = struct 
type t = int

let inc t x = t + x
let dec t x = t - x

let merge ~ancestor v1 v2 = ancestor + (v1-ancestor) + (v2-ancestor)
end 
