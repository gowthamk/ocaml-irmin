(* original = | |
   q1 = |5|
   q2 = |5||6| *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end



(* Queue *)
let _ =
  U.print_header "Heap1";
  let module Atom = struct
  type t = int64
  let t = Irmin.Type.int64
  let compare x y = Int64.to_int @@ Int64.sub x y
  let to_string = Int64.to_string
  let of_string = Int64.of_string
end  in 

  let module H = Heap_leftlist.Make(Atom) in

  let original = H.empty |> H.insert (Int64.of_int 1)  in 
  let q1 =  original |> H.delete_min |> H.insert (Int64.of_int 5) |> H.insert (Int64.of_int 6)  in 
  let s1 = H.t_to_set q1 in 
  H.OS.print_set H.print_int64 s1;

