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

  let original = H.empty |> H.insert (Int64.of_int 2) |> H.insert (Int64.of_int 3) |> H.insert (Int64.of_int 4)  in 
  let q1 =  original |> H.delete_min |> H.insert (Int64.of_int 5) |> H.delete_min  in 
  let q2 = original |> H.delete_min  |> H.insert (Int64.of_int 1) |> H.delete_min in 
  (* Edit seq generation demonstration *)
  let edit_seq_printer = U.string_of_list (H.edit_to_string Atom.to_string) in 
  (* edit seq generation with diff *)
  let p = H.op_diff original q1 in
  let q = H.op_diff original q2 in
  let _ = Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p);
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q) in
  let p', q' = H.op_transform p q in
  let _ = 
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p');
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in 
  let m = H.merge3 original q1 q2 in 
  H.print_heap H.print_int64 m;
  print_newline();
  print_float !H.merge_time
