(* Utility functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end

(* Queue *)
let _ =
  U.print_header "Queue";
  let module IntAtom = struct
    type t = int64
    let compare = Pervasives.compare
    (* Used for presentation purposes *)
    let t = Irmin.Type.int64
    let to_string = Int64.to_string
    let of_string = Int64.of_string
  end in

  let module M =  Queue_imp.Make(IntAtom) in

  let original = M.create () in
  let v1 = original |> M.add (Int64.of_int 10); M.add (Int64.of_int 20)   in 
  let v2 = original |> M.add (Int64.of_int 1); M.add (Int64.of_int 2) in 
  Queue.print original

  