(* Utility functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end

(* Queue *)
let _ =
  U.print_header "Vector - List";
  let module IntAtom = struct
    type t = int

    (* User defined merges for atom values *)
    let resolve x y = '#'
    let merge3 ~ancestor x y = '#'

    (* Used for presentation purposes *)
    let to_string = string_of_int
  end in

  let module M = Queue_imp.Make(IntAtom) in

  let a = M.empty |> M.add 4 |> M.add 3 |> M.add 2 |> M.add 1  in  
  M.iter (fun x -> print_int x; print_newline()) a;
  print_int (M.length a);
  print_newline () ;
  print_int (M.take a);


