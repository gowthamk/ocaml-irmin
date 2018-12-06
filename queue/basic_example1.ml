(* Utility functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end

(* Queue *)
let _ =
  U.print_header "Queue";
  let module IntAtom = struct
    type t = int

    (* User defined merges for atom values *)
    let resolve x y = 0
    let merge3 ~ancestor x y = 0
    let compare = Pervasives.compare
    (* Used for presentation purposes *)
    let to_string = string_of_int
  end in

  let module M = Queue_imp.Make(IntAtom) in

  let a = M.empty |> M.add 4 |> M.add 3 |> M.add 2 |> M.add 1  in 
  let b = M.empty |> M.add 4 |> M.add 1 in  
  M.iter (fun x -> print_string "|" ; print_int x; print_string "|") (M.take_upto_index 1 a);
  print_newline() ;
  print_int (M.length a);
  print_newline ();
  (*print_int (M.take a);*)
  print_newline() ;
  print_int (M.nthq a 0);
  Printf.printf "%B" (M.compare_till_index 0 a b) ;
  Printf.printf "%B" (M.compare_q a b) ;
  Printf.printf "%B" (M.check_common_el_list [2;3;4] [4;5;6]) ;
  print_int (M.find_el 3 [6;2;3;5]);
  M.print_list (M.prev_el_list 3 [6;2;3;5]) ;
  M.print_list (M.maintain_order_list [1;2;3;5] [7;3;1;4;5])



