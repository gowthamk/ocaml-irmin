(* original = | |
   q1 = |5|
   q2 = |5||6| *)
module U = struct
  let string_of_list f l =
    "[ " ^ List.fold_left (fun a b -> a ^ f b ^ "; ") "" l ^ "]"

  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end

module IntAtom = struct
  type t = int

  let compare = Pervasives.compare

  (* User defined merges for atom values *)
  let resolve x y = '#'

  let merge3 ~ancestor x y = '#'

  (* Used for presentation purposes *)
  let to_string = string_of_int
end

(* Queue *)
let _ =
  U.print_header "Queue" ;
  let module M = Queue_imp.Make (IntAtom) in
  let a = M.empty |> M.add 4 |> M.add 3 |> M.add 2 |> M.add 1 in
  let b = M.empty |> M.add 4 |> M.add 1 in
  M.iter
    (fun x -> print_string "|" ; print_int x ; print_string "|")
    (M.take_upto_index 1 a) ;
  print_newline () ;
  print_int (M.length a) ;
  print_newline () ;
  (*print_int (M.take a);*)
  print_newline () ;
  print_int (M.nthq a 0) ;
  Printf.printf "%B" (M.compare_till_index 0 a b) ;
  Printf.printf "%B" (M.compare_q a b) ;
  Printf.printf "%B" (M.check_common_el_list [2; 3; 4] [4; 5; 6]) ;
  print_int (M.find_el 3 [6; 2; 3; 5]) ;
  M.print_list (M.prev_el_list 3 [6; 2; 3; 5]) ;
  M.print_list (M.maintain_order_list [1; 2; 3; 5] [7; 3; 1; 4; 5])

(* Queue *)
let _ =
  U.print_header "Queue1" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = original |> M.add 5 in
  let q2 = original |> M.add 5 |> M.add 6 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original =  | |
   q1 = | |
 q2 = |1||2||3||4||5| *)

(* Queue *)
let _ =
  U.print_header "Queue2" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = M.empty in
  let q2 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 4 |> M.add 5 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1||2|
   q1 = | |
   q2 = |2| (Because queue follows FIFO order, hence we need to take out both the elements and then add 2)*)

(* Queue *)
let _ =
  U.print_header "Queue3" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 in
  let q1 = M.empty in
  let q2 = M.empty |> M.add 2 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = ||
   q1 = |1||2||3||4|
   q2 = |1||2||3||5| *)

(* Queue *)
let _ =
  U.print_header "Queue4" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = original |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 4 in
  let q2 = original |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 5 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1||2||3||4|
   q1 = |2||3||4|
   q2 = |1||2||3||5| *)

(* Queue *)
let _ =
  U.print_header "Queue5" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 4 in
  let q1 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 5 in
  let q2 = M.empty |> M.add 1 |> M.add 6 |> M.add 3 |> M.add 5 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = | |
   q1 = |1||2||3|
   q2 = |4||5||6| *)

(* Queue *)
let _ =
  U.print_header "Queue6" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  let q2 = M.empty |> M.add 4 |> M.add 5 |> M.add 6 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = | |
   q1 = |1||2||3|
   q2 = |4||5||6| *)

(* Queue *)
let _ =
  U.print_header "Queue7" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = M.empty |> M.add 1 |> M.add 2 in
  let q2 = M.empty |> M.add 1 |> M.add 2 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1|2|
   q1 = |1||2||3|
   q2 = |1| *)

(* Queue *)
let _ =
  U.print_header "Queue8" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 in
  let q1 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  let q2 = M.empty |> M.add 2 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1|2|
   q1 = |1|
   q2 = |1||2||3| *)

(* Queue *)
let _ =
  U.print_header "Queue9" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 in
  let q1 = M.empty |> M.add 2 in
  let q2 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1|2|
   q1 = |1|
   q2 = |1||2||3| *)

(* Queue *)
let _ =
  U.print_header "Queue10" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 in
  let q1 = M.empty in
  let q2 = M.empty |> M.add 1 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1||2||3||4|
   q1 = |2||3||4|
   q2 = |1||2||3||5| *)

(* Queue *)
let _ =
  U.print_header "Queue11" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 in
  let q1 = M.empty |> M.add 1 in
  let q2 = M.empty |> M.add 2 |> M.add 1 |> M.add 3 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1||2||3||4|
   q1 = |2||3||4|
   q2 = |1||2||3||5| *)

(* Queue *)
let _ =
  U.print_header "Queue12" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  let q1 = M.empty |> M.add 5 in
  let q2 = M.empty in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1||2||3|
   q1 = |1||5|
   q2 = |1||2||3| *)

(* Queue *)
let _ =
  U.print_header "Queue13" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  let q1 = M.empty |> M.add 1 |> M.add 5 in
  let q2 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1||2||3|
   q1 = |1||5|
   q2 = |1||2||3| *)

(* Queue *)
let _ =
  U.print_header "Queue14" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 in
  let q1 = M.empty |> M.add 2 |> M.add 3 |> M.add 4 in
  let q2 = M.empty |> M.add 1 |> M.add 5 |> M.add 6 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1||2||3|
   q1 = |1||5|
   q2 = |1||2||3| *)

(* Queue *)
let _ =
  U.print_header "Queue15" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 in
  let q1 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  let q2 = M.empty |> M.add 2 |> M.add 3 |> M.add 4 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1||2||3|
   q1 = |2|
   q2 = |2||1| *)

(* Queue *)
let _ =
  U.print_header "Queue16" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  let q1 = M.empty |> M.add 1 |> M.add 2 in
  let q2 = M.empty |> M.add 1 |> M.add 3 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1||2||3|
   q1 = |2|
   q2 = |2||1| *)

(* Queue *)
let _ =
  U.print_header "Queue17" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 2 in
  let q1 = M.empty |> M.add 7 |> M.add 3 |> M.add 1 in
  let q2 = M.empty |> M.add 2 |> M.add 3 |> M.add 5 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1||2||3|
   q1 = |2|
   q2 = |2||1| *)

(* Queue *)
let _ =
  U.print_header "Queue18" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 in
  let q1 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  let q2 = M.empty |> M.add 4 |> M.add 5 |> M.add 6 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1|
   q1 = |2|3|4|
   q2 = |5||6| *)

(* Queue *)
let _ =
  U.print_header "Queue19" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 in
  let q1 = M.empty |> M.add 2 |> M.add 3 |> M.add 1 in
  let q2 = M.empty |> M.add 1 |> M.add 5 |> M.add 6 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1|
   q1 = |2|3|
   q2 = |4||5| *)

(* Queue *)
let _ =
  U.print_header "Queue20" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 in
  let q1 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  let q2 = M.empty |> M.add 4 |> M.add 5 |> M.add 1 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1|
   q1 = |2|3|
   q2 = |4||5| *)

(* Queue *)
let _ =
  U.print_header "Queue21" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 in
  let q1 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 4 in
  let q2 = M.empty |> M.add 4 |> M.add 5 |> M.add 6 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1|
   q1 = |2|3|
   q2 = |4||5| *)

(* Queue *)
let _ =
  U.print_header "Queue22" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 in
  let q1 = M.empty |> M.add 2 |> M.add 3 |> M.add 1 in
  let q2 = M.empty |> M.add 1 |> M.add 4 |> M.add 5 |> M.add 3 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1|
   q1 = |2|3|
   q2 = |4||5| *)

(* Queue *)
let _ =
  U.print_header "Queue23" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  let q2 = M.empty |> M.add 4 |> M.add 5 |> M.add 1 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = |1|
   q1 = |2|3|
   q2 = |4||5| *)

(* Queue *)
let _ =
  U.print_header "Queue24" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 in
  let q1 = M.empty |> M.add 2 |> M.add 3 |> M.add 1 |> M.add 4 in
  let q2 = M.empty |> M.add 1 |> M.add 4 |> M.add 5 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  let _ =
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in
  let m = M.merge3 ~ancestor:original q1 q2 in
  M.iter (fun x -> print_string "|" ; print_int x ; print_string "|") m

(* original = | |
   q1 = |5|
   q2 = |5||6| *)

(* Queue *)
let _ =
  U.print_header "Queue" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = original |> M.add 5 in
  let q2 = original |> M.add 5 |> M.add 6 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
  Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)

(* original =  | |
   q1 = | |
 q2 = |1||2||3||4||5| *)

(* Queue *)
let _ =
  U.print_header "Queue" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = M.empty in
  let q2 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 4 |> M.add 5 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
  Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)

(* original = |1||2|
   q1 = | |
   q2 = |2| (Because queue follows FIFO order, hence we need to take out both the elements and then add 2)*)

(* Queue *)
let _ =
  U.print_header "Queue" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 in
  let q1 = M.empty in
  let q2 = M.q_after_take original in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
  Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)

(* original = ||
   q1 = |1||2||3||4|
   q2 = |1||2||3||5| *)

(* Queue *)
let _ =
  U.print_header "Queue" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = original |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 4 in
  let q2 = original |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 5 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
  Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)

(* original = |1||2||3||4|
   q1 = |2||3||4|
   q2 = |1||2||3||5| *)

(* Queue *)
let _ =
  U.print_header "Queue" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 4 in
  let q1 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 5 in
  let q2 = M.empty |> M.add 1 |> M.add 6 |> M.add 3 |> M.add 5 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
  Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)

(* original = | |
   q1 = |5|
   q2 = |5||6| *)

(* Queue *)
let _ =
  U.print_header "Queue1" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = original |> M.add 5 in
  let q2 = original |> M.add 5 |> M.add 6 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
  Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')

(* original =  | |
   q1 = | |
 q2 = |1||2||3||4||5| *)

(* Queue *)
let _ =
  U.print_header "Queue2" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = M.empty in
  let q2 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 4 |> M.add 5 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
  Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')

(* original = |1||2|
   q1 = | |
   q2 = |2| (Because queue follows FIFO order, hence we need to take out both the elements and then add 2)*)

(* Queue *)
let _ =
  U.print_header "Queue3" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 in
  let q1 = M.empty in
  let q2 = M.q_after_take original in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
  Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')

(* NOT CORRECT *)

(* original = ||
   q1 = |1||2||3||4|
   q2 = |1||2||3||5| *)

(* Queue *)
let _ =
  U.print_header "Queue4" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = original |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 4 in
  let q2 = original |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 5 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
  Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')

(* original = |1||2||3||4|
   q1 = |2||3||4|
   q2 = |1||2||3||5| *)

(* Queue *)
let _ =
  U.print_header "Queue5" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 4 in
  let q1 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 |> M.add 5 in
  let q2 = M.empty |> M.add 1 |> M.add 6 |> M.add 3 |> M.add 5 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
  Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')

(* original = | |
   q1 = |1||2||3|
   q2 = |4||5||6| *)

(* Queue *)
let _ =
  U.print_header "Queue6" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  let q2 = M.empty |> M.add 4 |> M.add 5 |> M.add 6 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
  Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')

(* original = | |
   q1 = |1||2||3|
   q2 = |4||5||6| *)

(* Queue *)
let _ =
  U.print_header "Queue7" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty in
  let q1 = M.empty |> M.add 1 |> M.add 2 in
  let q2 = M.empty |> M.add 1 |> M.add 2 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
  Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')

(* original = |1|2|
   q1 = |1||2||3|
   q2 = |1| *)

(* Queue *)
let _ =
  U.print_header "Queue8" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 in
  let q1 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  let q2 = M.empty |> M.add 2 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
  Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')

(* original = |1|2|
   q1 = |1|
   q2 = |1||2||3| *)

(* Queue *)
let _ =
  U.print_header "Queue9" ;
  let module M = Queue_imp.Make (IntAtom) in
  let original = M.empty |> M.add 1 |> M.add 2 in
  let q1 = M.empty |> M.add 2 in
  let q2 = M.empty |> M.add 1 |> M.add 2 |> M.add 3 in
  (* Edit seq generation demonstration *)
  let edit_seq_printer =
    U.string_of_list (M.edit_to_string IntAtom.to_string)
  in
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ =
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p) ;
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  let p', q' = M.op_transform p q in
  Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p') ;
  Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
