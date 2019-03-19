module IntAtom = struct
  type t = int
    let compare = Pervasives.compare
    (* User defined merges for atom values *)
    let resolve x y = '#'
    let merge3 ~ancestor x y = '#'

    (* Used for presentation purposes *)
    let to_string = string_of_int
  end

module M = Queue_imp.Make(IntAtom)

open Crowbar

let q_gen =
  fix (fun q_gen ->
    choose [
      map [range 100; q_gen] (fun i q -> M.push i q);
      map [range 100; q_gen] (fun i q -> M.q_after_take q);
      const M.empty])

let _ =
  add_test ~name:"commutativity" [q_gen; q_gen; q_gen] (fun lca a b ->
    print_endline "--start--";
    print_string "lca="; M.print_list (M.to_list lca); print_endline "";
    print_string "a="; M.print_list (M.to_list a); print_endline "";
    print_string "b="; M.print_list (M.to_list b); print_endline "";
    let l = M.merge3 lca a b in
    print_string "merge lca a b="; M.print_list (M.to_list l); print_endline "";
    let r = M.merge3 lca b a in
    print_string "merge lca b a="; M.print_list (M.to_list r); print_endline "";
    print_endline "--end--";
    check (M.to_list l = M.to_list r))
