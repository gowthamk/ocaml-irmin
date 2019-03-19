module IntAtom = struct
  type t = char
  let t = Irmin.Type.char
  let to_string c = String.make 1 c
  let of_string s = String.get s 0
  let compare = Char.compare
end

module M = Mlist.Make(IntAtom)
module S = Set.Make(struct
  type t = char
  let compare = Pervasives.compare
end)

open Crowbar

type dedup_list = S.t * char list
type merge_data = {left: dedup_list; right: dedup_list; lca: dedup_list}

type which = Left | Right | LCA [@@deriving crowbar]

let lca_gen =
  fix (fun lca_gen ->
    choose [
      begin
        let z = (S.empty, []) in
        const {left = z; right = z; lca = z}
      end;
      map [range 256; which_to_crowbar; lca_gen] (fun i w d ->
        let c = Char.chr i in
        match w,d with
        | Left, {left = (s, l); _} ->
            if S.mem c s then d else {d with left = S.add c s, c::l}
        | Right, {right = (s, l); _} ->
            if S.mem c s then d else {d with right = S.add c s, c::l}
        | LCA , {lca= (s, l); _} ->
            if S.mem c s then d else {d with lca = S.add c s, c::l}

      )
    ])

let print_list l =
  print_string "[";
  List.iter (fun e -> print_string (String.make 1 e ^ "; ")) l;
  print_endline "]"

let _ =
  add_test ~name:"commutativity" [lca_gen] (fun {left = _,l; right = _,r; lca = _,lca} ->
    check (M.merge lca l r = M.merge lca r l))
