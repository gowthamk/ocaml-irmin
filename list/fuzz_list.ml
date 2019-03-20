module Atom = struct
  type t = int64 * char
  let t = Irmin.Type.pair Irmin.Type.int64 Irmin.Type.char
  let to_string (seqno, c) = (Int64.to_string seqno) ^ ":" ^ (string_of_int (Char.code c))
  let of_string s =
    let seqno::c::[] = String.split_on_char ':' s in
    Int64.of_string seqno, Char.chr (int_of_string c)
  let compare (s1,c1) (s2,c2) =
    let r = Int64.compare s1 s2 in
    if not (r = 0) then r else Pervasives.compare c1 c2
end

module M = Mlist.Make(Atom)

open Crowbar

type merge_data = {
  lca   : Atom.t list;
  left  : Atom.t list;
  right : Atom.t list
}

type which = Left | Right | LCA

let triple_gen =
  fix (fun triple_gen ->
    choose [
      const (LCA, 0, {left = []; right = []; lca = []});

      (* Transition *)
      map [triple_gen] (fun state ->
        match state with
        | LCA, seqno, {lca;_} -> Left, seqno, {lca;left=lca;right=lca} (* common prefix *)
        | Left, seqno, d -> Right, seqno, d
        | _ -> state);

      (* Insert *)
      map [uint8; triple_gen] (fun v state ->
        match state with
        | LCA, seqno, ({lca;_} as d) ->
            LCA, seqno+1, {d with lca = (Int64.of_int seqno, Char.chr v)::lca}
        | Left, seqno, ({left;_} as d) ->
            Left, seqno+1, {d with left = (Int64.of_int seqno, Char.chr v)::left}
        | Right, seqno, ({right;_} as d) ->
            Right, seqno+1, {d with right = (Int64.of_int seqno, Char.chr v)::right});

      (* Delete *)
      map [triple_gen] (fun state ->
        match state with
        | Left, seqno, ({left = _::xs;_} as d) -> Left, seqno, {d with left = xs}
        | Right, seqno, ({right = _::xs;_} as d) -> Right, seqno, {d with right = xs}
        | _ -> state)
    ])

let pp_q ppf {left = l; right = r; lca = lca} =
  let f (seqno,c) = pp ppf "%s:%d; " (Int64.to_string seqno) (Char.code c) in
  pp ppf "lca = [";
  List.iter f lca;
  pp ppf "], l = [";
  List.iter f l;
  pp ppf "], r = [";
  List.iter f r;
  pp ppf "]"

let print_list l =
  let f (seqno,c) = Printf.printf "%s:%d; " (Int64.to_string seqno) (Char.code c) in
  print_string "[";
  List.iter f l;
  print_endline "]"

let _ =
  add_test ~name:"commutativity" [triple_gen] (fun (_,_,{lca; left; right}) ->
(*     print_endline "----"; *)
(*     print_list lca; *)
(*     print_list left; *)
(*     print_list right; *)
    check (M.merge lca left right = M.merge lca right left))
