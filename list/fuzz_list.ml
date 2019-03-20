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
      map [uint8; int32; triple_gen] (fun v idx state ->
        let insert l seqno =
          let idx = abs (Int32.to_int idx) mod (List.length l + 1) in
          M.insert l idx (Int64.of_int seqno, Char.chr v)
        in
        match state with
        | LCA, seqno, ({lca;_} as d) ->
            LCA, seqno+1, {d with lca = insert lca seqno}
        | Left, seqno, ({left;_} as d) ->
            Left, seqno+1, {d with left = insert left seqno}
        | Right, seqno, ({right;_} as d) ->
            Right, seqno+1, {d with right = insert right seqno});

      (* remove *)
      map [int32; triple_gen] (fun idx state ->
        let remove = function
          | [] -> []
          | l ->
            let idx = abs (Int32.to_int idx) mod List.length l in
            M.remove l idx
        in
        match state with
        | Left, seqno, ({left;_} as d) -> Left, seqno, {d with left = remove left}
        | Right, seqno, ({right;_} as d) -> Right, seqno, {d with right = remove right}
        | _ -> state)
    ])

let pp_q ppf (_,_,{left = l; right = r; lca = lca}) =
  let f (seqno,c) = pp ppf "%s:%d; " (Int64.to_string seqno) (Char.code c) in
  pp ppf "lca = [";
  List.iter f lca;
  pp ppf "], l = [";
  List.iter f l;
  pp ppf "], r = [";
  List.iter f r;
  pp ppf "]"

let print_list prefix l =
  let f (seqno,c) = Printf.printf "%s:%d; " (Int64.to_string seqno) (Char.code c) in
  print_string (prefix ^ "[");
  List.iter f l;
  print_endline "]"

let triple_gen = with_printer pp_q triple_gen

let _ =
  add_test ~name:"commutativity" [triple_gen] (fun (_,_,{lca; left; right}) ->
    print_endline "----";
    print_list "lca = " lca;
    print_list "left = " left;
    print_list "right = " right;
    let m1 = M.merge lca left right in
    print_list "m1 = " m1;
    let m2 = M.merge lca right left in
    print_list "m2 = " m2;
    check ( m1 = m2 ))

let verify_relational_property diff union subset relate lca left right merge =
  let slca, sleft, sright, smerge =
    relate lca, relate left, relate right, relate merge
  in
  let sadditions = union (diff sleft slca) (diff sright slca) in
  let sdeletions = union (diff slca sleft) (diff slca sright) in
  let smerge' = diff (union slca sadditions) sdeletions in
  check (subset smerge' smerge)

module S = Set.Make(Atom)

let _ =
  add_test ~name:"membership" [triple_gen] (fun (_,_,{lca; left; right}) ->
    print_endline "----";
    print_list "lca = " lca;
    print_list "left = " left;
    print_list "right = " right;
    let merge = M.merge lca left right in
    print_list "merge = " merge;
    verify_relational_property S.diff S.union S.subset S.of_list lca left right merge)

let _ =
  let module OB = Set.Make (struct
    type t = Atom.t * Atom.t
    let compare (x1,x2) (y1,y2) =
      let r = Atom.compare x1 y1 in
      if not (r = 0) then r else Atom.compare x2 y2
    end)
  in
  let generate_ordered_before merge l =
    let smerge = S.of_list merge in
    let rec loop = function
      | [] -> OB.empty
      | x::xs when S.mem x smerge ->
          let s = List.fold_left (fun s e ->
            if S.mem e smerge then OB.add (x,e) s else s) OB.empty xs
          in
          OB.union s (loop xs)
      | _::xs -> loop xs
    in
    loop l
  in
  add_test ~name:"ordering" [triple_gen] (fun (_,_,{lca; left; right}) ->
    print_endline "--ordering--";
    print_list "lca = " lca;
    print_list "left = " left;
    print_list "right = " right;
    let merge = M.merge lca left right in
    print_list "merge = " merge;
    verify_relational_property OB.diff OB.union OB.subset
      (generate_ordered_before merge) lca left right merge)
