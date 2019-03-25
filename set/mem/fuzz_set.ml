module Atom = struct
  type t = int64 * char

  let t = Irmin.Type.pair Irmin.Type.int64 Irmin.Type.char

  let to_string (seqno, c) =
    Int64.to_string seqno ^ ":" ^ string_of_int (Char.code c)

  let of_string s =
    match String.split_on_char ':' s with
    | [seqno; c] -> (Int64.of_string seqno, Char.chr (int_of_string c))
    | _ -> assert false

  let compare (s1, c1) (s2, c2) =
    let r = Int64.compare s1 s2 in
    if not (r = 0) then r else Pervasives.compare c1 c2
end

module MSet = Set_imp.Make (Atom)
open Crowbar

let gen_mset =
  fix (fun gen_mset ->
      choose
        [ const (0, MSet.empty)
        ; map [uint8; gen_mset] (fun v (seqno, s) ->
              let s = MSet.add (Int64.of_int seqno, Char.chr v) s in
              (seqno + 1, s) )
        ; map [int32; gen_mset] (fun idx (seqno, s) ->
              let l = MSet.fold (fun e l -> e :: l) s [] in
              match l with
              | [] -> (seqno, s)
              | _ ->
                  let idx = abs (Int32.to_int idx) mod List.length l in
                  let e = List.nth l idx in
                  (seqno, MSet.remove e s) ) ] )

let _ =
  add_test ~name:"height" [gen_mset] (fun (_, s) ->
      match s with
      | Empty -> check true
      | Node {l; r; _} ->
          check
            ( abs (Int64.to_int (Int64.sub (MSet.height l) (MSet.height r)))
            <= 2 ) )

type merge_data = {lca: MSet.t; left: MSet.t; right: MSet.t}

type which = Left | Right | LCA

let triple_gen =
  fix (fun triple_gen ->
      choose
        [ const (LCA, 0, {left= MSet.empty; right= MSet.empty; lca= MSet.empty})
        ; (* Transition *)
          map [triple_gen] (fun state ->
              match state with
              | LCA, seqno, {lca; _} ->
                  (Left, seqno, {lca; left= lca; right= lca})
                  (* common prefix *)
              | Left, seqno, d -> (Right, seqno, d)
              | _ -> state )
        ; (* Insert *)
          map [uint8; triple_gen] (fun v state ->
              let insert s seqno =
                MSet.add (Int64.of_int seqno, Char.chr v) s
              in
              match state with
              | LCA, seqno, ({lca; _} as d) ->
                  (LCA, seqno + 1, {d with lca= insert lca seqno})
              | Left, seqno, ({left; _} as d) ->
                  (Left, seqno + 1, {d with left= insert left seqno})
              | Right, seqno, ({right; _} as d) ->
                  (Right, seqno + 1, {d with right= insert right seqno}) )
        ; (* remove *)
          map [int32; triple_gen] (fun idx state ->
              let remove s =
                let l = MSet.fold (fun e l -> e :: l) s [] in
                match l with
                | [] -> s
                | l ->
                    let idx = abs (Int32.to_int idx) mod List.length l in
                    let e = List.nth l idx in
                    MSet.remove e s
              in
              match state with
              | Left, seqno, ({left; _} as d) ->
                  (Left, seqno, {d with left= remove left})
              | Right, seqno, ({right; _} as d) ->
                  (Right, seqno, {d with right= remove right})
              | _ -> state ) ] )

let pp_s ppf (_, _, {left; right; lca}) =
  let f (seqno, c) = pp ppf "%s:%d; " (Int64.to_string seqno) (Char.code c) in
  pp ppf "lca = [" ;
  MSet.iter f lca ;
  pp ppf "], l = [" ;
  MSet.iter f left ;
  pp ppf "], r = [" ;
  MSet.iter f right ;
  pp ppf "]"

let print_set prefix l =
  let f (seqno, c) =
    Printf.printf "%s:%d; " (Int64.to_string seqno) (Char.code c)
  in
  print_string (prefix ^ "[") ;
  MSet.iter f l ;
  print_endline "]"

let triple_gen = with_printer pp_s triple_gen

let _ =
  add_test ~name:"commutativity" [triple_gen]
    (fun (_, _, {lca; left; right}) ->
      let m1 = MSet.merge3 lca left right in
      let m2 = MSet.merge3 lca right left in
      check (MSet.equal m1 m2) )

let verify_relational_property predicate diff union relate lca left right merge
    =
  let slca, sleft, sright, smerge =
    (relate lca, relate left, relate right, relate merge)
  in
  let sadditions = union (diff sleft slca) (diff sright slca) in
  let sdeletions = union (diff slca sleft) (diff slca sright) in
  let smerge' = diff (union slca sadditions) sdeletions in
  check (predicate smerge' smerge)

module S = Set.Make (Atom)

let _ =
  add_test ~name:"membership" [triple_gen] (fun (_, _, {lca; left; right}) ->
      let merge = MSet.merge3 lca left right in
      verify_relational_property MSet.equal MSet.diff MSet.union
        (fun x -> x)
        lca left right merge )

let _ =
  add_test ~name:"sortedness" [triple_gen] (fun (_, _, {lca; left; right}) ->
      let merge = MSet.merge3 lca left right in
      ignore
      @@ MSet.fold
           (fun cur prev ->
             match prev with
             | None -> Some cur
             | Some prev ->
                 check (prev <= cur) ;
                 Some cur )
           merge None )
