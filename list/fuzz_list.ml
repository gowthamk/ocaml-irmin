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

module M = Mlist.Make (Atom)
open Crowbar

type triple_data = {lca: Atom.t list; left: Atom.t list; right: Atom.t list}

type which3 = Left | Right | LCA

let triple_gen =
  fix (fun triple_gen ->
      choose
        [ const (LCA, 0, {left= []; right= []; lca= []})
        ; (* Transition *)
          map [triple_gen] (fun state ->
              match state with
              | LCA, seqno, {lca; _} ->
                  (Left, seqno, {lca; left= lca; right= lca})
                  (* common prefix *)
              | Left, seqno, d -> (Right, seqno, d)
              | _ -> state )
        ; (* Insert *)
          map [uint8; int32; triple_gen] (fun v idx state ->
              let insert l seqno =
                let idx = abs (Int32.to_int idx) mod (List.length l + 1) in
                M.insert l idx (Int64.of_int seqno, Char.chr v)
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
              let remove = function
                | [] -> []
                | l ->
                    let idx = abs (Int32.to_int idx) mod List.length l in
                    M.remove l idx
              in
              match state with
              | Left, seqno, ({left; _} as d) ->
                  (Left, seqno, {d with left= remove left})
              | Right, seqno, ({right; _} as d) ->
                  (Right, seqno, {d with right= remove right})
              | _ -> state ) ] )

type quint_data =
  { lca1: Atom.t list
  ; lca2: Atom.t list
  ; a: Atom.t list
  ; b: Atom.t list
  ; c: Atom.t list }

type which5 = LCA1 | A | LCA2 | B | C

let quint_gen =
  fix (fun quint_gen ->
      choose
        [ const (LCA1, 0, {lca1= []; lca2= []; a= []; b= []; c= []})
        ; (* Transition *)
          map [quint_gen] (fun state ->
              match state with
              | LCA1, seqno, ({lca1; _} as d) ->
                  (A, seqno, {d with a = lca1; lca2 = lca1})
                  (* common prefix *)
              | A, seqno, d -> (LCA2, seqno, d)
              | LCA2, seqno, ({lca2; _} as d) ->
                  (B, seqno, {d with b = lca2; c = lca2})
                  (* common prefix *)
              | B, seqno, d -> (C, seqno, d)
              | _ -> state )
        ; (* Insert *)
          map [uint8; int32; quint_gen] (fun v idx state ->
              let insert l seqno =
                let idx = abs (Int32.to_int idx) mod (List.length l + 1) in
                M.insert l idx (Int64.of_int seqno, Char.chr v)
              in
              match state with
              | LCA1, seqno, ({lca1; _} as d) ->
                  (LCA1, seqno + 1, {d with lca1 = insert lca1 seqno})
              | LCA2, seqno, ({lca2; _} as d) ->
                  (LCA2, seqno + 1, {d with lca2 = insert lca2 seqno})
              | A, seqno, ({a; _} as d) ->
                  (A, seqno + 1, {d with a = insert a seqno})
              | B, seqno, ({b; _} as d) ->
                  (B, seqno + 1, {d with b = insert b seqno})
              | C, seqno, ({c; _} as d) ->
                  (C, seqno + 1, {d with c = insert c seqno}))
        ; (* remove *)
          map [int32; quint_gen] (fun idx state ->
              let remove = function
                | [] -> []
                | l ->
                    let idx = abs (Int32.to_int idx) mod List.length l in
                    M.remove l idx
              in
              match state with
              | LCA1, seqno, ({lca1; _} as d) ->
                  (LCA1, seqno, {d with lca1 = remove lca1})
              | LCA2, seqno, ({lca2; _} as d) ->
                  (LCA2, seqno, {d with lca2 = remove lca2 })
              | A, seqno, ({a; _} as d) ->
                  (A, seqno, {d with a = remove a })
              | B, seqno, ({b; _} as d) ->
                  (B, seqno, {d with b = remove b })
              | C, seqno, ({c; _} as d) ->
                  (C, seqno, {d with c = remove c })) ] )

let pp_t ppf (_, _, {left= l; right= r; lca}) =
  let f (seqno, c) = pp ppf "%s:%d; " (Int64.to_string seqno) (Char.code c) in
  pp ppf "lca = [" ;
  List.iter f lca ;
  pp ppf "], l = [" ;
  List.iter f l ;
  pp ppf "], r = [" ;
  List.iter f r ;
  pp ppf "]"

let pp_q ppf (_, _, {lca1; a; lca2; b; c}) =
  let f (seqno, c) = pp ppf "%s:%d; " (Int64.to_string seqno) (Char.code c) in
  pp ppf "lca1 = [" ;
  List.iter f lca1 ;
  pp ppf "], a = [" ;
  List.iter f a ;
  pp ppf "], lca2 = [" ;
  List.iter f lca2 ;
  pp ppf "], b = [" ;
  List.iter f b ;
  pp ppf "], c = [" ;
  List.iter f c ;
  pp ppf "]"


let print_list prefix l =
  let f (seqno, c) =
    Printf.printf "%s:%d; " (Int64.to_string seqno) (Char.code c)
  in
  print_string (prefix ^ "[") ;
  List.iter f l ;
  print_endline "]"

let triple_gen = with_printer pp_t triple_gen

let quint_gen = with_printer pp_q quint_gen

let _ =
  add_test ~name:"commutativity" [triple_gen]
    (fun (_, _, {lca; left; right}) ->
      (*     print_endline "----"; *)
      (*     print_list "lca = " lca; *)
      (*     print_list "left = " left; *)
      (*     print_list "right = " right; *)
      let m1 = M.merge lca left right in
      (*     print_list "m1 = " m1; *)
      let m2 = M.merge lca right left in
      (*     print_list "m2 = " m2; *)
      check (m1 = m2) )

let _ =
  add_test ~name:"associativity" [quint_gen]
    (fun (_,_,{lca1; a; lca2; b; c}) ->
      (* lca1 = LCA(a,lca2) && lca2 = LCA(b,c) *)
      let mx = M.merge lca1 a b in
      print_list "mx = " mx;
      let m1 = M.merge lca1 mx c in
      print_list "m1 = " m1;
      let my = M.merge lca2 b c in
      print_list "my = " my;
      let m2 = M.merge lca1 a my in
      print_list "m2 = " m2;
      check (m1 = m2))

let _ =
  add_test ~name:"idempotency" [triple_gen]
    (fun (_,_,{lca;left;right}) ->
      let m1 = M.merge lca left right in
      let m2 = M.merge lca m1 right in
      check (m1 = m2))

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
      (*     print_endline "----"; *)
      (*     print_list "lca = " lca; *)
      (*     print_list "left = " left; *)
      (*     print_list "right = " right; *)
      let merge = M.merge lca left right in
      (*     print_list "merge = " merge; *)
      verify_relational_property S.equal S.diff S.union S.of_list lca left
        right merge )

let _ =
  let module OB = Set.Make (struct
    type t = Atom.t * Atom.t

    let compare (x1, x2) (y1, y2) =
      let r = Atom.compare x1 y1 in
      if not (r = 0) then r else Atom.compare x2 y2
  end) in
  let generate_ordered_before merge l =
    let smerge = S.of_list merge in
    let rec loop = function
      | [] -> OB.empty
      | x :: xs when S.mem x smerge ->
          let s =
            List.fold_left
              (fun s e -> if S.mem e smerge then OB.add (x, e) s else s)
              OB.empty xs
          in
          OB.union s (loop xs)
      | _ :: xs -> loop xs
    in
    loop l
  in
  add_test ~name:"ordering" [triple_gen] (fun (_, _, {lca; left; right}) ->
      (*     print_endline "--ordering--"; *)
      (*     print_list "lca = " lca; *)
      (*     print_list "left = " left; *)
      (*     print_list "right = " right; *)
      let merge = M.merge lca left right in
      (*     print_list "merge = " merge; *)
      verify_relational_property OB.subset OB.diff OB.union
        (generate_ordered_before merge)
        lca left right merge )
