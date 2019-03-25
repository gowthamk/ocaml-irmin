module C = Canvas.Make
open Crowbar

type triple_data = {lca: C.canvas; left: C.canvas; right: C.canvas}

type which3 = Left | Right | LCA

let triple_gen =
  fix (fun triple_gen ->
      choose
        [ map [uint8; uint8] (fun x y ->
              let c = C.new_canvas (x mod 2 +1) (y mod 2 +1) in
              (LCA, {left= c; right= c; lca= c}) )
        ; (* Transition *)
          map [triple_gen] (fun state ->
              match state with
              | LCA, {lca; _} ->
                  (Left, {lca; left= lca; right= lca}) (* common prefix *)
              | Left, d -> (Right, d)
              | _ -> state )
        ; (* Update *)
          map [uint8; uint8; uint8; uint8; uint8; triple_gen]
            (fun r g b x y state ->
              let update ({C.max_x; max_y; _} as c) =
                let x = x mod max_x in
                let y = y mod max_y in
                C.set_px c {x; y}
                  {r= Int32.of_int r; g= Int32.of_int g; b= Int32.of_int b}
              in
              match state with
              | LCA, ({lca; _} as d) -> (LCA, {d with lca= update lca})
              | Left, ({left; _} as d) -> (Left, {d with left= update left})
              | Right, ({right; _} as d) ->
                  (Right, {d with right= update right}) ) ] )

type quint_data =
  {lca1: C.canvas; lca2: C.canvas; a: C.canvas; b: C.canvas; c: C.canvas}

type which5 = LCA1 | A | LCA2 | B | C

let quint_gen =
  fix (fun quint_gen ->
      choose
        [ map [uint8; uint8] (fun x y ->
              let c = C.new_canvas ((x mod 2) + 1) ((y mod 2) + 1) in
              (LCA1, {lca1= c; lca2 = c; a = c; b = c; c = c}) )
        ; (* Transition *)
          map [quint_gen] (fun state ->
              match state with
              | LCA1, ({lca1; _} as d) ->
                  (A, {d with a= lca1; lca2= lca1}) (* common prefix *)
              | A, d -> (LCA2, d)
              | LCA2, ({lca2; _} as d) ->
                  (B, {d with b= lca2; c= lca2}) (* common prefix *)
              | B, d -> (C, d)
              | _ -> state )
        ; (* Insert *)
          map [uint8; uint8; uint8; uint8; uint8; quint_gen]
            (fun r g b x y state ->
              let update ({C.max_x; max_y; _} as c) =
                let x = x mod max_x in
                let y = y mod max_y in
                C.set_px c {x; y}
                  {r= Int32.of_int r; g= Int32.of_int g; b= Int32.of_int b}
              in
              match state with
              | LCA1, ({lca1; _} as d) ->
                  (LCA1, {d with lca1= update lca1})
              | LCA2, ({lca2; _} as d) ->
                  (LCA2, {d with lca2= update lca2 })
              | A, ({a; _} as d) ->
                  (A, {d with a= update a })
              | B, ({b; _} as d) ->
                  (B, {d with b= update b })
              | C, ({c; _} as d) ->
                  (C, {d with c= update c }) )
        ] )

let pp_t ppf (_, {left= l; right= r; lca}) =
  pp ppf "lca = [" ;
  C.print ppf lca ;
  pp ppf "], l = [" ;
  C.print ppf l ;
  pp ppf "], r = [" ;
  C.print ppf r ;
  pp ppf "]"

let pp_q ppf (_, {lca1; a; lca2; b; c}) =
  pp ppf "lca1 = [" ;
  C.print ppf lca1 ;
  pp ppf "], a = [" ;
  C.print ppf a ;
  pp ppf "], lca2 = [" ;
  C.print ppf lca2 ;
  pp ppf "], b = [" ;
  C.print ppf b ;
  pp ppf "], c = [" ;
  C.print ppf c ;
  pp ppf "]"

let triple_gen = with_printer pp_t triple_gen

let quint_gen = with_printer pp_q quint_gen

let _ =
  add_test ~name:"commutativity" [triple_gen]
    (fun (_, {lca; left; right}) ->
      let m1 = C.merge_canvas lca left right in
      let m2 = C.merge_canvas lca right left in
      check (m1 = m2) )

let _ =
  add_test ~name:"associativity" [quint_gen]
    (fun (_, {lca1; a; lca2; b; c}) ->
      flush stdout;
      (* lca1 = LCA(a,lca2) && lca2 = LCA(b,c) *)
      let mx = C.merge_canvas lca1 a b in
      (*       print_list "mx = " mx; *)
      let m1 = C.merge_canvas lca2 mx c in
      (*       print_list "m1 = " m1; *)
      let my = C.merge_canvas lca2 b c in
      (*       print_list "my = " my; *)
      let m2 = C.merge_canvas lca1 a my in
      (*       print_list "m2 = " m2; *)
      check (m1 = m2) )

let _ =
  add_test ~name:"idempotency" [triple_gen] (fun (_, {lca; left; right}) ->
      let m1 = C.merge_canvas lca left right in
      let m2 = C.merge_canvas lca m1 right in
      check (m1 = m2) )
