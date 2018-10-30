(**************************************************************************)
(*                                                                        *)
(*   Xavier Leroy, projet Cristal, INRIA Rocquencourt                     *)
(*                                                                        *)
(*   Copyright 1996 INRIA                                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file CORE-LICENSE.txt  *)
(*                                                                        *)
(**************************************************************************)

module type KEY = Mmap.KEY
module type ATOM = Mmap.ATOM

module Make (Key: KEY) (Atom: ATOM) (* : Mmap.S *) = 
struct
  type key = Key.t
  type atom = Atom.t
  let compare = Pervasives.compare

  type node = {l:t; v:key; d:atom; r:t; h:int}
  and t =
      Empty
    | Node of node 

  let height = function
      Empty -> 0
    | Node {h; _} -> h

  let create l x d r =
    let hl = height l and hr = height r in
    Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)}

  let singleton x d = Node{l=Empty; v=x; d; r=Empty; h=1}

  let bal l x d r =
    let hl = match l with Empty -> 0 | Node {h; _} -> h in
    let hr = match r with Empty -> 0 | Node {h; _} -> h in
    if hl > hr + 2 then begin
      match l with
        Empty -> invalid_arg "Map.bal"
      | Node{l=ll; v=lv; d=ld; r=lr; _} ->
        if height ll >= height lr then
          create ll lv ld (create lr x d r)
        else begin
          match lr with
            Empty -> invalid_arg "Map.bal"
          | Node{l=lrl; v=lrv; d=lrd; r=lrr; _}->
            create (create ll lv ld lrl) lrv lrd (create lrr x d r)
        end
    end else if hr > hl + 2 then begin
      match r with
        Empty -> invalid_arg "Map.bal"
      | Node{l=rl; v=rv; d=rd; r=rr; _} ->
        if height rr >= height rl then
          create (create l x d rl) rv rd rr
        else begin
          match rl with
            Empty -> invalid_arg "Map.bal"
          | Node{l=rll; v=rlv; d=rld; r=rlr; _} ->
            create (create l x d rll) rlv rld (create rlr rv rd rr)
        end
    end else
      Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)}

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  (* add functions adds the element data with key x *)
  let rec add x data = function
      Empty ->
      Node{l=Empty; v=x; d=data; r=Empty; h=1}
    | Node {l; v; d; r; h} as m ->
      let c = Key.compare x v in
      if c = 0 then
        if d == data then m else Node{l; v=x; d=data; r; h}
      else if c < 0 then
        let ll = add x data l in
        if l == ll then m else bal ll v d r
      else
        let rr = add x data r in
        if r == rr then m else bal l v d rr
  
  (* find x finds the element with key x *)
  let rec find x = function
      Empty ->
      raise Not_found
    | Node {l; v; d; r; _} ->
      let c = Key.compare x v in
      if c = 0 then d
      else find x (if c < 0 then l else r)

  let rec find_first_aux v0 d0 f = function
      Empty ->
      (v0, d0)
    | Node {l; v; d; r; _} ->
      if f v then
        find_first_aux v d f l
      else
        find_first_aux v0 d0 f r

  let rec find_first f = function
      Empty ->
      raise Not_found
    | Node {l; v; d; r; _} ->
      if f v then
        find_first_aux v d f l
      else
        find_first f r

  let rec find_first_opt_aux v0 d0 f = function
      Empty ->
      Some (v0, d0)
    | Node {l; v; d; r; _} ->
      if f v then
        find_first_opt_aux v d f l
      else
        find_first_opt_aux v0 d0 f r

  let rec find_first_opt f = function
      Empty ->
      None
    | Node {l; v; d; r; _} ->
      if f v then
        find_first_opt_aux v d f l
      else
        find_first_opt f r

  let rec find_last_aux v0 d0 f = function
      Empty ->
      (v0, d0)
    | Node {l; v; d; r; _} ->
      if f v then
        find_last_aux v d f r
      else
        find_last_aux v0 d0 f l

  let rec find_last f = function
      Empty ->
      raise Not_found
    | Node {l; v; d; r; _} ->
      if f v then
        find_last_aux v d f r
      else
        find_last f l

  let rec find_last_opt_aux v0 d0 f = function
      Empty ->
      Some (v0, d0)
    | Node {l; v; d; r; _} ->
      if f v then
        find_last_opt_aux v d f r
      else
        find_last_opt_aux v0 d0 f l

  let rec find_last_opt f = function
    | Empty -> None
    | Node {l; v; d; r; _} ->
      if f v then
        find_last_opt_aux v d f r
      else
        find_last_opt f l

  let rec find_opt x = function
    | Empty -> None
    | Node {l; v; d; r; _} ->
      let c = Key.compare x v in
      if c = 0 then Some d
      else find_opt x (if c < 0 then l else r)

  (* mem returns true if any element with key x is present in the tree else it returns false *)
  let rec mem x = function
    | Empty -> false
    | Node {l; v; r; _} ->
      let c = Key.compare x v in
      c = 0 || mem x (if c < 0 then l else r)

  let rec min_binding = function
    | Empty -> raise Not_found
    | Node {l=Empty; v; d; _} -> (v, d)
    | Node {l; _} -> min_binding l

  let rec min_binding_opt = function
    | Empty -> None
    | Node {l=Empty; v; d; _} -> Some (v, d)
    | Node {l; _}-> min_binding_opt l

  let rec max_binding = function
    | Empty -> raise Not_found
    | Node {v; d; r=Empty; _} -> (v, d)
    | Node {r; _} -> max_binding r

  let rec max_binding_opt = function
    | Empty -> None
    | Node {v; d; r=Empty; _} -> Some (v, d)
    | Node {r; _} -> max_binding_opt r

  let rec remove_min_binding = function
    | Empty -> invalid_arg "Map.remove_min_elt"
    | Node {l=Empty; r; _} -> r
    | Node {l; v; d; r; _} -> bal (remove_min_binding l) v d r

  let merge t1 t2 =
    match (t1, t2) with
    | (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) ->
      let (x, d) = min_binding t2 in
      bal t1 x d (remove_min_binding t2)

  let rec remove x = function
    | Empty -> Empty
    | (Node {l; v; d; r; _} as m) ->
      let c = Key.compare x v in
      if c = 0 then merge l r
      else if c < 0 then
        let ll = remove x l in if l == ll then m else bal ll v d r
      else
        let rr = remove x r in if r == rr then m else bal l v d rr

  let rec update x f = function
    | Empty ->
      begin match f None with
        | None -> Empty
        | Some data -> Node{l=Empty; v=x; d=data; r=Empty; h=1}
      end
    | Node {l; v; d; r; h} as m ->
      let c = Key.compare x v in
      if c = 0 then begin
        match f (Some d) with
        | None -> merge l r
        | Some data ->
          if d == data then m else Node{l; v=x; d=data; r; h}
      end else if c < 0 then
        let ll = update x f l in
        if l == ll then m else bal ll v d r
      else
        let rr = update x f r in
        if r == rr then m else bal l v d rr

  let rec iter f = function
    | Empty -> ()
    | Node {l; v; d; r; _} ->
      iter f l; f v d; iter f r

  let rec map f = function
    | Empty -> Empty
    | Node {l; v; d; r; h} ->
      let l' = map f l in
      let d' = f d in
      let r' = map f r in
      Node{l=l'; v; d=d'; r=r'; h}

  let rec mapi f = function
    | Empty -> Empty
    | Node {l; v; d; r; h} ->
      let l' = mapi f l in
      let d' = f v d in
      let r' = mapi f r in
      Node{l=l'; v; d=d'; r=r'; h}

  let rec fold f m accu =
    match m with
    | Empty -> accu
    | Node {l; v; d; r; _} ->
      fold f r (f v d (fold f l accu))

  let rec for_all p = function
    | Empty -> true
    | Node {l; v; d; r; _} -> p v d && for_all p l && for_all p r

  let rec exists p = function
    | Empty -> false
    | Node {l; v; d; r; _} -> p v d || exists p l || exists p r

  (* Beware: those two functions assume that the added k is *strictly*
     smaller (or bigger) than all the present keys in the tree; it
     does not test for equality with the current min (or max) key.

     Indeed, they are only used during the "join" operation which
     respects this precondition.
  *)

  let rec add_min_binding k x = function
    | Empty -> singleton k x
    | Node {l; v; d; r; _} ->
      bal (add_min_binding k x l) v d r

  let rec add_max_binding k x = function
    | Empty -> singleton k x
    | Node {l; v; d; r; _} ->
      bal l v d (add_max_binding k x r)

  (* Same as create and bal, but no assumptions are made on the
     relative heights of l and r. *)

  let rec join l v d r =
    match (l, r) with
    | (Empty, _) -> add_min_binding v d r
    | (_, Empty) -> add_max_binding v d l
    | (Node{l=ll; v=lv; d=ld; r=lr; h=lh}, Node{l=rl; v=rv; d=rd; r=rr; h=rh}) ->
      if lh > rh + 2 then bal ll lv ld (join lr v d r) else
      if rh > lh + 2 then bal (join l v d rl) rv rd rr else
        create l v d r

  (* Merge two trees l and r into one.
     All elements of l must precede the elements of r.
     No assumption on the heights of l and r. *)

  let concat t1 t2 =
    match (t1, t2) with
    | (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) ->
      let (x, d) = min_binding t2 in
      join t1 x d (remove_min_binding t2)

  let concat_or_join t1 v d t2 =
    match d with
    | Some d -> join t1 v d t2
    | None -> concat t1 t2

  let rec split x = function
    | Empty -> (Empty, None, Empty)
    | Node {l; v; d; r; _} ->
      let c = Key.compare x v in
      if c = 0 then (l, Some d, r)
      else if c < 0 then
        let (ll, pres, rl) = split x l in (ll, pres, join rl v d r)
      else
        let (lr, pres, rr) = split x r in (join l v d lr, pres, rr)

  let rec merge f s1 s2 =
    match (s1, s2) with
    | (Empty, Empty) -> Empty
    | (Node {l=l1; v=v1; d=d1; r=r1; h=h1}, _) when h1 >= height s2 ->
      let (l2, d2, r2) = split v1 s2 in
      concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
    | (_, Node {l=l2; v=v2; d=d2; r=r2; _}) ->
      let (l1, d1, r1) = split v2 s1 in
      concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
    | _ -> assert false

  let rec union f s1 s2 =
    match (s1, s2) with
    | (Empty, s) | (s, Empty) -> s
    | (Node {l=l1; v=v1; d=d1; r=r1; h=h1}, Node {l=l2; v=v2; d=d2; r=r2; h=h2}) ->
      if h1 >= h2 then
        let (l2, d2, r2) = split v1 s2 in
        let l = union f l1 l2 and r = union f r1 r2 in
        match d2 with
        | None -> join l v1 d1 r
        | Some d2 -> concat_or_join l v1 (f v1 d1 d2) r
      else
        let (l1, d1, r1) = split v2 s1 in
        let l = union f l1 l2 and r = union f r1 r2 in
        match d1 with
        | None -> join l v2 d2 r
        | Some d1 -> concat_or_join l v2 (f v2 d1 d2) r

  let rec filter p = function
    | Empty -> Empty
    | Node {l; v; d; r; _} as m ->
      (* call [p] in the expected left-to-right order *)
      let l' = filter p l in
      let pvd = p v d in
      let r' = filter p r in
      if pvd then if l==l' && r==r' then m else join l' v d r'
      else concat l' r'

  let rec partition p = function
    | Empty -> (Empty, Empty)
    | Node {l; v; d; r; _} ->
      (* call [p] in the expected left-to-right order *)
      let (lt, lf) = partition p l in
      let pvd = p v d in
      let (rt, rf) = partition p r in
      if pvd
      then (join lt v d rt, concat lf rf)
      else (concat lt rt, join lf v d rf)

  type enumeration = End | More of key * atom * t * enumeration

  let rec cons_enum m e =
    match m with
    | Empty -> e
    | Node {l; v; d; r; _} -> cons_enum l (More(v, d, r, e))

  let compare cmp m1 m2 =
    let rec compare_aux e1 e2 =
      match (e1, e2) with
      | (End, End) -> 0
      | (End, _)  -> -1
      | (_, End) -> 1
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
        let c = Key.compare v1 v2 in
        if c <> 0 then c else
          let c = cmp d1 d2 in
          if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in compare_aux (cons_enum m1 End) (cons_enum m2 End)

  let equal m1 m2 =
    let rec equal_aux e1 e2 =
      match (e1, e2) with
      | (End, End) -> true
      | (End, _)  -> false
      | (_, End) -> false
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
        Key.compare v1 v2 = 0 && Atom.equal d1 d2 &&
        equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in equal_aux (cons_enum m1 End) (cons_enum m2 End)

  let rec cardinal = function
    | Empty -> 0
    | Node {l; r; _} -> cardinal l + 1 + cardinal r

  let rec bindings_aux accu = function
    | Empty -> accu
    | Node {l; v; d; r; _} -> bindings_aux ((v, d) :: bindings_aux accu r) l

  let bindings s =
    bindings_aux [] s

  let choose = min_binding

  let choose_opt = min_binding_opt

    let rec elements_aux accu = function
      Empty -> accu
    | Node{l; v; d; r; _} -> elements_aux ((v, d) :: elements_aux accu r) l

  let elements s =
    elements_aux [] s

  (* Patching *)
  type edit = 
    | Add of key * atom
    | Remove of key
    | Replace of key * atom * atom
  type patch = edit list

  let edit_to_string key_to_string atom_to_string = function
    | Add (k, a) -> Printf.sprintf "Add (%s, %s)" (key_to_string k) (atom_to_string a)
    | Remove (k) -> Printf.sprintf "Remove (%s)" (key_to_string k)
    | Replace (k, a, b) -> Printf.sprintf "Rep (%s, %s, %s)" (key_to_string k) (atom_to_string a) (atom_to_string b)

  let op_diff xt yt = 
    (* TODO: Use reverse appends for faster list manipulations *)
    let rec diff_avltree s1 s2 =
      match (s1, s2) with
      | (Empty, s) -> fold (fun k x y ->  y @ [Add (k,x)]) s []
      | (s, Empty) -> fold (fun k _ y -> y @ [Remove k]) s []
      | (Node {l=l1; v=v1; d=d1; r=r1; h=h1}, Node {l=l2; v=v2; d=d2; r=r2; h=h2}) ->
        if h1 >= h2 then
          let (l2, d2, r2) = split v1 s2 in
          let l = diff_avltree l1 l2 in 
          let r = diff_avltree r1 r2 in
          match d2 with
          | None -> List.append l (Remove v1 :: r)
          | Some d2 ->
            if Atom.equal d1 d2 then
              List.append l r
            else
              List.append l (Replace (v1, d1, d2) :: r)
        else
          let (l1, d1, r1) = split v2 s1 in
          let l = diff_avltree l1 l2 in 
          let r = diff_avltree r1 r2 in
          match d1 with
          | None -> List.append l (Add (v2, d2) :: r)
          | Some d1 ->
            if Atom.equal d1 d2 then
              List.append l r
            else
              List.append l (Replace (v1, d1, d2) :: r)
    in
    diff_avltree xt yt

  let op_transform p q = 
    let rec transform_aux xs ys =
      match xs, ys with
      | [], [] -> [], []
      | [], _ -> [], ys
      | _, [] -> xs, []   
      | hx::rxs, hy::rys ->
        let handle kx ky on_conflict =
          let c = Key.compare kx ky in
          if c = 0 then on_conflict ()
          else if c < 0 then 
            let a, b = transform_aux rxs ys in
            hx::a, b
          else (* c > 0 *)
            let a, b = transform_aux xs rys in
            a, hy::b in
        match hx, hy with
        | Add (kx, x), Add (ky, y) ->
          let on_conflict () =
            if Atom.equal x y then
              transform_aux rxs rys
            else
              let m = Atom.resolve x y in
              let a, b = transform_aux rxs ys in
              Add (kx, m)::a, Add(ky, m)::b in
          handle kx ky on_conflict
        | Add (kx, _), Replace (ky, _, _) ->
          let on_conflict = fun () -> assert false in
          handle kx ky on_conflict
        | Add (kx, _), Remove ky ->
          let on_conflict = fun () -> assert false in
          handle kx ky on_conflict
        | Replace (kx, _, _), Add (ky, _) ->
          let on_conflict = fun () -> assert false in
          handle kx ky on_conflict
        | Replace (kx, ax, x), Replace (ky, ay, y) ->
          let on_conflict () =
            if Atom.equal x y then
              transform_aux rxs rys
            else
              let m = Atom.merge3 ~ancestor:ax x y in
              let a, b = transform_aux rxs rys in
              Replace (kx, ax, m)::a, Replace (ky, ay, m)::b in
          handle kx ky on_conflict
        | Replace (kx, _, x), Remove ky ->
          let on_conflict () =
            let a, b = transform_aux rxs rys in
            a, Add (kx,x)::b in
          handle kx ky on_conflict
        | Remove kx, Add (ky, _) ->
          let on_conflict = fun () -> assert false in
          handle kx ky on_conflict
        | Remove kx, Replace (ky, _, y) ->
          let on_conflict () =
            let a, b = transform_aux rxs rys in
            Add (ky, y)::a, b in
          handle kx ky on_conflict
        | Remove kx, Remove ky ->
          let on_conflict () = transform_aux rxs rys in
          handle kx ky on_conflict
    in
    transform_aux p q

  (* Merging *)
  let resolve = 
    let aux _ x y = Some (Atom.resolve x y) in
    union aux

  let rec apply s = function
    | [] -> s
    | Add (k, x)::r -> let s' = add k x s in apply s' r
    | Remove (k)::r -> let s' = remove k s in apply s' r
    | Replace (k, _, x)::r -> let s' = add k x s in apply s' r

  let merge3 ~ancestor l r =
    let p = op_diff ancestor l in
    let q = op_diff ancestor r in
    let _,q' = op_transform p q in
    apply l q'
end