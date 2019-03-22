module type ATOM  =
  sig
    type t
    val t : t Irmin.Type.t
    val compare : t -> t -> int
    val to_string : int64 -> string
    val of_string : string -> int64
  end
module Make(Atom:ATOM) =
  struct
    type atom = Atom.t[@@derive ezjsonm]
    type node = {
      l: t ;
      v: Atom.t ;
      r: t ;
      h: int64 }
    and t =
      | Empty 
      | Node of node [@@derive versioned]
    let height = function | Empty -> Int64.of_int 0 | Node { h;_} -> h
    let create l v r =
      let hl = match l with | Empty -> Int64.of_int 0 | Node { h;_} -> h in
      let hr = match r with | Empty -> Int64.of_int 0 | Node { h;_} -> h in
      Node { l; v; r; h = (if hl >= hr then Int64.of_int (Int64.to_int hl + 1) 
      else Int64.of_int (Int64.to_int hr + 1)) }
    let bal l v r =
      let hl = match l with | Empty -> Int64.of_int 0 | Node { h;_} -> h in
      let hr = match r with | Empty -> Int64.of_int 0 | Node { h;_} -> h in
      if hl > Int64.of_int (Int64.to_int hr + 2)
      then
        match l with
        | Empty -> invalid_arg "Set.bal"
        | Node { l = ll; v = lv; r = lr;_} ->
            (if (height ll) >= (height lr)
             then create ll lv (create lr v r)
             else
               (match lr with
                | Empty -> invalid_arg "Set.bal"
                | Node { l = lrl; v = lrv; r = lrr;_} ->
                    create (create ll lv lrl) lrv (create lrr v r)))
      else
        if hr > Int64.of_int (Int64.to_int hl + 2)
        then
          (match r with
           | Empty -> invalid_arg "Set.bal"
           | Node { l = rl; v = rv; r = rr;_} ->
               if (height rr) >= (height rl)
               then create (create l v rl) rv rr
               else
                 (match rl with
                  | Empty -> invalid_arg "Set.bal"
                  | Node { l = rll; v = rlv; r = rlr;_} ->
                      create (create l v rll) rlv (create rlr rv rr)))
        else Node { l; v; r; h = (if hl >= hr then Int64.of_int (Int64.to_int hl + 1) 
      else Int64.of_int (Int64.to_int hr + 1)) }
    let rec add x =
      function
      | Empty -> Node { l = Empty; v = x; r = Empty; h = Int64.of_int 1 }
      | Node { l; v; r;_} as t ->
          let c = Atom.compare x v in
          if c = 0
          then t
          else
            if c < 0
            then (let ll = add x l in if l == ll then t else bal ll v r)
            else (let rr = add x r in if r == rr then t else bal l v rr)
    let singleton x = Node { l = Empty; v = x; r = Empty; h = Int64.of_int 1 }
    let rec add_min_element x =
      function
      | Empty -> singleton x
      | Node { l; v; r;_} -> bal (add_min_element x l) v r
    let rec add_max_element x =
      function
      | Empty -> singleton x
      | Node { l; v; r;_} -> bal l v (add_max_element x r)
    let rec join l v r =
      match (l, r) with
      | (Empty, _) -> add_min_element v r
      | (_, Empty) -> add_max_element v l
      | (Node { l = ll; v = lv; r = lr; h = lh }, Node
         { l = rl; v = rv; r = rr; h = rh }) ->
          if lh > Int64.of_int (Int64.to_int rh + 2)
          then bal ll lv (join lr v r)
          else
            if rh > Int64.of_int (Int64.to_int lh + 2) then bal (join l v rl) rv rr else create l v r
    let rec min_elt =
      function
      | Empty -> raise Not_found
      | Node { l = Empty; v;_} -> v
      | Node { l;_} -> min_elt l
    let rec min_elt_opt =
      function
      | Empty -> None
      | Node { l = Empty; v;_} -> Some v
      | Node { l;_} -> min_elt_opt l
    let rec max_elt =
      function
      | Empty -> raise Not_found
      | Node { v; r = Empty;_} -> v
      | Node { r;_} -> max_elt r
    let rec max_elt_opt =
      function
      | Empty -> None
      | Node { v; r = Empty;_} -> Some v
      | Node { r;_} -> max_elt_opt r
    let rec remove_min_elt =
      function
      | Empty -> invalid_arg "Set.remove_min_elt"
      | Node { l = Empty; r;_} -> r
      | Node { l; v; r;_} -> bal (remove_min_elt l) v r
    let merge t1 t2 =
      match (t1, t2) with
      | (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)
    let concat t1 t2 =
      match (t1, t2) with
      | (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)
    let rec split x =
      function
      | Empty -> (Empty, false, Empty)
      | Node { l; v; r;_} ->
          let c = Atom.compare x v in
          if c = 0
          then (l, true, r)
          else
            if c < 0
            then
              (let (ll, pres, rl) = split x l in (ll, pres, (join rl v r)))
            else
              (let (lr, pres, rr) = split x r in ((join l v lr), pres, rr))
    let empty = Empty
    let is_empty = function | Empty -> true | _ -> false
    let rec mem x =
      function
      | Empty -> false
      | Node { l; v; r;_} ->
          let c = Atom.compare x v in
          (c = 0) || (mem x (if c < 0 then l else r))
    let rec remove x =
      function
      | Empty -> Empty
      | Node { l; v; r;_} as t ->
          let c = Atom.compare x v in
          if c = 0
          then merge l r
          else
            if c < 0
            then (let ll = remove x l in if l == ll then t else bal ll v r)
            else (let rr = remove x r in if r == rr then t else bal l v rr)
    let rec union s1 s2 =
      match (s1, s2) with
      | (Empty, t2) -> t2
      | (t1, Empty) -> t1
      | (Node { l = l1; v = v1; r = r1; h = h1 }, Node
         { l = l2; v = v2; r = r2; h = h2 }) ->
          if h1 >= h2
          then
            (if h2 = Int64.of_int 1
             then add v2 s1
             else
               (let (l2, _, r2) = split v1 s2 in
                join (union l1 l2) v1 (union r1 r2)))
          else
            if h1 = Int64.of_int 1
            then add v1 s2
            else
              (let (l1, _, r1) = split v2 s1 in
               join (union l1 l2) v2 (union r1 r2))
    let rec inter s1 s2 =
      match (s1, s2) with
      | (Empty, _) -> Empty
      | (_, Empty) -> Empty
      | (Node { l = l1; v = v1; r = r1;_}, t2) ->
          (match split v1 t2 with
           | (l2, false, r2) -> concat (inter l1 l2) (inter r1 r2)
           | (l2, true, r2) -> join (inter l1 l2) v1 (inter r1 r2))
    let rec diff s1 s2 =
      match (s1, s2) with
      | (Empty, _) -> Empty
      | (t1, Empty) -> t1
      | (Node { l = l1; v = v1; r = r1;_}, t2) ->
          (match split v1 t2 with
           | (l2, false, r2) -> join (diff l1 l2) v1 (diff r1 r2)
           | (l2, true, r2) -> concat (diff l1 l2) (diff r1 r2))
    type enumeration =
      | End 
      | More of atom * t * enumeration 
    let rec cons_enum s e =
      match s with
      | Empty -> e
      | Node { l; v; r;_} -> cons_enum l (More (v, r, e))
    let rec compare_aux e1 e2 =
      match (e1, e2) with
      | (End, End) -> 0
      | (End, _) -> (-1)
      | (_, End) -> 1
      | (More (v1, r1, e1), More (v2, r2, e2)) ->
          let c = Atom.compare v1 v2 in
          if c <> 0
          then c
          else compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
    let compare s1 s2 = compare_aux (cons_enum s1 End) (cons_enum s2 End)
    let equal s1 s2 = (compare s1 s2) = 0
    let rec subset s1 s2 =
      match (s1, s2) with
      | (Empty, _) -> true
      | (_, Empty) -> false
      | (Node { l = l1; v = v1; r = r1;_},
         (Node { l = l2; v = v2; r = r2;_} as t2)) ->
          let c = Atom.compare v1 v2 in
          if c = 0
          then (subset l1 l2) && (subset r1 r2)
          else
            if c < 0
            then
              (subset (Node { l = l1; v = v1; r = Empty; h = Int64.of_int 0 }) l2) &&
                (subset r1 t2)
            else
              (subset (Node { l = Empty; v = v1; r = r1; h = Int64.of_int 0 }) r2) &&
                (subset l1 t2)
    let rec iter f =
      function | Empty -> () | Node { l; v; r;_} -> (iter f l; f v; iter f r)
    let rec fold f s accu =
      match s with
      | Empty -> accu
      | Node { l; v; r;_} -> fold f r (f v (fold f l accu))
    let rec for_all p =
      function
      | Empty -> true
      | Node { l; v; r;_} -> (p v) && ((for_all p l) && (for_all p r))
    let rec exists p =
      function
      | Empty -> false
      | Node { l; v; r;_} -> (p v) || ((exists p l) || (exists p r))
    let rec filter p =
      function
      | Empty -> Empty
      | Node { l; v; r;_} as t ->
          let l' = filter p l in
          let pv = p v in
          let r' = filter p r in
          if pv
          then (if (l == l') && (r == r') then t else join l' v r')
          else concat l' r'
    let rec partition p =
      function
      | Empty -> (Empty, Empty)
      | Node { l; v; r;_} ->
          let (lt, lf) = partition p l in
          let pv = p v in
          let (rt, rf) = partition p r in
          if pv
          then ((join lt v rt), (concat lf rf))
          else ((concat lt rt), (join lf v rf))
    let rec cardinal =
      function
      | Empty -> 0
      | Node { l; r;_} -> ((cardinal l) + 1) + (cardinal r)
    let rec elements_aux accu =
      function
      | Empty -> accu
      | Node { l; v; r;_} -> elements_aux (v :: (elements_aux accu r)) l
    let elements s = elements_aux [] s
    let choose = min_elt
    let choose_opt = min_elt_opt
    let rec find x =
      function
      | Empty -> raise Not_found
      | Node { l; v; r;_} ->
          let c = Atom.compare x v in
          if c = 0 then v else find x (if c < 0 then l else r)
    let rec find_first_aux v0 f =
      function
      | Empty -> v0
      | Node { l; v; r;_} ->
          if f v then find_first_aux v f l else find_first_aux v0 f r
    let rec find_first f =
      function
      | Empty -> raise Not_found
      | Node { l; v; r;_} ->
          if f v then find_first_aux v f l else find_first f r
    let rec find_first_opt_aux v0 f =
      function
      | Empty -> Some v0
      | Node { l; v; r;_} ->
          if f v then find_first_opt_aux v f l else find_first_opt_aux v0 f r
    let rec find_first_opt f =
      function
      | Empty -> None
      | Node { l; v; r;_} ->
          if f v then find_first_opt_aux v f l else find_first_opt f r
    let rec find_last_aux v0 f =
      function
      | Empty -> v0
      | Node { l; v; r;_} ->
          if f v then find_last_aux v f r else find_last_aux v0 f l
    let rec find_last f =
      function
      | Empty -> raise Not_found
      | Node { l; v; r;_} ->
          if f v then find_last_aux v f r else find_last f l
    let rec find_last_opt_aux v0 f =
      function
      | Empty -> Some v0
      | Node { l; v; r;_} ->
          if f v then find_last_opt_aux v f r else find_last_opt_aux v0 f l
    let rec find_last_opt f =
      function
      | Empty -> None
      | Node { l; v; r;_} ->
          if f v then find_last_opt_aux v f r else find_last_opt f l
    let rec find_opt x =
      function
      | Empty -> None
      | Node { l; v; r;_} ->
          let c = Atom.compare x v in
          if c = 0 then Some v else find_opt x (if c < 0 then l else r)
    let try_join l v r =
      if
        ((l = Empty) || ((Atom.compare (max_elt l) v) < 0)) &&
          ((r = Empty) || ((Atom.compare v (min_elt r)) < 0))
      then join l v r
      else union l (add v r)
    let rec map f =
      function
      | Empty -> Empty
      | Node { l; v; r;_} as t ->
          let l' = map f l in
          let v' = f v in
          let r' = map f r in
          if (l == l') && ((v == v') && (r == r'))
          then t
          else try_join l' v' r'
    let of_sorted_list l =
      let rec sub n l =
        match (n, l) with
        | (0, l) -> (Empty, l)
        | (1, x0::l) -> ((Node { l = Empty; v = x0; r = Empty; h = (Int64.of_int 1) }), l)
        | (2, x0::x1::l) ->
            ((Node
                {
                  l = (Node { l = Empty; v = x0; r = Empty; h = Int64.of_int 1 });
                  v = x1;
                  r = Empty;
                  h = Int64.of_int 2
                }), l)
        | (3, x0::x1::x2::l) ->
            ((Node
                {
                  l = (Node { l = Empty; v = x0; r = Empty; h = Int64.of_int 1 });
                  v = x1;
                  r = (Node { l = Empty; v = x2; r = Empty; h = Int64.of_int 1 });
                  h = Int64.of_int 2
                }), l)
        | (n, l) ->
            let nl = n / 2 in
            let (left, l) = sub nl l in
            (match l with
             | [] -> assert false
             | mid::l ->
                 let (right, l) = sub ((n - nl) - 1) l in
                 ((create left mid right), l)) in
      fst (sub (List.length l) l)


      let rec merge2 s1 s2 = match (s1, s2) with 
        | Empty, Empty -> Empty
        | _ , Empty -> s1
        | Empty, _ -> s2
        | (Node { l = l1; v = v1; r = r1; h = h1 }, Node
           { l = l2; v = v2; r = r2; h = h2 }) -> union s1 s2 

      let merge3 o s1 s2 = match (o,s1,s2) with 
        | o, s1 ,s2 -> 
          let is = inter s1 s2 in
          let ids1 = diff s1 s2 in
          let ids1o = diff ids1 o in
          let ids2 = diff s2 s1 in
          let ids2o = diff ids2 o in
          merge2 is (merge2 ids1o ids2o)

      let print_set f s =
      let rec print_elements =
        function
        | Empty -> ()
        | Node { l; v; r; h } ->
            (print_elements l;
             print_string ";";
             f v;
             print_string ";";
             print_elements r) in
      print_string "{"; print_elements s; print_string "}"



       let print_int64 i = output_string stdout (string_of_int (Int64.to_int i))



  end[@@derive_versioned ]