exception Empty 
module type ATOM  =
  sig
    type t
    val t : t Irmin.Type.t
    val compare : t -> t -> int
    val to_string : int64 -> string
    val of_string : string -> int64
  end
(* Heap is defined as variant type, Empty and T of node where node is a record type consisting of rank of node, root, left
node and right node *)

(* Properties 
(1) Easy to retrieve minimum element 
(2) Root is always minimum.
(3) Element on the left hand side is smaller than the elements on its right hand side.
(4) Deleting is just deleting the head which is O(n) times.
(5) Inserting is O(n) times
(6) Merge is like mergesort. *)
module Make(Atom:ATOM) =
  struct
    type node = {
      l: t ;
      d: Atom.t ;
      r: t ;
      ra: int64}
    and t =
      | E 
      | T of node [@@derive versioned]

    module OS = Set_imp.Make(Atom)

    let rec t_to_set h = match h with 
      | E -> OS.Empty 
      | T {ra =_; d= n; l= E; r= E} -> OS.singleton n
      | T {ra=_; d= n; l= lt; r=rt} -> 
        let ls = t_to_set lt in 
        let rs = t_to_set rt in 
        OS.union (OS.union (OS.singleton n) ls) rs

    (* Rank of node which is length of the path between the node and the right most leaf *)
    let rank = function | E -> Int64.of_int 0 | T { ra;_} -> ra

    let makeT x a b =
      if (rank a) >= (rank b)
      then T { ra = Int64.of_int (Int64.to_int (rank b) + 1); d = x; l = a; r = b }
      else T { ra = Int64.of_int (Int64.to_int (rank a) + 1); d = x; l = b; r = a }
    

    let empty = E

    let is_empty h = h = E
    
    (* merge h1 h2 merges two trees h1 and h2 where suppose h1 is (r1, x, l1, r1) and h2 is (r2, x, l2, r2) 
       (1) Comapre two roots:
           if x > y : switch two trees and perform merge h2 h1. This ensures trees on the left has smaller keys
                      since h2 is the smaller key so root of h2 will become the new root.
                      since we know right branch is always shortest, we then merge h1_right h2
                      continue till one of the tree becomes leaf and return the other in this case
                      Update rank *) 
   let rec merge h1 h2 = 
    match h1, h2 with 
     | E, _ -> h2 
     | _ , E -> h1 
     | (T { ra = r1; d = x; l = a1; r = b1 }, T
         { ra =_; d = y; l = _; r = _ }) ->
        if (Atom.compare x y) > 0 then merge h2 h1 
      else 
        let merged = merge b1 h2 in 
        let rank_left = rank a1 and rank_right = rank merged in 
        if rank_left >= rank_right then T{ra= Int64.of_int (Int64.to_int (rank_right) +1); d=x; l = a1; r= merged} 
                                   else T{ra= Int64.of_int (Int64.to_int (rank_left) +1); d=x; l = merged; r= a1}

      (* gives the root and then merge the rest *)
    let pop_min =
      function
      | E -> raise Empty
      | T { ra = _; d = x; l = a; r = b } -> (x, (merge a b))

    let rec elements h =
      if is_empty h
      then []
      else (let (min, h') = pop_min h in min :: (elements h'))

    (*let rec merge h1 h2 =
      match (h1, h2) with
      | (_, E) -> h1
      | (E, _) -> h2
      | (T { ra = _; d = x; l = a1; r = b1 }, T
         { ra = _; d = y; l = a2; r = b2 }) ->
          if (Atom.compare x y) <= 0
          then makeT x a1 (merge b1 h2)
          else makeT y a2 (merge h1 b2)*)
    (* insert x h inserts the element in the heap *)
    (* It comapres the element x with all existing element one by one and insert it in appropriate position. 
       Element on its left hand side must be smaller than it and elements on its right must be bigger *)      
    let insert x h = merge (T { ra = Int64.of_int 1; d = x; l = E; r = E }) h

    let rec set_to_t s = match s with 
      | OS.Empty -> E
      | OS.Node {l= Empty; v = n; r=Empty; h = _} -> insert n E
      | OS.Node {l=lt; v =n; r=rt; h = _} -> 
        let lh = set_to_t lt in 
        let rh = set_to_t rt in 
        insert n (merge lh rh)
    
    (* return the minimum element that will be the head : Time is O(1) *)
    let find_min =
      function | E -> raise Empty | T { ra = _; d = x; l = _; r = _ } -> x

    (* delete_min deleters the head and then merge *) 
    let delete_min =
      function
      | E -> raise Empty 
      | T { ra = _; d = _; l = a; r = b } -> merge a b

  

    let rec heap_after_delete_min h = function 
      | E -> E
      | T{ra = r; d = x; l = a; r = b} -> merge a b

    let get_leftnode = function 
      | E -> E
      | T{ra = r; d = x; l = a; r = b} -> a

    let get_rightnode = function 
      | E -> E
      | T{ra = r; d = x; l = a; r = b} -> b

      
    let rec elements h =
      if is_empty h
      then []
      else (let (min, h') = pop_min h in min :: (elements h'))

    let rec choose = function
    | E -> raise Not_found
    | T {ra=r; d=x; l=E; r=E} -> x
    | T {ra=r; d=x; l=E; r=r'} -> 
      if Random.int 2 = 0 then x else choose r'
    | T {ra=r; d=x;l=l'; r=E} -> 
      if Random.int 2 = 0 then choose l' else x
    | T {ra=r;d=x;l=l';r=r'} -> 
      (match Random.int 3 with
        | 0 -> choose l'
        | 1 -> x
        | _ -> choose r')

      (* counts empty nodes as 1 *)
  let rec size = function
    | E -> 1
    | T{ra=r; d=x; l=l'; r=r'} ->
      (size l') + 1 + (size r')

  let swap_left_right h = function 
    | E -> E
    | T{ra=r; d=x; l=l'; r=r'} -> T{ra=r; d=x; l=r'; r=l'}

    (*(* Two types of edits: Insert and Delete *)  
    type edit =
      | Insert of Atom.t 
      | Delete of Atom.t 

    (* patch is the list of edits *)
    type patch = edit list

    let edit_to_string atom_to_string =
      function
      | Insert a -> Printf.sprintf "Insert (%s)" (atom_to_string a)
      | Delete a -> Printf.sprintf "Delete (%s)" (atom_to_string a)


    let op_diff xt yt =
      let rec heap_diff hx hy =
        match (hx, hy) with
        | (E, E) -> []
        | (E, _) ->
            let (m, hy) = pop_min hy in (Insert m) :: (heap_diff hx hy)
        | (_, E) ->
            let (m, hx) = pop_min hx in (Delete m) :: (heap_diff hx hy)
        | (_, _) ->
            let a1 = find_min hx in
            let a2 = find_min hy in
            let c = Atom.compare a1 a2 in
            if c = 0
            then
              let hy = delete_min hy in
              let hx = delete_min hx in heap_diff hx hy
            else
              if c < 0
              then
                (let hx = delete_min hx in (Delete a1) :: (heap_diff hx hy))
              else
                (let hy = delete_min hy in (Insert a2) :: (heap_diff hx hy)) in
      heap_diff xt yt


    let op_transform p q = 
      let rec go xs ys = 
       match xs, ys with 
        | [],[] -> ([],[])
        | xs, [] -> (xs, [])
        | [], ys -> ([], ys)
        | x :: xs, y :: ys ->
          begin 
          match x, y with 
           | Insert nx, Insert ny -> 
             let c = Pervasives.compare nx ny in 
             if c = 0 then let (a,b) = go xs ys in 
                           (a,b)
                      else if c < 0 then let (a, b) = go xs (y::ys) in 
                                         (Insert nx :: a, b)
                                    else let (a, b) = go (x :: xs) (ys) in 
                                         (a , Insert ny :: b)
           | Delete nx, Delete ny -> 
             let (a,b) = go xs ys in 
             let c = Pervasives.compare nx ny in 
             if c=0 then (a,b)
             else if c < 0 then let (a, b) = go xs (y::ys) in 
                                (Delete nx :: a, b)
                           else let (a, b) = go (x :: xs) (ys) in 
                                (a , Delete ny :: b)
           | Delete nx, Insert ny -> 
             let c = Pervasives.compare nx ny in 
             if c = 0 
              then let (a,b) = go xs ys in (Delete ny :: Delete ny :: a, b)
              else if c < 0 then let a, b = go xs (y :: ys) in 
                                 (Delete nx :: a, b)
                            else let a, b = go (x :: xs) ys in 
                                 (a, Insert ny :: b)
           | Insert nx, Delete ny ->
             let c = Pervasives.compare nx ny in 
             if c = 0 
              then let a, b = go xs ys in 
                   (a, Delete nx :: Delete nx :: b)
              else if c < 0 then let a, b = go xs (y :: ys) in 
                                 (Insert nx :: a, b)
                            else let a, b = go (x :: xs) ys in 
                                 (a, Delete ny :: b)
          end  in 
        go p q






    (*let op_transform p q =
      let rec transform_aux xs ys =
        match (xs, ys) with
        | ([], []) -> ([], [])
        | ([], _) -> ([], ys)
        | (_, []) -> (xs, [])
        | (hx::rxs, hy::rys) ->
            let handle kx ky on_conflict =
              let c = Atom.compare kx ky in
              if c = 0
              then on_conflict ()
              else
                if c < 0
                then (let (a, b) = transform_aux rxs ys in ((hx :: a), b))
                else (let (a, b) = transform_aux xs rys in (a, (hy :: b))) in
            (match (hx, hy) with
             | (Insert x, Insert y) -> let on_conflict () = transform_aux rxs rys in
                 handle x y on_conflict
             | (Delete x, Delete y) ->  
                 let on_conflict () = transform_aux rxs rys in
                 handle x y on_conflict
             | (Insert x, Delete y) ->
                 let on_conflict () =
                   let (a, b) = transform_aux rxs rys in ((hx :: hx :: a), b) in
                 handle x y on_conflict
             | (Delete x, Insert y) ->
                 let on_conflict () =
                   let (a, b) = transform_aux rxs rys in (a, (hy :: hy :: b)) in
                 handle x y on_conflict) in
      transform_aux p q*)

    let rec apply s =
      function
      | [] -> s
      | (Insert x)::r -> let s' = insert x s in apply s' r
      | (Delete x)::r -> let s' = snd(pop_min s) in apply s' r
          (*let (xx, s') = pop_min s in let _ = assert (x = xx) in apply s' r*)*)

    let merge_time = ref 0.0

    let merge3 ~ancestor l r = 
     let a = t_to_set ancestor in 
     let ls = t_to_set l in 
     let rs = t_to_set r in 
     let ms = OS.merge3 a ls rs in 
     set_to_t ms 


  let print_int64 i = output_string stdout (string_of_int (Int64.to_int i))

  let print_heap f lst =
    let rec print_elements = function
      | E -> print_string "empty"
      | T{ra=r; d=n; l=l'; r=r'} -> 
        print_string "n";
        f n ; 
        print_newline();
        print_elements l'; 
        print_newline();
        print_elements r'
    in
    print_string "{";
    print_elements lst;
    print_string "}"


  end[@@derive_versioned ]