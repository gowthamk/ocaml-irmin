(*open Lwt.Infix*)
module type ATOM =
sig 
 type t 
 (*val t: t Irmin.Type.t*)
 val compare: t -> t -> int
 val to_string: t -> string
 (*val of_string: string -> t*)
end 

module Make (Atom : ATOM) =
struct 
 type atom = Atom.t

 type node = {content: atom; mutable next: cell}  
 and cell = 
  | Nil
  | Cons of node

 exception Empty 

 type t = {mutable length: int; mutable first: cell; mutable last: cell}

 let empty = {length=0; first = Nil; last= Nil}

 (* create function creates a queue which is initially empty *)
 let create () = {length = 0; first = Nil ; last = Nil}
 
 (* clear q clears the queue where the length is assigned to 0 and first and last field are 
    assigned Nil  *)
 let clear q =  q.length <- 0 ; q.first <- Nil; q.last <- Nil
 
 (* add x q adds the element x to the queue q *) 
 let add x q = 
     let cell = Cons {content = x; next = Nil} in 
     match q.last with 
       | Nil -> {length = 1; first = cell; last = cell}
       | Cons last -> last.next <- cell; {length = q.length + 1; first = q.first; last = cell}

 (* push represents the push operation which basically pushes an element to the queue *)
 let push = add

 let nthq q n = 
     if n < 0 then invalid_arg "Queue.nthq" else 
     let rec nthq_aux q n =
       match q.first with
       | Nil -> raise Empty
       | Cons {content; next = Nil} -> if n = 0 then content else invalid_arg "Queue range"
       | Cons {content; next} -> if n = 0 then content else nthq_aux {length = q.length - 1; first = next; last = next} (n-1)
     in nthq_aux q n 


 let get q i = nthq q i

 (* peek q peeks into the queue q and returns its content which is at the top *)
 let peek q = match q.first with 
               | Nil -> raise Empty
               | Cons {content; next} -> content

 (* top returns the top element *)
 let top = peek
 
 (* take takes out the element from the queue in the FIFO order *) 
 let take q =
  match q.first with
  | Nil -> raise Empty
  | Cons { content; next = Nil } ->
    clear q;
    content
  | Cons { content; next } ->
    q.length <- q.length - 1;
    q.first <- next;
    content

  let q_after_take q = 
    match q.first with 
    | Nil -> {length=0; first= Nil; last = Nil}
    | Cons {content; next = Nil} -> {length=0; first= Nil; last = Nil}
    | Cons {content; next} -> {length = q.length-1; first = next; last = next}

    let rec equal_q q1 q2 = if ((get q1 0) = (get q2 0)) then equal_q (q_after_take q1) (q_after_take q2) else false

 (* pop pops out the element from the queue *)
 let pop =
  take
 
 (* is_empty represents the empty queue *)
 let is_empty q =
  q.length = 0
 
 (* length returns the length of the queue q *)
 let length q =
   q.length
 
 (* iter scans the queue *)
 let iter =
  let rec iter f cell =
    match cell with
    | Nil -> ()
    | Cons { content; next } ->
      f content;
      iter f next
  in
  fun f q -> iter f q.first

 let fold =
  let rec fold f accu cell =
    match cell with
    | Nil -> accu
    | Cons { content; next } ->
      let accu = f accu content in
      fold f accu next
  in
  fun f accu q -> fold f accu q.first
 
 (* transfer q1 q2 transfers the content of q1 into q2 *)
 let transfer q1 q2 =
  if q1.length > 0 then
    match q2.last with
    | Nil ->
      q2.length <- q1.length;
      q2.first <- q1.first;
      q2.last <- q1.last;
      clear q1
    | Cons last ->
      q2.length <- q2.length + q1.length;
      last.next <- q1.first;
      q2.last <- q1.last;
      clear q1

  module type S = sig
  (* Patching *)
  type edit = 
    | Add of atom 
    | Take of atom

  include Msigs.PATCHABLE with type t := t and type edit := edit

  (* Merging *)
  include Msigs.MERGEABLE with type t := t
end

  (* Patching *)

  type edit =
   | Add of atom 
   | Take of atom 


  type patch = edit list 

  let edit_to_string atom_to_string = function 
  | Add a -> Printf.sprintf "Add %s" (atom_to_string a)
  | Take a -> Printf.sprintf "Take %s" (atom_to_string a) 

  let get_edit_atom e = match e with 
  | Add nx -> nx 
  | Take nx -> nx


    let rec delete x l = match l with 
    | [] -> []
    | y :: l' -> if x = y then l' else y :: delete x l'


  let rec take_upto_index i q = if i = 0 then q_after_take q else (take_upto_index (i-1) (q_after_take q))

  let rec compare_till_index i q1 q2 = if i = 0 then (if ((get q1 0) = (get q2 0)) then true else false) else compare_till_index (i-1) (q1) (q2)

  let rec compare_q q1 q2 = match (q1, q2) with 
  | ({length=0;first=Nil; last=Nil}, {length=0;first=Nil; last=Nil}) -> true 
  |({length=0;first=Nil; last=Nil}, ys ) -> false
  | (xs, {length=0;first=Nil; last=Nil}) -> false
  | (xs, ys) -> if ((get xs 0) = (get ys 0)) then compare_q (q_after_take xs) (q_after_take ys) else false


  let rec op_diff xt yt =
   let rec diff_avlt s1 s2 =
      match (s1, s2) with
      | ({length=0;first=Nil; last=Nil}, {length=0;first=Nil; last=Nil} ) -> []
      | (xs, {length=0;first=Nil; last=Nil}) -> Take (get xs 0) :: (diff_avlt (q_after_take xs) {length=0;first=Nil; last=Nil})
      | ({length=0;first=Nil; last=Nil}, ys) -> Add (get ys 0) :: (diff_avlt {length=0;first=Nil; last=Nil} (q_after_take ys))
      | (xs, ys) -> let c = compare (length xs) (length ys) in  
                   if c > 0 then
                    (if (compare_till_index (length ys - 1) xs ys) 
                     then (List.append (Take (get xs 0) :: (diff_avlt (q_after_take xs) {length=0;first=Nil; last=Nil})) 
                          (Add (get ys 0) :: diff_avlt {length=0;first=Nil; last=Nil} (q_after_take ys)))
                     else (Take (get xs 0) :: diff_avlt (q_after_take xs) (ys)))
                  else if c = 0 then 
                      (if (compare_q xs ys) then []
                        else (List.append (Take (get xs 0) :: (diff_avlt (q_after_take xs) {length=0;first=Nil; last=Nil})) 
                             (Add (get ys 0) :: diff_avlt {length=0;first=Nil; last=Nil} (q_after_take ys))))
                    else (if (compare_till_index (length xs - 1) xs ys) 
                     then (diff_avlt {length=0;first=Nil; last=Nil} (take_upto_index (length xs -1) ys))
                     else (List.append (Take (get xs 0) :: (diff_avlt (q_after_take xs) {length=0;first=Nil; last=Nil})) 
                          (Add (get ys 0) :: diff_avlt {length=0;first=Nil; last=Nil} (q_after_take ys))))

    in
    diff_avlt xt yt


   let rec shift_patch acc = function
    | [] -> List.rev acc
    | e::tl -> shift_patch (e::acc) tl


   let offset = function
    | Add _ -> 1
    | Take _ -> -1

    (* take_all gives us the list of take edits needed to empty the queue *)
    let rec take_all x = match x with 
    | [] -> []
    | Add nx::tl -> Take nx :: take_all tl
    | Take nx :: tl -> take_all tl

    (* take_all gives us the list of take edits needed to empty the queue *)
    let rec add_all x = match x with 
    | [] -> []
    | Add nx::tl -> Add nx :: add_all tl
    | Take nx :: tl -> add_all tl

    let uniq_cons x xs = if List.mem x xs then xs else x :: xs

    let remove_from_right xs = List.fold_right uniq_cons xs []

    let cons_uniq xs x = if List.mem x xs then xs else x :: xs

    let remove_from_left xs = List.rev (List.fold_left cons_uniq [] xs)

    let rec find_el x lst =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if x = h then 0 else 1 + find_el x t


    let rec check_common_el_list x y = match (x,y) with 
     | [], [] -> true 
     | xs, [] -> false
     | [], ys -> false  
     | x' :: xs', y' :: ys' -> if x' = y' || List.mem x' (y' :: ys') then true else check_common_el_list xs' (y' :: ys')

    let rec sublist_upto_index i l = match l with 
     | [] -> raise (Failure "Not Found")
     | h :: t -> if i = 0 then [] else h :: sublist_upto_index (i-1) t 

    let prev_el_list e l = match l with 
    | [] -> []
    | x :: xs -> let i = (find_el e l) in
                 if i = 0 then []
                 else (x :: (sublist_upto_index (i-1) xs)) 

     let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

    let rec maintain_order_list l1 l2 = match (l1, l2) with 
     | [], [] -> []
     | xs, [] -> xs
     | [], ys -> ys 
     | (x :: xs), (y:: ys) -> if (List.mem x (y::ys)) then 
       (List.append (prev_el_list x (y :: ys)) 
        (List.append [x] (maintain_order_list xs (diff (y :: ys) (List.append (prev_el_list x (y :: ys)) [x])))))
       else List.append [x] (maintain_order_list xs (y :: ys))

    let rec print_list = function 
     [] -> ()
     | e::l -> print_int e ; print_string " " ; print_list l


  (* calculates the operation transform between two edit sequences *)
  let rec diff_edit x y = match (x,y) with 
    | [],[] -> []
    | xs , [] -> xs
    | [], ys -> ys 
    | Add nx :: xs, Add ny :: ys -> (let c = Atom.compare nx ny in 
                                        if c < 0 then Add nx :: Add ny :: diff_edit xs ys 
                                        else if c = 0 then Add nx :: (diff_edit xs ys)
                                        else  Add ny :: Add nx :: (diff_edit xs ys))
    | Take nx :: xs, Take ny :: ys -> if (List.mem (Add nx) ys) then 
                                      (if (List.mem (Add nx) xs) then remove_from_right(diff_edit xs ys) 
                                      else remove_from_right(diff_edit xs (delete (Add nx) ys)))
                                     else remove_from_right(diff_edit (delete (Add nx) xs) ys) 
    | x', y' when x' = y' -> []
    | Add nx :: xs, Take ny :: ys -> remove_from_right(diff_edit (Add nx :: xs) (Take ny :: ys))
    | Take nx :: xs, Add ny :: ys -> remove_from_right(diff_edit (Take nx :: ys) (Add ny :: xs))


  let rec diff_append p q = match p, q with 
  | [], [] -> ([], [])
  | xs, [] -> (xs, [])
  | [], ys -> ([], ys)
  | Add nx :: xs, Add ny :: ys -> if (check_common_el_list (Add nx :: xs) (Add ny :: ys)) then
                                       ((remove_from_right (List.append 
                                       (take_all (Add ny :: ys))
                                       (remove_from_left (maintain_order_list (add_all (Add nx :: xs)) (add_all (Add ny :: ys)))))), 
                                       ((List.append (take_all (Add nx :: xs)) 
                                        (remove_from_left (maintain_order_list (add_all (Add nx :: xs)) (add_all (Add ny :: ys))))))) else           
               ((remove_from_right (List.append 
                (take_all (Add ny :: ys))
                (diff_edit (Add nx :: xs) (Add ny :: ys)))), 
                (((remove_from_right (List.append 
                (take_all (Add nx :: xs))
                (diff_edit (Add nx :: xs) (Add ny :: ys)))))))
  | Add nx :: xs, Take ny :: ys -> if ((check_common_el_list (Add nx :: xs) (delete (Add ny) (Add ny :: ys)))) then 
             ((remove_from_right (List.append (take_all (ys))
              (remove_from_left (maintain_order_list (add_all (Add nx :: xs)) (add_all (ys)))))), 
              ((List.append (take_all (Add ny :: Add nx :: xs)) 
              (remove_from_left (maintain_order_list (add_all (Add nx :: xs)) (add_all (ys)))))))
         else    if (List.mem (Add ny) ys) then 
                (((remove_from_right
                (add_all (Add nx :: xs)))),
                (remove_from_right (List.append
                (take_all (Add ny :: Add nx :: xs))
                (List.append (add_all (ys)) (add_all (delete (Add ny) (Add nx :: xs)))))))
         else   ((delete (Add ny) (remove_from_right (List.append
                (take_all (ys))
                (diff_edit (Add ny :: Add nx :: xs) (ys))))),
                (delete (Add ny) (remove_from_right (List.append 
                (take_all (Add ny :: Add nx :: xs))
                (diff_edit (Add ny :: Add nx :: xs) (ys))))))
  | Take nx :: xs, Add ny :: ys -> if ((check_common_el_list (xs) (delete (Add nx) (Add ny :: ys)))) then 
             ((remove_from_right (List.append (take_all (Add nx :: Add ny :: ys))
              (remove_from_left (maintain_order_list (add_all (xs)) (add_all (Add ny :: ys)))))), 
              ((List.append (take_all (xs)) 
              (remove_from_left (maintain_order_list (add_all (xs)) (add_all (Add ny :: ys)))))))
             else if (List.mem (Add nx) xs) then 
                ((remove_from_right(List.append
                (take_all (Add nx :: Add ny :: ys))
                (List.append (add_all (delete (Add nx) xs)) (add_all (Add nx :: Add ny :: ys))))),
                ((remove_from_right
                (add_all (Add ny :: ys)))))
         else   ((delete (Add nx) (remove_from_right (List.append
                (take_all (Add nx :: Add ny :: ys))
                (diff_edit (xs) (Add ny :: ys))))),
                (delete (Add nx) (remove_from_left (List.append 
                (take_all (xs))
                (diff_edit (xs) (Add ny :: ys))))))
  | Take nx :: xs, Take ny :: ys -> if ((check_common_el_list (xs) (delete (Add nx) (Add ny :: ys))) && (not (List.mem (Add nx) ys)) &&
                 (not (List.mem (Add nx) xs))) then 
                 ((remove_from_right (List.append (take_all (ys))
                 (remove_from_left (maintain_order_list (add_all (xs)) (add_all (ys)))))), 
                 ((List.append (take_all (xs)) 
                 (remove_from_left (maintain_order_list (add_all (xs)) (add_all (ys)))))))
                else ((remove_from_right (List.append 
                (take_all (Take ny :: ys))
                (diff_edit (Take nx :: xs) (Take ny :: ys)))), 
                (remove_from_right (List.append 
                (take_all (Take nx :: xs))
                (diff_edit (Take nx :: xs) (Take ny :: ys)))))

   let op_transform p q =
    let rec go xs ys =
      match xs, ys with
      | [], [] -> ([], [])
      | xs, [] -> (xs, [])
      | [], ys -> ([], ys)
      | p', q' when p'= q' -> ([], []) 
      | x::xs, y::ys ->
        begin
          match x,y with
          | Add nx, Add ny  when nx = ny -> 
             (diff_append (x :: xs) (y :: ys)) 
          | Add (nx), Add (ny) -> 
              diff_append (x :: xs) (y :: ys)
          | Add nx, Take ny -> if (xs = [] && ys = []) then (Add nx :: [], Take ny :: []) 
                               else (diff_append (x :: xs) (y :: ys))
          | Take nx, Add ny -> if (xs = [] && ys = []) then (Take nx :: [], Add ny :: []) 
                               else (diff_append (x :: xs) (y :: ys))
          | Take nx, Take ny -> if ys = [] then (fst (diff_append xs []), []) else diff_append (x :: xs) (y :: ys)

        end
    in
    go p q



   let rec apply s = function
    | [] -> s
    | Add(c)::tl ->
      let s' = ((add c) s) in
      apply s' tl
    | Take(c)::tl ->
      let s' = (q_after_take s)  in
      (apply s' tl)

  let merge3 ~ancestor l r =
    let p = op_diff ancestor l in
    let q = op_diff ancestor r in
    let _,q' = op_transform p q in
    apply l q'



end 
