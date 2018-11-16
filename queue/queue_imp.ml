(*open Lwt.Infix*)

module type ATOM =
sig 
 type t 
 (*val t: t Irmin.Type.t
 val compare: t -> t -> int*)
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

 (* create function creates a queue which is initially empty *)
 let create () = {length = 0; first = Nil ; last = Nil}

 let empty = {length = 0; first = Nil ; last = Nil}
 
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
       | Cons {content; next} -> if n = 0 then content else nthq_aux {length = q.length -1; first = next; last = next} (n-1)
     in nthq_aux q n 


 let get q i = nthq q i



       (*| empty -> failwith "nthq"
       | {length = m; first = f; last = l} -> if n = 0 then f else nthq_aux {length = m -1; first=f; last=l} (n-1)
      in nthq_aux q n *)

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


  (* Patching *)

  type edit =
   | Add of atom 
   | Take 


  type patch = edit list 

  let edit_to_string atom_to_string = function 
  | Add a -> Printf.sprintf "Add %s" (atom_to_string a)
  | Take -> Printf.sprintf "Take" 


  (*let oo_diff xt yt =
   let rec diff_que s1 s2 =
    match (s1, s2) with
      | ({length=0; first = Nil; last = Nil}, t2) -> fold (fun x y -> y @ [Add x]) t2 []
      | (t1, {length=0; first = Nil; last = Nil}) -> fold (fun x y -> y @ [Take]) t1 []
      | ({length=_; first = f1; last = l1}, {length=_; first = f1; last = l1})*)

  (*let op_diff xs ys =
    let cache = Array.init (length xs+1)
        (fun _ -> Array.make (length ys+1) None)
    in
    let rec loop i j =
      let cache_i = Array.unsafe_get cache i in
      let min3 x y z =
        let m' (a,al) (b,bl) = if a < b then (a,al) else (b,bl) in
        m' (m' x y) z
      in
      match Array.unsafe_get cache_i j with
      | Some v -> v
      | None ->
        let res =
          begin match i,j with
            | 0,0 -> (0, [])
            | 0, j ->
              let d,e = loop 0 (j-1) in
              (d+1, (Add (get ys (j-1))::e))
            | i, 0 ->
              let d,e = loop (i-1) 0 in
              (d+1, (Take::e))*)

 

 





























                          
end 
