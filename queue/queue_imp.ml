open Lwt.Infix

module type ATOM =
sig 
 type t 
 val t: t Irmin.Type.t
 val compare: t -> t -> int
 val to_string: t -> string
 val of_string: string -> t
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
 
 (* clear q clears the queue where the length is assigned to 0 and first and last field are 
    assigned Nil  *)
 let clear q =  q.length <- 0 ; q.first <- Nil; q.last <- Nil
 
 (* add x q adds the element x to the queue q *) 
 let add x q = 
     let cell = Cons {content = x; next = Nil} in 
     match q.last with 
       | Nil -> q.length <- 1; 
                q.first <- cell;
                q.last <- Nil
       | Cons last -> q.length <- q.length + 1;
                      last.next <- cell;
                      q.last <- cell
 (* push represents the push operation which basically pushes an element to the queue *)
 let push = add

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

                          
end 
