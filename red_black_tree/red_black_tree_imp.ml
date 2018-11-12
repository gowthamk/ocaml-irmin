open Lwt.Infix

module type ATOM =
sig 
 type t 
 val t: t Irmin.Type.t
 val compare: t -> t -> int
 val to_string: t -> string
 val of_string: string -> t
end 

module Make (Atom : ATOM) = struct
 type atom = Atom.t
 type color = R | B
 type node = {cl : color; l : t; v : atom; r : t} 
 and t = E | T of node

 let empty = E

 let makeBlack t = match t with 
   | E -> E
   | T{cl; l; v; r} -> T {cl = B; l ; v; r}

 let balance = function
      | {cl=B; l=T {cl=R; l= T {cl=R; l=a; v=x; r=b}; v=y; r=c}; v=z; r=d} -> T {cl=R; l=T {cl=B; l=a; v=x; r=b}; v=y; r=T {cl=B; l=c; v=z; r=d}}
      | {cl=B; l=T {cl=R; l=a; v=x; r=T {cl=R; l=b; v=y; r=c}}; v=z; r=d} -> T {cl=R; l=T {cl=B; l=a; v=x; r=b}; v=y; r=T {cl=B; l=c; v=z; r=d}}
      | {cl=B; l=a; v=x; r=T {cl=R; l=T {cl=R; l=b; v=y; r=c}; v=z; r=d}} -> T {cl=R; l=T {cl=B; l=a; v=x; r=b}; v=y; r=T {cl=B; l=c; v=z; r=d}}
      | {cl=B; l=a; v=x; r=T{cl=R; l=b; v=y; r=T{cl=R;l=c; v=z; r=d}}} -> T {cl=R; l=T {cl=B; l=a; v=x; r=b}; v=y; r=T {cl=B; l=c; v=z; r=d}}
      | {cl=color; l=a; v=x; r=b} -> T {cl=color; l=a; v=x; r=b}

 let rec insert x t =
  let rec ins = function
        | E -> T {cl=R; l=E; v=x; r=E}
        | T {cl=color; l=a; v=y; r=b} as s ->
           if Atom.compare x y < 0 then balance {cl=color; l=ins a; v=y; r=b}
           else if Atom.compare x y > 0 then balance {cl=color; l=a; v=y; r=ins b}
           else s
        in
        let T {cl=_; l=a; v=y; r=b} = ins t in
        T {cl=B; l=a; v=y; r=b}

 let rec member x = function
    | E -> false
    | T {cl=_; l=a; v=y; r=b} ->
       if Atom.compare x y < 0 then member x a
       else if Atom.compare x y > 0 then member x b
       else true

  let rec add_min_element x = function
    | E -> T {cl=R; l=E; v=x; r=E}
    | T {cl=co; l=a; v=x; r=b} ->
      balance {cl=co; l=(add_min_element x a); v=x; r=b}

  let rec add_max_element x = function
    | E -> T {cl=R; l=E; v=x; r=E}
    | T {cl=co; l=a; v=x; r=b} ->
      balance {cl=co; l=a; v=x; r=(add_max_element x a)}


  let rec min_elt = function 
    | E -> raise Not_found
    | T {cl=co; l=E; v=x; r=E} -> x
    | T {cl=co; l=a; v=x; r=b} -> min_elt a

  
  let rec max_elt = function
    | E -> raise Not_found
    | T {cl=co; l=E; v=x; r=E} -> x
    | T {cl=co; l=a; v=x; r=b} -> max_elt b


  let rec remove_min_elt = function 
    | E -> raise Not_found 
    | T {cl=co; l=E; v=x; r=b} -> b
    | T {cl=co; l=a; v=x; r=b} -> balance {cl=co; l=(remove_min_elt a); v=x; r=b}

	  
  (* all the elements of l preceeds the elements of r and height of l is atmost 2 more than the height of r *)
  let merge t1 t2 = 
	match (t1, t2) with 
	  (E, t) -> t
	| (t, E) -> t
	| (_,_) -> balance {cl=R; l=t1; v=(min_elt t2); r=(remove_min_elt t2)}


  let rec fold f s accu = match s with
   | E -> accu 
   | T {cl=co; l=a; v=x; r=b} -> fold f a ( f x (fold f b accu))


  let rec remove e = function 
    | E -> E
    | (T{cl=co; l=a; v=x; r=b} as t) -> 
        let c = Atom.compare e x in 
        if c = 0 then merge a b 
        else   
         if c < 0 then 
           let ll = remove e a in 
           if a == ll then t 
             else balance {cl=co; l=ll; v=x; r=b}
         else 
          let rr = remove e b in 
          if b == rr then t 
           else balance {cl=co; l=a; v=x; r=rr}

  exception MergeConflict
  let rec merge3 old v1 v2 = 
    if v1=v2 then v1
    else if v1=old then v2
    else if v2=old then v1
    else match (old,v1,v2) with
      | (E, T _, E) -> v1 (* new sub-tree in v1 *)
      | (E, E, T _) -> v2 (* new sub-tree in v2 *)
      | (E, T {cl=co;l=lt1;v=x1;r=rt1}, T {cl=co';l=lt2;v=x2;r=rt2}) -> 
          (* new sub-trees in v1 and v2 *)
          if x1=x2 (* and they are compatible *)
          then balance {cl=R; l=merge3 E lt1 lt2; v=x1; r=merge3 E rt1 rt2}
          else raise MergeConflict (* not compatible *)
      | (T {cl=co;l=lt;v=x;r=rt}, T {cl=co';l=lt1;v=x1;r=rt1}, T {cl=co'';l=lt2;v=x2;r=rt2}) ->
          let lt'() = merge3 lt lt1 lt2 in
          let rt'() = merge3 rt rt1 rt2 in
          if (x1=x2) then  balance {cl=R; l=lt'(); v=x1; r=rt'()}
          else if (x1=x) then balance {cl=R;l=lt'(); v=x2; r=rt'()}
          else if (x2=x) then balance {cl= R; l=lt'(); v=x1; r=rt'()}
          else raise MergeConflict (* same text edited *)
      | (T _,E, _) | (T _, _, E) -> 
          failwith "Impossible; with tombstones, branch 
                    cannot be deleted."
      | (E,E,E) -> E (* for the sake of completeness *)


 (*(* Patching *)
 type edit = 
   | Add of atom 
   | Remove of atom 

 type patch = edit list 

 let edit_to_string atom_to_string = function
   | Add(a) -> Printf.sprintf "Add (%s)" (atom_to_string a)
   | Remove(a) -> Printf.sprintf "Remove (%s)" (atom_to_string a)

 let of_diff xt yt =
   let rec diff_rbt s1 s2 =
     match (s1,s2) with 
      | (E, t2) -> fold (fun x y -> y @ [Add x]) t2 []
      | (t1, E) -> fold (fun x y -> y @ [Remove x]) t1 []
      | (T{cl=co; l=a; v=x; r=b}, T{cl=co'; l=a'; v=x'; r=b'}) -> *)


	    



	end
