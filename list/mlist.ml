open Printf
module type ATOM = sig
  type t
  val t: t Irmin.Type.t
  val to_string : t -> string
  val of_string: string -> t
  val compare: t -> t -> int
  (*include Msigs.RESOLVEABLE with type t := t*)
end

module Make (Atom: ATOM)  = struct
  type atom = Atom.t
  type t = atom list

  (* Assumes i≥0 *)
  let rec insert l i a = match i,l with
    | 0,_ -> a::l
    | _,[] -> [a]
    | _,x::xs -> x::(insert xs (i-1) a)

  (* Assumes i≥0 ∧ i<length(l)*)
  let rec remove l i = match i,l with
    | 0, x::xs -> xs
    | _, [] -> []
    | _, x::xs -> x::(remove xs (i-1))

  let is_empty = function
    | [] -> true
    | _ -> false

  (*module S = Set.Make(struct
                        type t = Atom.t
                        let compare = Atom.compare`
                      end)*)

  let (yt : (Atom.t,bool) Hashtbl.t) = Hashtbl.create 2017

  let (zt : (Atom.t,bool) Hashtbl.t) = Hashtbl.create 2017

  let (removes : (Atom.t,bool) Hashtbl.t) = Hashtbl.create 2017

  let contains t a =
    (match Hashtbl.find_opt t a with
      | Some true -> true
      | Some false -> false
      | None -> false)

  let removed a = contains removes a

  let remove a = 
    let _ = printf "Removing %s\n" (Atom.to_string a) in
    Hashtbl.add removes a true

  let populate_table t l =
    List.iter (fun a -> Hashtbl.add t a true) l

  let populate_removes xs = 
    List.iter (fun x -> if contains yt x && contains zt x 
                        then () else remove x) xs
  (*
   * IMPORTANT: merge assumes there are no duplicates
   *)
  let merge_time = ref 0.0

  let rec two_way_merge ys zs = 
    match ys,zs with
      | y::ys', z::zs' ->
          if y<=z then y::(two_way_merge ys' zs)
          else z::(two_way_merge ys zs')
      | [], _ -> List.filter (fun z -> not @@ removed z) zs
      | _, [] -> List.filter (fun y -> not @@ removed y) ys


  let merge xs ys zs =
    begin
(*       let t1 = Sys.time () in *)
      populate_table yt ys;
      populate_table zt zs;
      populate_removes xs;
      let ys' = List.filter (fun y -> not @@ removed y) ys in
      let zs' = List.filter (fun z -> not @@ removed z) zs in
      let v = two_way_merge ys' zs' in
(*       let t2 = Sys.time () in *)
      Hashtbl.clear yt;
      Hashtbl.clear zt;
      Hashtbl.clear removes;
(*       merge_time := !merge_time +. (t2-.t1); *)
(*       printf "Merge time: %fs\n" !merge_time; *)
(*       flush_all(); *)
      v
    end

end

