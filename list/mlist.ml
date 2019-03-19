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

  let contains t a =
    (match Hashtbl.find_opt t a with
      | Some true -> true
      | Some false -> false
      | None -> false)

  let populate_table t l =
    List.iter (fun a -> Hashtbl.add t a true) l

  (*
   * IMPORTANT: merge assumes there are no duplicates
   *)
  let rec merge_vertical xs ys yt =
    match xs,ys with
      | x::xs', y::ys'
          when x=y -> x::(merge_vertical xs' ys' yt)
      | x::xs', y::ys' -> if contains yt x
                          then y::(merge_vertical xs ys' yt)
                          else merge_vertical xs' ys yt
      | [],_ -> ys
      | _,[] -> []

  let rec merge_horizontal ys zs =
    match ys,zs with
      | y::ys', z::zs' ->
          if y<z then y::z::(merge_horizontal ys' zs')
          else z::y::(merge_horizontal ys' zs')
      | [], _ -> zs
      | _, [] -> ys

  let rec merge xs ys zs =
    match xs,ys,zs with
      | x::xs', y::ys', z::zs'
          when x=y && y=z -> x::(merge xs' ys' zs')
      | x::xs', y::ys', z::zs'
          when x=y -> if contains zt x
                      then z::(merge xs ys zs')
                      else merge xs' ys' zs
      | x::xs', y::ys', z::zs'
          when x=z -> if contains yt x
                      then y::(merge xs ys' zs)
                      else merge xs' ys zs'
      | x::xs', y::ys', z:: zs' ->
          if contains yt x && contains zt x
          then if y<z then y::z::(merge xs ys' zs')
               else z::y::(merge xs ys' zs')
          else merge xs' ys zs
      | [], _::_,_::_ | [], _::_, [] | [], [], _::_ -> merge_horizontal ys zs
      | _::_, _::_, [] -> merge_vertical xs ys yt
      | _::_,[],_::_ -> merge_vertical xs zs zt
      | _,[],[] -> []

  let merge_time = ref 0.0

  let merge xs ys zs =
    begin
(*       let t1 = Sys.time () in *)
      populate_table yt ys;
      populate_table zt zs;
      let v = merge xs ys zs in
(*       let t2 = Sys.time () in *)
      Hashtbl.clear yt;
      Hashtbl.clear zt;
(*       merge_time := !merge_time +. (t2-.t1); *)
(*       printf "Merge time: %fs\n" !merge_time; *)
(*       flush_all(); *)
      v
    end

end

