open Lwt.Infix

module type ATOM =
sig 
 type t 
 val t: t Irmin.Type.t
 val compare: t -> t -> int
 val to_string: t -> string
 val of_string: string -> t
end 

(* Graph is defined as a set of nodes and a set of edges, where each edge is a pair of different nodes *)
module Make (Atom : ATOM) =
struct
 type atom = Atom.t
 type t = {nodes : atom list; edges : (atom * atom) list}
 
end 