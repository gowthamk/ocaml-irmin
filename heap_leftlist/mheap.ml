(* Heap is a data structure where the minimum element can be easily retrived *)
(* In heap we just need to keep partial order instead of keeping the total order in the binary trees *)
(* The root needs to be smalled than the two children but the relationship between the two children does not matter *)
module type ATOM =
sig
  type t
  val t: t Irmin.Type.t
  val compare: t -> t -> int
  val to_string : int64 -> string   
  val of_string: string -> t
end

module type Base = sig
  type t
  type atom

  val empty : t
  val is_empty : t -> bool

  val insert : atom -> t -> t
  val merge : t -> t -> t

  val find_min : t -> atom  (* raises Empty if heap is empty *)
  val delete_min : t -> t   (* raises Empty if heap is empty *)
  val pop_min : t -> atom * t   (* raises Empty if heap is empty *)
end

module type S = sig
  include Base

  (* Patching *)
  type edit =
    | Insert of atom
    | Delete of atom
  
  include Msigs.PATCHABLE with type t := t and type edit := edit

  (* Merging *)
  include Msigs.RESOLVEABLE with type t := t
end