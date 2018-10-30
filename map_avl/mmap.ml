(* KEY is a module that consist of type t and function comapre *)
module type KEY =
sig
  type t
  val t: t Irmin.Type.t
  val to_string : t -> string   
  val of_string: string -> t
  val compare: t -> t -> int
end

module type ATOM = sig
  type t
  val equal: t -> t -> bool
  val t: t Irmin.Type.t
  val to_string : int64 -> string   
  val of_string: string -> t
  include Msigs.RESOLVEABLE with type t := t
end

module type Base = sig
  type t
  type key
  type atom

  val empty : t
  val is_empty : t -> bool
  val mem : key -> t -> bool
  val add : key -> atom -> t -> t
  val remove : key -> t -> t
  val compare : (atom -> atom -> int) -> t -> t -> int
  val equal : t -> t -> bool
  val iter : (key -> atom -> unit) -> t -> unit
  val fold : (key -> atom -> 'b -> 'b) -> t -> 'b -> 'b
  val find : key -> t -> atom
  val map : (atom -> atom) -> t -> t
  val mapi : (key -> atom -> atom) -> t -> t
end

module type S = sig
  include Base

(* edit is of three forms Add, Remove and Replace *)
  (* Patching *)
  type edit = 
    | Add of key * atom
    | Remove of key
    | Replace of key * atom * atom

  include Msigs.PATCHABLE with type t := t and type edit := edit

  (* Merging *)
  include Msigs.RESOLVEABLE with type t := t
end

module type Make = functor (Key : KEY) (Atom : ATOM) -> S
  with type key = Key.t 
  and type atom = Atom.t 