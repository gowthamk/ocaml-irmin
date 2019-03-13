module K = Irmin.Hash.SHA1

module type TAG_TREE = sig
  type t
  type tag
  type value
  val tag_of_string: string -> tag
  val tag_of_hash: K.t -> tag
  val empty: unit -> t
  val add: t -> tag -> value -> t Lwt.t
end

module type AO_STORE = sig
  type t
  type adt
  type value
  val create: unit -> t

  val add_adt: (module TAG_TREE 
                 with type t='a 
                  and type value=value) 
                -> t -> adt -> 'a -> (K.t*'a) Lwt.t

  val read_adt: t -> K.t -> adt
end

module type IRMIN_STORE_VALUE = sig
  type adt
  include Irmin.Contents.S
  val of_adt : (module TAG_TREE 
                 with type t='a 
                  and type value=t) 
              -> adt -> 'a -> (t*'a) Lwt.t
  val to_adt: t -> adt Lwt.t
end

module type IRMIN_STORE = 
sig
  type t
  type repo
  type path = string list
  type tree
  type adt
  type value
  module Sync:Irmin.SYNC with type db = t
  module Tree: TAG_TREE with type t=tree and type value=value
  val init : ?root:'a -> ?bare:'b -> unit -> repo Lwt.t
  val master : repo -> t Lwt.t
  val clone : t -> string -> t Lwt.t
  val get_branch : repo -> branch_name:string -> t Lwt.t
  val merge : t ->
    into:t ->
    info:Irmin.Info.f -> (unit, Irmin.Merge.conflict) result Lwt.t
  val read : t -> path -> adt option Lwt.t
  val info : string -> Irmin.Info.f
  val update : ?msg:string -> t -> path -> value -> unit Lwt.t
  val with_tree : t -> path -> info:Irmin.Info.f ->
                  (tree option -> tree option Lwt.t) -> unit Lwt.t
end


module type VERSIONED_DATA_STRUCTURE_STORE = sig
  type t
  type adt
  val t: t Irmin.Type.t
  module AO_value: Irmin.Contents.Conv with type t =t
  module AO_store: AO_STORE with type adt=adt 
                             and type value=AO_value.t
  module BC_value: IRMIN_STORE_VALUE with type t=t 
                                      and type adt = adt

  module BC_store: IRMIN_STORE with type adt=adt 
                                and type value=t
end
