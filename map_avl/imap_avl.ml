open Lwt.Infix
open Irmin_unix

(* Config module has three functions root, shared and init. *)
module type Config = sig
  val root: string
  val shared: string
  val init: unit -> unit
end

(* MakeVersioned is a functor which takes Config and Atom as arguments *)
module MakeVersioned (Config: Config) (Key: Mmap_avltree.KEY) (Atom: Mmap_avltree.ATOM) = struct
  module OM = Mmap_avltree.Make(Key)(Atom)
  module K = Irmin.Hash.SHA1

  let from_just = function (Some x) -> x
  | None -> failwith "Expected Some. Got None."

  (* vnode is a record type *)
  type vnode = {l:K.t; v:OM.key; d:OM.atom; r:K.t; h:int64}
  
  (* vt is a type with two constructors 
   * Empty which represents empty tree
   * Node of vnode *)
  type vt = 
    | Empty 
    | Node of vnode

  (* M is a structure with module AO_value 
   * AO_value is a structure with type t which is equal to vt 
   * node is a record type defined using record type in Irmin.Type
   * sealr is used to close the open record 
   * t is a function which includes the variant type *)
  module M = struct
    module AO_value = struct
      type t = vt

     let node = 
        let open Irmin.Type in
        record "node" (fun l v d r h -> {l;v;d;r;h})
        |+ field "l" K.t (fun t -> t.l)
        |+ field "v" Key.t (fun t -> t.v)
        |+ field "d" Atom.t (fun t -> t.d)
        |+ field "r" K.t (fun t -> t.r)
        |+ field "h" int64 (fun t -> t.h)
        |> sealr


      let t =
        let open Irmin.Type in
        variant "t" (fun empty node -> function
            | Empty -> empty
            | Node n -> node n)
        |~ case0 "Empty" Empty
        |~ case1 "Node" node (fun x -> Node x)
        |> sealv

      let pp = Irmin.Type.dump t
      
      (* why of_string is defined? *)
      (* It converts the string back to our type *)
      let of_string (s:string) =
        let from_just_ok = function (Ok x) -> x
                                  | _ -> failwith "Expected Ok. Got error." in
        let remove_blanks = Str.global_replace (Str.regexp "[\n\r\t ]") "" in
        let s = remove_blanks s in
        match Str.split (Str.regexp "[{}]") s with
        | ["Empty"] -> Ok Empty
        | ["Node"; node] ->
          let rx = Str.regexp "l=\\(.+\\);v=\\(.+\\);r=\\(.+\\);h=\\(.+\\);" in
          let _ = Str.search_forward rx node 0 in
          let l = from_just_ok @@ K.of_string @@ Str.matched_group 1 node in
          let v = Key.of_string @@ Str.matched_group 2 node in  
          let d = Atom.of_string @@ Str.matched_group 3 node in
          let r = from_just_ok @@ K.of_string @@ Str.matched_group 4 node in
          let h = Int64.of_string @@ Str.matched_group 5 node in
          Ok (Node {l;v;d;r;h})
        | _ -> Error (`Msg ("invalid entry: " ^ s))
    end
    
    (* storage backhend: Append-only store *)
    module AO_store = struct
      (* Immutable collection of all versionedt *)
      module S = Irmin_git.AO(Git_unix.FS)(AO_value)
      include S

      let create config =
        let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
            "level" Irmin.Private.Conf.(some int) None
        in
        let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
        let level = Irmin.Private.Conf.get config level in
        Git_unix.FS.create ?root ?level ()

      (* Somehow pulls the config set by Store.init *)
      (* And creates a Git backend *)
      let create () = create @@ Irmin_git.config Config.shared
    end

    type t = K.t
     
    (* returns the Irmin data type *) 
    (* aostore is created using the create function *)
    (* function aostore_add adds the value to the store *)
    (* Value a is matched against the different constructor of the set data type *)
    (* If the set data type is Empty then the thread returns Empty *)
    (* If the set data type is Node then of_adt is applied to left and right node *)
    let rec of_adt (a:OM.t) : t Lwt.t  =
      let aostore = AO_store.create () in
      let aostore_add value =
        aostore >>= (fun ao_store -> AO_store.add ao_store value) in
      aostore_add =<<
      (match a with
       | OM.Empty -> Lwt.return @@ Empty
       | OM.Node {l;v;d;r;h} -> 
         (of_adt l >>= fun l' ->
          of_adt r >>= fun r' ->
          Lwt.return {l=l'; v; d; r=r'; h=Int64.of_int h})
         >>= ((fun n -> Lwt.return @@ (Node n))))
    
    (* returns the basic OCaml data type *)
    (* for the key k we find the data stored with that key using the function find *)
    (* Then t is matched with type defined in the AO_value *)
    let rec to_adt (k:t) : OM.t Lwt.t =
      AO_store.create () >>= fun ao_store ->
      AO_store.find ao_store k >>= fun t ->
      let t = from_just t in
      (match t with
      | Empty -> Lwt.return @@ OM.Empty
      | Node {l;v;d;r;h} ->
        (to_adt l >>= fun l' ->
         to_adt r >>= fun r' ->
         Lwt.return {OM.l=l'; OM.v; OM.d; OM.r=r'; OM.h=Int64.to_int h})
        >>= ((fun n -> Lwt.return @@ (OM.Node n))))

    let t = K.t

    let pp = K.pp

    let of_string = K.of_string
 
    (* merge function merges old, v1_k and v2_k *)
    (* Irmin.Merge.promise t is a promise containing a value of type t *)
    (* using the to_adt, old_k, v1_k and v2_k is converted to the OCaml data type *)
    let rec merge ~(old:t Irmin.Merge.promise) v1_k v2_k =
      let open Irmin.Merge.Infix in
      old () >>=* fun old_k ->
      let old_k = from_just old_k in
      to_adt old_k >>= fun oldv  ->
      to_adt v1_k >>= fun v1  ->
      to_adt v2_k >>= fun v2 ->
      let v = OM.merge3 oldv v1 v2 in
      of_adt v >>= fun merged_k ->
      Irmin.Merge.ok merged_k

    let merge = Irmin.Merge.(option (v t merge))
  end

  (* Store is defined as follows which is a module *)
  module BC_store = struct
    module Store = Irmin_unix.Git.FS.KV(M)
    module Sync = Irmin.Sync(Store)

    type t = Store.t

    let init ?root ?bare () =
      let config = Irmin_git.config Config.root in
      Store.Repo.v config

    let master (repo:Store.repo) = Store.master repo

    let clone t name = Store.clone t name

    let get_branch r ~branch_name = Store.of_branch r branch_name

    let merge s ~into = Store.merge s ~into

    let update t k v = Store.set t k v

    let read t k = Store.find t k
  end

(* Vpst is a module which consist of type store, st and 'a t *)
  module Vpst : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val with_init_version_do: OM.t -> 'a t -> 'a
  val fork_version : 'a t -> unit t
  val get_latest_version: unit -> OM.t t
  val sync_next_version: ?v:OM.t -> OM.t t
  val liftLwt : 'a Lwt.t -> 'a t
end = struct
    (* store is a type which is basically of type BC_store.t *)
    type store = BC_store.t
    (* st is a record type with fields as master, local, name and next_id *)
    type st = {master   : store;
               local    : store;
               name     : string;
               next_id  : int}
    type 'a t = st -> ('a * st) Lwt.t

    let info s = Irmin_unix.info "[repo %s] %s" Config.root s  

    let path = ["state"]

    let return (x : 'a) : 'a t = fun st -> Lwt.return (x,st)

    let bind (m1: 'a t) (f: 'a -> 'b t) : 'b t = 
      fun st -> (m1 st >>= fun (a,st') -> f a st')

    let with_init_version_do (v: OM.t) (m: 'a t) =
      Lwt_main.run 
        begin
          BC_store.init () >>= fun repo -> 
          BC_store.master repo >>= fun m_br -> 
          M.of_adt v >>= fun k ->
          let cinfo = info "creating state of master" in
          BC_store.update m_br path k ~info:cinfo >>= fun () ->
          BC_store.clone m_br "1_local" >>= fun t_br ->
          let st = {master=m_br; local=t_br; name="1"; next_id=1} in
          m st >>= fun (a,_) -> Lwt.return a
        end

    let with_init_forked_do (m: 'a t) = 
      BC_store.init () >>= fun repo -> 
      BC_store.master repo >>= fun m_br ->
      BC_store.clone m_br "1_local" >>= fun t_br ->
      let st = {master=m_br; local=t_br; name="1"; next_id=1} in
      m st >>= fun (a, _) -> Lwt.return a

    let fork_version (m: 'a t) : unit t = fun (st: st) ->
      let thread_f () = 
        let child_name = st.name^"_"^(string_of_int st.next_id) in
        let parent_m_br = st.master in
        (* Ideally, the following has to happen: *)
        (* BC_store.clone_force parent_m_br m_name >>= fun m_br -> *)
        (* But, we currently default to an SC mode. Master is global. *)
        let m_br = parent_m_br in
        BC_store.clone m_br (child_name^"_local") >>= fun t_br ->
        let new_st = {master = m_br; local  = t_br; name = child_name; next_id = 1} in
        m new_st in
      begin
        Lwt.async thread_f;
        Lwt.return ((), {st with next_id=st.next_id+1})
      end

    let get_latest_version () : OM.t t = fun (st: st) ->
      BC_store.read st.local path >>= fun k ->
      M.to_adt @@ from_just k >>= fun td ->
      Lwt.return (td,st)

    let sync_remote_version remote_uri ?v : OM.t t = fun (st: st) ->
      (* How do you commit the next version? Simply update path? *)
      (* 1. Commit to the local branch *)
      let cinfo = info "committing local state" in
      (match v with 
       | None -> Lwt.return ()
       | Some v -> 
         M.of_adt v >>= fun k -> 
         BC_store.update st.local path k cinfo) >>= fun () ->

      (* 2.. Pull from remote to master *)
      let cinfo = info (Printf.sprintf "Merging remote: %s" remote_uri) in
      BC_store.Sync.pull st.master (Irmin.remote_uri remote_uri) (`Merge  cinfo) >>= fun _ ->
      (* 2. Merge local master to the local branch *)
      let cinfo = info "Merging master into local" in
      BC_store.merge st.master ~into:st.local ~info:cinfo >>= fun _ ->
      (* 3. Merge local branch to the local master *)
      let cinfo = info "Merging local into master" in
      BC_store.merge st.local ~into:st.master ~info:cinfo >>= fun _ ->
      get_latest_version () st

    let sync_next_version ?v : OM.t t = fun (st: st) ->
      (* How do you commit the next version? Simply update path? *)
      (* 1. Commit to the local branch *)
      let cinfo = info "committing local state" in
      (match v with 
       | None -> Lwt.return ()
       | Some v -> 
         M.of_adt v >>= fun k -> 
         BC_store.update st.local path k cinfo) >>= fun () ->

      (* 2. Merge local master to the local branch *)
      let cinfo = info "Merging master into local" in
      BC_store.merge st.master ~into:st.local ~info:cinfo >>= fun _ ->
      (* 3. Merge local branch to the local master *)
      let cinfo = info "Merging local into master" in
      BC_store.merge st.local ~into:st.master ~info:cinfo >>= fun _ ->
      get_latest_version () st

    let liftLwt (m: 'a Lwt.t) : 'a t = fun st ->
      m >>= fun a -> Lwt.return (a,st)
end 
end






