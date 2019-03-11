open Lwt.Infix
open Irmin_unix
open Printf

(* Config module has three functions root, shared and init. *)
module type Config = sig
  val root: string
  val shared: string
  val init: unit -> unit
end

let from_just op msg = match op with
  | Some x -> x
  | None -> failwith @@ msg^": Expected Some. Got None."

(* MakeVersioned is a functor which takes Config and Atom as arguments *)
module MakeVersioned (Config: Config) (Atom: Mlist.ATOM) = struct
  module OM = Mlist.Make(Atom)
  module K = Irmin.Hash.SHA1

  type targ = (Atom.t * K.t)
  type vt = 
    | Nil
    | Cons of targ

  module AO_value  = struct
    
    type t = vt 

    let mktarg t = let open Irmin.Type in (pair Atom.t K.t)

    let mkt targ =
      let open Irmin.Type in
      variant "t" (fun c n -> function
          | Cons a  -> c a
          | Nil -> n)
      |~ case1 "Cons" targ (fun x -> Cons x)
      |~ case0 "Nil" Nil
      |> sealv

    let t,earg = let open Irmin.Type in mu2 (fun t targ -> mkt targ, mktarg t)

    let pp = Irmin.Type.pp_json ~minify:false t

    let of_string s =
      let decoder = Jsonm.decoder (`String s) in
      let res = try Irmin.Type.decode_json t decoder 
                with Invalid_argument s -> 
                  (failwith @@ sprintf "AO_Value.of_string:\
                    \ Invalid_argument: %s" s) in
      let _ = match res with
        | Ok _ -> ()
        | Error (`Msg str) -> 
          (printf "Decoding error: %s\n" str;
          printf "While decoding: %s\n" s) in 
      res
  end
    
    (* storage backhend: Append-only store *)
  module AO_store = struct
    (* Immutable collection of all versionedt *)
    module S = Irmin_git.AO(Git_unix.Mem)(AO_value)
    include S

    let create config =
      let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
          "level" Irmin.Private.Conf.(some int) None
      in
      let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
      let level = Irmin.Private.Conf.get config level in
      Git_unix.Mem.create ?root ?level ()

    (* Somehow pulls the config set by Store.init *)
    (* And creates a Git backend *)
    let create () = create @@ Irmin_git.config Config.shared

    let on_add = ref (fun k v -> Lwt.return ())

    let add t v = 
      S.add t v >>= fun k ->
      (!on_add) k v >>= fun _ ->
      Lwt.return k

    let rec add_adt t (a:OM.t) : K.t Lwt.t =
      add t =<<
        (match a with
         | n::rt -> 
           (add_adt t rt >>= fun rt' ->
            Lwt.return @@ Cons (n,rt'))
         | [] -> Lwt.return @@ Nil)

    let rec read_adt t (k:K.t) : OM.t Lwt.t =
      find t k >>= fun aop ->
      let a = from_just aop "to_adt" in
      (match a with
        | Cons (n, rt) ->
          (read_adt t rt >>= fun rt' ->
           Lwt.return @@ n::rt')
        | Nil -> Lwt.return @@ [])

    let find_or_fail t (k:K.t) : AO_value.t Lwt.t =
      find t k >>= fun vop ->
      Lwt.return @@ from_just vop "find_or_fail"
  end

  let merge_time = ref 0.0
  let real_merge_time = ref 0.0
  let merge_count = ref 0

  module type IRMIN_STORE_VALUE = sig
    include Irmin.Contents.S
    val of_adt: OM.t -> t Lwt.t
    val to_adt: t -> OM.t Lwt.t
  end
 
  module BC_value: IRMIN_STORE_VALUE with type t = vt = struct
    include AO_value

    let of_adt (a:OM.t) : t Lwt.t  =
      AO_store.create () >>= fun ao_store -> 
      let aostore_add adt =
        AO_store.add_adt ao_store adt in
      match a with
       | n::rt -> 
         (aostore_add rt >>= fun rt' ->
          Lwt.return @@ Cons(n,rt'))
       | [] -> Lwt.return @@ Nil

    let to_adt (t:t) : OM.t Lwt.t =
      AO_store.create () >>= fun ao_store ->
      let aostore_read k =
        AO_store.read_adt ao_store k in
      match t with
        | Cons (n,rt) ->
          (aostore_read rt >>= fun rt' ->
           Lwt.return @@ n::rt')
        | Nil -> Lwt.return []
 
    (* merge function merges old, v1_k and v2_k *)
    (* Irmin.Merge.promise t is a promise containing a value of type t *)
    (* using the to_adt, old_k, v1_k and v2_k is converted to the OCaml data type *)
    let rec merge ~(old:t Irmin.Merge.promise) v1 v2 =
      let _ = Gc.full_major () in
      let t1 = Sys.time () in 
      let res = 
        if v1=v2 then Irmin.Merge.ok v1
        else begin 
          let open Irmin.Merge.Infix in
          old () >>=* fun old ->
          let old = from_just old "merge.old" in
          to_adt old >>= fun oldv  ->
          to_adt v1 >>= fun v1  ->
          to_adt v2 >>= fun v2 ->
          let v = OM.merge oldv v1 v2 in
          of_adt v >>= fun merged_k ->
          Irmin.Merge.ok merged_k
        end in
      let t2 = Sys.time () in
      let _ = merge_time := !merge_time +. (t2-.t1) in
      let _ = merge_count := !merge_count + 1 in
      let _ = real_merge_time := !OM.merge_time in
      res

    let merge = Irmin.Merge.(option (v t merge))
  end

  (* Store is defined as follows which is a module *)
  module BC_store = struct
    module Store = Irmin_unix.Git.Mem.KV(BC_value)
    module Sync = Irmin.Sync(Store)
    module Status = Store.Status
    module Head = Store.Head

    type t = Store.t (* = branch *)

    type path = string list

    let init ?root ?bare () =
      let config = Irmin_git.config Config.root in
      Store.Repo.v config

    let master (repo:Store.repo) = Store.master repo

    let clone t name = Store.clone t name

    let get_branch r ~branch_name = Store.of_branch r branch_name

    let merge s ~into = Store.merge s ~into

    (*let update t k v = Store.set t k v*)

    let read t (p:path) = Store.find t p

    let string_of_path p = String.concat "/" p

    let info s = Irmin_unix.info "[repo %s] %s" Config.root s

    let status t = Store.status t

    let rec update ?msg t (p:path) (v:BC_value.t) = 
      let msg = match msg with
        | Some s -> s
        | None -> "Setting "^(string_of_path p) in
      Store.set t p v ~info:(info msg)

    let pp = Fmt.using Store.status Store.Status.pp
  end

(* Vpst is a module which consist of type store, st and 'a t *)
  module type VPST = sig
    type 'a t
    type branch
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val with_init_version_do : OM.t -> 'a t -> 'a
    val fork_version : ?parent:branch -> 'a t -> branch t
    val set_parent: branch -> unit t
    val get_latest_version : unit -> OM.t t
    val sync_next_version : ?v:OM.t -> OM.t t
    val liftLwt : 'a Lwt.t -> 'a t
    val print_info: unit t
  end

  module Vpst : VPST = struct
    type branch = BC_store.t
    (* st is a record type with fields as master, local, name and next_id *)
    type st = {master   : branch;
               parent   : branch;
               local    : branch;
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
          BC_value.of_adt v >>= fun (v':BC_value.t) ->
          BC_store.update ~msg:"initial version" 
                          m_br path v' >>= fun () ->
          BC_store.clone m_br "1_local" >>= fun t_br ->
          let st = {master=m_br; parent=m_br; 
                    local=t_br; name="1"; next_id=1} in
          m st >>= fun (a,_) -> Lwt.return a
        end

      let fork_version ?parent (m : 'a t) = fun (st : st) ->
        let child_name =
          st.name ^ ("_" ^ (string_of_int st.next_id)) in
        let m_br = st.master in
        BC_store.clone m_br (child_name ^ "_local") >>= fun t_br ->
        let p_br = match parent with
          | Some br -> br
          | None -> st.local in
        let new_st = { master = m_br; parent = p_br; 
                       local = t_br; name = child_name; 
                       next_id = 1} in
        Lwt.async (fun () -> m new_st);
        Lwt.return (t_br, { st with next_id = (st.next_id + 1) })


    let get_latest_version () : OM.t t = fun (st: st) ->
      (*let status = BC_store.status st.local in
      let bs = Fmt.to_to_string BC_store.Status.pp status in 
      let ps = BC_store.string_of_path path in 
      let _ = printf "Branch: %s, path: %s\n" bs ps in*)
      BC_store.read st.local path >>= fun (vop:BC_value.t option) ->
      let v = from_just vop "get_latest_version"  in
      BC_value.to_adt v >>= fun td ->
      Lwt.return (td,st)

    let set_parent parent = fun (st:st) ->
      Lwt.return ((), {st with parent=parent})

    let sync_next_version ?v = fun (st: st) ->
      (* 1. Commit to the local branch *)
      (match v with 
       | None -> Lwt.return ()
       | Some v -> 
         BC_value.of_adt v >>= fun v' -> 
         BC_store.update ~msg:"Committing local state" 
                         st.local path v') >>= fun () ->
      (* 2. Merge parent to the local branch *)
      let cinfo = info "Merging parent into local" in
      Lwt_unix.sleep @@ 0.1 *. (float @@ Random.int 5) >>= fun _ ->
      BC_store.Head.find st.parent >>= fun commitop ->
      let latest_commit = match commitop with
        | Some p -> p
        | None -> failwith "Parent has no commits!" in
      BC_store.Head.merge latest_commit
              ~into:st.local ~info:cinfo >>= fun _ ->
      get_latest_version () st

    let liftLwt (m: 'a Lwt.t) : 'a t = fun st ->
      m >>= fun a -> Lwt.return (a,st)

    let print_info = fun (st:st) ->
      let str = Fmt.to_to_string BC_store.pp st.local in
      let pstr = Fmt.to_to_string BC_store.pp st.parent in
      begin
        Lwt_io.printf "I am: %s\n My parent: %s\n" 
                                      str pstr >>= fun () ->
        Lwt.return ((),st)
      end
	end 
 
end






