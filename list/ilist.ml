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

  module type TAG_TREE = sig
    type t
    type tag
    val tag_of_string: string -> tag
    val tag_of_hash: K.t -> tag
    val empty: unit -> t
    val add: t -> tag -> AO_value.t -> t Lwt.t
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

    let add_and_link (type a) (module T:TAG_TREE with type t=a) 
                     t v (tree:a) : (K.t*a) Lwt.t=
      (S.add t v) >>= fun k ->
      let tag = T.tag_of_hash k in
      T.add tree tag v >>= fun tree' ->
      Lwt.return (k,tree')

    let rec add_adt : type a. (module TAG_TREE with type t=a) -> t 
                                -> OM.t -> (a -> (K.t*a) Lwt.t) =
    fun  (module T) t (adt:OM.t) ->
      (*
       * We momentarily override Lwt's bind and return so as to pass
       * the tree around without making a mess.
       *)
      let (>>=) m f = 
        fun tr -> m tr >>= fun (a,tr') -> f a tr' in
      let add_to_store (v:AO_value.t) = fun tr ->
        add_and_link (module T:TAG_TREE with type t = a) t v tr in
      let add_adt = add_adt (module T:TAG_TREE with type t = a) t in
        (match adt with
         | n::rt -> 
           (add_adt rt >>= fun rt' ->
            add_to_store @@ Cons (n,rt'))
         | [] -> add_to_store @@ Nil)

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
    val of_adt : (module TAG_TREE with type t = 'a) -> 
      OM.t -> 'a -> (t*'a) Lwt.t
    val to_adt: t -> OM.t Lwt.t
  end
 
  module type IRMIN_STORE = 
  sig
    type t
    type repo
    type path = string list
    type tree
    module Sync:Irmin.SYNC with type db = t
    module Tree: TAG_TREE with type t = tree
    val init : ?root:'a -> ?bare:'b -> unit -> repo Lwt.t
    val master : repo -> t Lwt.t
    val clone : t -> string -> t Lwt.t
    val get_branch : repo -> branch_name:string -> t Lwt.t
    val merge : t ->
      into:t ->
      info:Irmin.Info.f -> (unit, Irmin.Merge.conflict) result Lwt.t
    val read : t -> path -> vt option Lwt.t
    val info : string -> Irmin.Info.f
    val update : ?msg:string -> t -> path -> vt -> unit Lwt.t
    val with_tree : t -> path -> info:Irmin.Info.f ->
                    (tree option -> tree option Lwt.t) -> unit Lwt.t
  end
 
  module rec BC_value: IRMIN_STORE_VALUE with type t = vt = struct
    include AO_value

    let of_adt : type a. (module TAG_TREE with type t = a) -> OM.t
                         -> a -> (t*a) Lwt.t = fun (module T) adt ->
     (*
      * Momentarily overriding Lwt's bind and return with our own
      * bind and return to pass around the tree.
      *)
     let return x = fun tr -> Lwt.return (x,tr) in
     let lift m = fun tr -> m >>= fun x -> Lwt.return (x,tr) in
     let (>>=) m f = 
       fun tr -> m tr >>= fun (a,tr') -> f a tr' in
     lift (AO_store.create ()) >>= fun ao_store -> 
     let aostore_add =
       AO_store.add_adt (module T) ao_store in
     match adt with
      | n::rt -> 
        (aostore_add rt >>= fun rt' ->
         return @@ Cons (n,rt'))
      | [] -> return @@ Nil

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
          let merged_v = ref v1 in (* Hack! see below. *)
          to_adt old >>= fun oldv  ->
          to_adt v1 >>= fun v1  ->
          to_adt v2 >>= fun v2 ->
          let v = OM.merge oldv v1 v2 in
          BC_store.init () >>= fun repo ->
          BC_store.master repo >>= fun t ->
          BC_store.with_tree t ["state"]
            ~info:(BC_store.info "Mergefn")
            begin fun trop ->
              let tr = from_just trop "merge.trop" in
              let tmod = (module BC_store.Tree : 
                           TAG_TREE with type t = BC_store.tree) in
              of_adt tmod v tr >>= fun (v',tr') ->
              let _ = merged_v := v' in
              Lwt.return @@ Some tr'
            end >>= fun () ->
          Irmin.Merge.ok !merged_v
        end in
      let t2 = Sys.time () in
      let _ = merge_time := !merge_time +. (t2-.t1) in
      let _ = merge_count := !merge_count + 1 in
      let _ = real_merge_time := !OM.merge_time in
      res

    let merge = Irmin.Merge.(option (v t merge))
  end

  (* Store is defined as follows which is a module *)
  and BC_store : IRMIN_STORE = struct
    module Store = Irmin_unix.Git.FS.KV(BC_value)
    module Sync = Irmin.Sync(Store)
    module Status = Store.Status

    type t = Store.t (* = branch *)
    type repo = Store.repo
    type tree = Store.tree
    type path = string list

    module Tree = 
      struct
        type t = Store.tree

        type tag = string list

        let empty () = Store.Tree.empty

        let tag_of_string str = [str]

        let tag_of_hash k = 
          let sha_str = Fmt.to_to_string Irmin.Hash.SHA1.pp k in
          let fname_k = String.sub sha_str 0 10 in
            [fname_k]

        let add t k v = Store.Tree.add t k v
      end

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

    let with_tree t path ~info f = Store.with_tree t path f
                                    ~info:info
                                    ~strategy:`Set

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
    val return : 'a -> 'a t
    val bind: 'a t -> ('a -> 'b t) -> 'b t
    val with_init_version_do: OM.t -> 'a t -> 'a 
    val with_remote_version_do: string -> 'a t -> 'a
    val get_latest_version: unit -> OM.t t
    val sync_next_version: ?v:OM.t -> string list -> OM.t t
    val liftLwt: 'a Lwt.t -> 'a t
    val pull_remote: string -> unit t
  end

  module Vpst : VPST = struct
    type store = BC_store.t
    (* st is a record type with fields as master, local, name and next_id *)
    type st = {master   : store;
               name     : string;
               next_id  : int;
               seq_no    : int}
    type 'a t = st -> ('a * st) Lwt.t

    let info s = Irmin_unix.info "[repo %s] %s" Config.root s  

    let path = ["state"]

    let return (x : 'a) : 'a t = fun st -> Lwt.return (x,st)

    let bind (m1: 'a t) (f: 'a -> 'b t) : 'b t = 
      fun st -> (m1 st >>= fun (a,st') -> f a st')
    
    let with_init_version_do (v : OM.t) (m : 'a t) =
      Lwt_main.run
      begin 
        BC_store.init () >>= fun repo ->
        BC_store.master repo >>= fun m_br ->
        BC_store.with_tree m_br ["state"]
          ~info:(BC_store.info "Initial version")
          begin fun trop ->
            let module Tree = BC_store.Tree in
            let tr = match trop with 
              | Some tr -> tr
              | None -> Tree.empty () in
            let tmod = (module Tree : TAG_TREE 
                         with type t = BC_store.tree) in
            BC_value.of_adt tmod v tr >>= fun (v',tr') ->
            let head_tag = Tree.tag_of_string "head" in
            Tree.add tr' head_tag v' >>= fun tr'' ->
            Lwt.return @@ Some tr''
          end >>= fun () ->
        let st = { master = m_br; name = "1"; 
                   next_id = 1; seq_no = 1 } in
        m st >>= (fun (a, _) -> Lwt.return a)
      end

    let get_latest_version () =
      (fun (st : st) ->
         (BC_store.read st.master (*st.local*) ["state"; "head"]) >>=
           (fun (vop : BC_value.t option) ->
              let v = from_just vop "get_latest_version" in
              (BC_value.to_adt v) >>= (fun td -> Lwt.return (td, st))) : 
      OM.t t)

    let pull_remote remote_uri = fun (st: st) ->
      (* Pull and merge remote to master *)
      let cinfo = info (sprintf "Merging remote(%s) to local master" 
                          remote_uri) in
      let remote = Irmin.remote_uri remote_uri in
      let _ = printf "Pulling from %s\n" remote_uri in
      let _ = flush_all () in
      BC_store.Sync.pull st.master remote 
                            (`Merge  cinfo) >>= fun res -> 
      (match res with
          | Ok _ -> Lwt.return ((),st)
          | Error _ -> failwith "Error while pulling the remote")

    let with_remote_version_do remote_uri m = 
      Lwt_main.run 
        begin
          BC_store.init () >>= fun repo -> 
          BC_store.master repo >>= fun m_br -> 
          let remote = Irmin.remote_uri remote_uri in
          BC_store.Sync.pull m_br remote `Set >>= fun res ->
          (match res with
              | Ok _ -> Lwt.return ()
              | Error _ -> failwith "Error while \
                                     pulling the remote") >>= fun _ ->
          let st = {master=m_br; name="1"; 
                    next_id=1; seq_no=1} in
          m st >>= fun (a,_) -> Lwt.return a
        end
      (* Fork master from remote master *)

    let sync_next_version ?v (uris:string list) = fun (st:st) ->
      try
        (* 1. Commit to the master branch *)
        (match v with 
         | None -> Lwt.return ()
         | Some v -> 
           BC_store.with_tree st.master ["state"]
             ~info:(info @@
                    sprintf "%d. setting latest version" st.seq_no)
             begin fun trop ->
               let module Tree = BC_store.Tree in
               let tr = match trop with
                 | Some tr -> tr
                 | None -> Tree.empty () in
                let tmod = (module Tree : TAG_TREE 
                             with type t = BC_store.tree) in
                BC_value.of_adt tmod v tr >>= fun (v',tr') ->
                let head_tag = Tree.tag_of_string "head" in
                Tree.add tr' head_tag v' >>= fun tr'' ->
                Lwt.return @@ Some tr''
             end) >>= fun () ->
        (* 2. Pull remotes to master *)
        let pull_vpst = List.fold_left 
            (fun (pre: unit t) uri -> 
              bind pre 
                  (fun () -> 
                    bind (pull_remote uri) (fun () ->
                    return ()))) 
            (return ()) uris in
        pull_vpst st >>= fun ((),st') ->
        (*
        (* 3. Merge local master to the local branch *)
        let cinfo = info "Merging master into local" in
        BC_store.merge st'.master ~into:st'.local ~info:cinfo >>= fun _ ->
        (* 4. Merge local branch to the local master *)
        let cinfo = info "Merging local into master" in
        BC_store.merge st'.local ~into:st'.master ~info:cinfo >>= fun _ ->
        *)
        get_latest_version () {st' with seq_no = st'.seq_no + 1}
      with _ -> failwith "Some error occured"
        
    let liftLwt (m: 'a Lwt.t) : 'a t = fun st ->
      m >>= fun a -> Lwt.return (a,st)
	end 
 
end

