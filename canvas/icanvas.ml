open Printf
open Lwt.Infix
open Irmin_unix
module OM = Canvas.Make
module Canvas = OM
open OM
module K = Irmin.Hash.SHA1
module G = Git_unix.FS
module type Config  =
sig val root : string val shared : string val init : unit -> unit end
let from_just op msg =
match op with
| Some x -> x
| None -> failwith @@ (msg ^ ": Expected Some. Got None.")
module MakeVersioned(Config:Config) =
struct
  type node = {
    tl_t: K.t ;
    tr_t: K.t ;
    bl_t: K.t ;
    br_t: K.t }
  and madt =
    | B of node 
    | N of pixel 
  module IrminConvert =
    struct
      let pixel =
        let open Irmin.Type in
          ((((record "pixel" (fun r -> fun g -> fun b -> { r; g; b }))
               |+ (field "r" int32 (fun t -> t.r)))
              |+ (field "g" int32 (fun t -> t.g)))
             |+ (field "b" int32 (fun t -> t.b)))
            |> sealr
      let mknode t =
        let open Irmin.Type in
          (((((record "node"
                 (fun tl_t ->
                    fun tr_t ->
                      fun bl_t ->
                        fun br_t -> { tl_t; tr_t; bl_t; br_t }))
                |+ (field "tl_t" K.t (fun t -> t.tl_t)))
               |+ (field "tr_t" K.t (fun t -> t.tr_t)))
              |+ (field "bl_t" K.t (fun t -> t.bl_t)))
             |+ (field "br_t" K.t (fun t -> t.br_t)))
            |> sealr
      and mkmadt node =
        let open Irmin.Type in
          (((variant "madt"
               (fun b ->
                  fun n -> function | B a0 -> b a0 | N a0 -> n a0))
              |~ (case1 "B" node (fun x -> B x)))
             |~ (case1 "N" pixel (fun x -> N x)))
            |> sealv
    end
  module IrminConvertTie =
    struct
      let () = ()
      let () = ()
      and (node, madt) =
        let open Irmin.Type in
          mu2
            (fun node ->
               fun madt ->
                 ((IrminConvert.mknode madt),
                   (IrminConvert.mkmadt node)))
    end
  module AO_value : (Irmin.Contents.Conv with type  t =  madt) =
  struct
    type t = madt
    let t = IrminConvertTie.madt
    let pp = Irmin.Type.pp_json ~minify:false t
    let of_string s =
      let decoder = Jsonm.decoder (`String s) in
      let res =
        try Irmin.Type.decode_json t decoder
        with
        | Invalid_argument s ->
            failwith @@
              (Printf.sprintf
                 "AO_Value.of_string: Invalid_argument: %s" s) in
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

  module AO_store = 
  struct
    module S = Irmin_git.AO(Git_unix.FS)(AO_value)
    include S

    let create config =
      let level =
        Irmin.Private.Conf.key ~doc:"The Zlib compression level."
          "level" (let open Irmin.Private.Conf in some int) None in
      let root =
        Irmin.Private.Conf.get config Irmin.Private.Conf.root in
      let level = Irmin.Private.Conf.get config level in
      G.create ?root ?level ()

    let create () = create @@ (Irmin_git.config Config.root)

    (*let on_add = ref (fun k v -> printf "%s\n" 
                                   (Fmt.to_to_string K.pp k); 
                                 Lwt.return ())*)

    let add_and_link (type a) (module T:TAG_TREE with type t=a) 
                     t v (tree:a) : (K.t*a) Lwt.t=
      (S.add t v) >>= fun k ->
      let tag = T.tag_of_hash k in
      T.add tree tag v >>= fun tree' ->
      Lwt.return (k,tree')

    module PHashtbl = Hashtbl.Make(struct 
        type t = OM.t
        let equal x y = x == y
        let hash x = Hashtbl.hash_param 2 10 x
      end)

    let (read_cache: (K.t, OM.t) Hashtbl.t) = Hashtbl.create 5051

    let (write_cache: K.t PHashtbl.t) = PHashtbl.create 5051
        
    let rec add_adt : type a. (module TAG_TREE with type t=a) -> t 
                                -> OM.t -> (a -> (K.t*a) Lwt.t) =
    fun  (module T) t (adt:OM.t) ->
      (*
       * We momentarily override Lwt's bind and return so as to pass
       * the tree around without making a mess.
       *)
      let (>>=) m f = 
        fun tr -> m tr >>= fun (a,tr') -> f a tr' in
      let return x = fun tr -> Lwt.return (x,tr) in
      try 
        return  @@ PHashtbl.find write_cache adt
      with Not_found -> begin 
        let add_to_store (v:madt) = fun tr ->
          add_and_link (module T:TAG_TREE with type t = a) t v tr in
        let add_adt = add_adt (module T:TAG_TREE with type t = a) t in
        match adt with
         | OM.N {r;g;b} -> add_to_store @@ N {r;g;b}
         | OM.B {tl_t;tr_t;bl_t;br_t} -> 
             add_adt tl_t >>= fun tl_t' ->
             add_adt tr_t >>= fun tr_t' ->
             add_adt bl_t >>= fun bl_t' ->
             add_adt br_t >>= fun br_t' ->
             add_to_store @@ B {tl_t=tl_t'; tr_t=tr_t'; 
                                bl_t=bl_t'; br_t=br_t'}
      end

    let rec read_adt t (k:K.t) : OM.t Lwt.t =
      try 
        Lwt.return @@ Hashtbl.find read_cache k
      with Not_found -> begin 
        find t k >>= fun aop ->
        let a = from_just aop "to_adt" in
        match a with
          | N {r;g;b} -> Lwt.return @@ OM.N {r;g;b}
          | B {tl_t;tr_t;bl_t;br_t} ->
            (read_adt t tl_t >>= fun tl_t' ->
             read_adt t tr_t >>= fun tr_t' ->
             read_adt t bl_t >>= fun bl_t' ->
             read_adt t br_t >>= fun br_t' ->
             Lwt.return @@ OM.B {OM.tl_t=tl_t'; OM.tr_t=tr_t'; 
                                 OM.bl_t=bl_t'; OM.br_t=br_t'})
      end
  end

  module type IRMIN_STORE_VALUE  =
    sig
      include Irmin.Contents.S
      val of_adt : (module TAG_TREE with type t = 'a) -> 
        Canvas.t -> 'a -> (t*'a) Lwt.t
      val to_adt : t -> Canvas.t Lwt.t
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
    val read : t -> path -> madt option Lwt.t
    val info : string -> Irmin.Info.f
    val update : ?msg:string -> t -> path -> madt -> unit Lwt.t
    val with_tree : t -> path -> info:Irmin.Info.f ->
                    (tree option -> tree option Lwt.t) -> unit Lwt.t
  end

  let merge_time = ref 0.0
  let merge_count = ref 0

  module rec BC_value : (IRMIN_STORE_VALUE with type  t =  madt) =
  struct
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
      | OM.N {r;g;b} -> return @@ N {r;g;b}
      | OM.B {tl_t;tr_t;bl_t;br_t} -> 
        (aostore_add tl_t >>= fun tl_t' ->
         aostore_add tr_t >>= fun tr_t' ->
         aostore_add bl_t >>= fun bl_t' ->
         aostore_add br_t >>= fun br_t' ->
         return @@ B {tl_t=tl_t'; tr_t=tr_t'; 
                      bl_t=bl_t'; br_t=br_t'})

    let to_adt (t:t) : OM.t Lwt.t =
      AO_store.create () >>= fun ao_store ->
      let aostore_read k =
        AO_store.read_adt ao_store k in
      match t with
        | N {r;g;b} -> Lwt.return @@ OM.N {r;g;b}
        | B {tl_t;tr_t;bl_t;br_t} ->
          (aostore_read tl_t >>= fun tl_t' ->
           aostore_read tr_t >>= fun tr_t' ->
           aostore_read bl_t >>= fun bl_t' ->
           aostore_read br_t >>= fun br_t' ->
           Lwt.return @@ OM.B {OM.tl_t=tl_t'; OM.tr_t=tr_t'; 
                               OM.bl_t=bl_t'; OM.br_t=br_t'})

    let rec merge ~old:(old : t Irmin.Merge.promise)  (v1 : t)
      (v2 : t) =
      if v1 = v2 then Irmin.Merge.ok v1
      else
        begin 
          let t1 = Sys.time () in
          let open Irmin.Merge.Infix in
          let _ = printf "Merge called\n" in
          let _ = flush_all() in
          let merged_v = ref v1 in (* Hack! see below. *)
          old() >>=* fun old ->
          to_adt (from_just old "merge") >>= fun oldv ->
          to_adt v1 >>= fun v1 ->
          to_adt v2 >>= fun v2 ->
          let v = OM.merge oldv v1 v2 in
          BC_store.init () >>= fun repo ->
          BC_store.master repo >>= fun t ->
          BC_store.with_tree t ["state"]
            ~info:(BC_store.info "merging")
            begin fun trop ->
              let tr = from_just trop "merge.trop" in
              let tmod = (module BC_store.Tree : 
                           TAG_TREE with type t = BC_store.tree) in
              of_adt tmod v tr >>= fun (v',tr') ->
              let _ = merged_v := v' in
              Lwt.return @@ Some tr'
            end >>= fun () ->
          let t2 = Sys.time () in
          let _ = merge_time := !merge_time +. (t2-.t1) in
          let _ = merge_count := !merge_count + 1 in
          Irmin.Merge.ok !merged_v
        end

    let merge = let open Irmin.Merge in option (v t merge)
  end

  and BC_store : IRMIN_STORE =
    struct
      module Store = Irmin_unix.Git.FS.KV(BC_value)
      module Sync = Irmin.Sync(Store)

      module Tree = 
        struct
          type t = Store.tree

          type tag = string list

          let empty () = Store.Tree.empty

          let tag_of_string str = [str]

          let tag_of_hash k = 
            let sha_str = Fmt.to_to_string Irmin.Hash.SHA1.pp k in
            let fname_k = String.sub sha_str 0 7 in
              [fname_k]

          let add t k v = Store.Tree.add t k v
        end

      type t = Store.t
      type repo = Store.repo
      type tree = Store.tree
      type path = string list

      let init ?root  ?bare  () =
        let config = Irmin_git.config Config.root in
        Store.Repo.v config

      let master (repo : Store.repo) = Store.master repo

      let clone t name = Store.clone t name

      let get_branch r ~branch_name  = Store.of_branch r branch_name

      let merge s ~into  = Store.merge s ~into

      let read t (p : path) = Store.find t p

      let string_of_path p = String.concat "/" p

      let info s = Irmin_unix.info "[repo %s] %s" Config.root s;;

      let with_tree t path ~info f = Store.with_tree t path f
                                      ~info:info
                                      ~strategy:`Set

      (*AO_store.on_add := fun k v ->
        begin
          init () >>= fun repo -> 
          master repo >>= fun m_br ->
          let sha_str = Fmt.to_to_string Irmin.Hash.SHA1.pp k in
          let fname_k = String.sub sha_str 0 7 in
          let path_k = [fname_k] in
          let msg = sprintf "Setting %s" fname_k in
          Store.set m_br path_k v ~info:(info msg)
        end*)

      let rec update ?msg  t (p : path) (v : BC_value.t) =
        let msg =
          match msg with
          | Some s -> s
          | None -> "Setting " ^ (string_of_path p) in
        Store.set t p v ~info:(info msg)
    end

  module Vpst :
    sig
      type 'a t
      val return : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
      val with_init_version_do : Canvas.t -> 'a t -> 'a
      val with_remote_version_do : string -> 'a t -> 'a
      (*val fork_version : 'a t -> unit t*)
      val get_latest_version : unit -> Canvas.t t
      val sync_next_version : ?v:Canvas.t -> string list -> Canvas.t t
      val liftLwt : 'a Lwt.t -> 'a t
      val pull_remote : string -> unit t
    end =
    struct
      type store = BC_store.t
      type st =
        {
        master: store ;
        name: string ;
        next_id: int }

      type 'a t = st -> ('a * st) Lwt.t

      let info s = Irmin_unix.info "[repo %s] %s" Config.root s

      (*let path = ["state"]*)

      let return (x : 'a) = (fun st -> Lwt.return (x, st) : 'a t)

      let bind (m1 : 'a t) (f : 'a -> 'b t) =
        (fun st -> (m1 st) >>= (fun (a, st') -> f a st') : 'b t)

      let with_init_version_do (v : Canvas.t) (m : 'a t) =
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
          let st = { master = m_br; name = "1"; next_id = 1 } in
          m st >>= (fun (a, _) -> Lwt.return a)
        end

      let get_latest_version () =
        (fun (st : st) ->
           (BC_store.read st.master (*st.local*) ["state"; "head"]) >>=
             (fun (vop : BC_value.t option) ->
                let v = from_just vop "get_latest_version" in
                (BC_value.to_adt v) >>= (fun td -> Lwt.return (td, st))) : 
        Canvas.t t)

      let pull_remote remote_uri (st : st) =
        try
          let cinfo =
            info
              (Printf.sprintf "Merging remote(%s) to master"
                 remote_uri) in
          let remote = Irmin.remote_uri remote_uri in
          let _ = printf "Pulling from %s\n" remote_uri in
          let _ = flush_all () in
          (BC_store.Sync.pull st.master remote (`Merge cinfo)) >>=
            (fun res ->
               match res with
               | Ok _ -> Lwt.return ((), st)
               | Error _ -> failwith "Error while pulling the remote")
        with _ ->
          begin 
            let _ = printf "Exception raised while pull\n" in
            let _ = flush_all() in
            Lwt.return ((), st)
          end

      let with_remote_version_do remote_uri m =
        Lwt_main.run
          ((BC_store.init ()) >>=
             (fun repo ->
                (BC_store.master repo) >>=
                  (fun m_br ->
                     let remote = Irmin.remote_uri remote_uri in
                     (BC_store.Sync.pull m_br remote `Set) >>=
                       (fun res ->
                          (match res with
                           | Ok _ -> Lwt.return ()
                           | Error _ ->
                               failwith
                                 "Error while pulling the remote")
                            >>=
                            (fun _ ->
                                 (let st =
                                      {
                                        master = m_br;
                                        name = "1";
                                        next_id = 1
                                      } in
                                    (m st) >>=
                                      (fun (a, _) -> Lwt.return a)))))))

      let sync_next_version ?v (uris:string list) = fun (st:st) ->
        try
          (* 1. Commit to the master branch *)
          (match v with 
           | None -> Lwt.return ()
           | Some v -> 
             BC_store.with_tree st.master ["state"]
               ~info:(BC_store.info "setting latest version")
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
          get_latest_version () st'
        with _ -> failwith "Some error occured"
        
      let liftLwt (m : 'a Lwt.t) =
        (fun st -> m >>= (fun a -> Lwt.return (a, st)) : 'a t)
    end 

end
