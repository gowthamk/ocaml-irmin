open Lwt.Infix
open Irmin_unix
open Printf
open Msigs

let from_just op msg = match op with
  | Some x -> x
  | None -> failwith @@ msg^": Expected Some. Got None."

module MakeVersioned (Config: CONFIG) 
                     (Key: Rbmap.KEY)
                     (Value: Rbmap.VALUE)
                     (OM: Rbmap.S with type key=Key.t 
                                   and type value=Value.t)
                     (V: IRMIN_DATA_STRUCTURE
                         with type adt = Value.t) :
              IRMIN_DATA_STRUCTURE with type adt = OM.t  = 
struct
  (*module OM = Rbmap.Make(Key)(Value)*)
  module K = Irmin.Hash.SHA1

  type adt = OM.t

  type targ = (K.t * (Key.t *(V.t * K.t)))
  type my_t =
    | Black of targ
    | Red of targ
    | Empty
  type t = 
    | Me of my_t
    | Child of V.t

  type madt = my_t
  type boxed_t = t

  let targ  = 
    let open Irmin.Type in 
    (pair K.t (pair Key.t (pair V.t K.t)))

  let my_t =
    let open Irmin.Type in
    variant "my_t" (fun b r e -> function
        | Black a  -> b a
        | Red a -> r a
        | Empty -> e)
    |~ case1 "Black" targ (fun x -> Black x)
    |~ case1 "Red" targ (fun x -> Red x)
    |~ case0 "Empty" Empty
    |> sealv

  let t = 
    let open Irmin.Type in
    variant "t" (fun m c -> function
        | Me a  -> m a
        | Child a -> c a)
    |~ case1 "Me" my_t (fun x -> Me x)
    |~ case1 "Child" V.t (fun x -> Child x)
    |> sealv

  module AO_value : Irmin.Contents.Conv with type t = t = struct
    type t = boxed_t

    let t = t

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

  module type MY_TREE = TAG_TREE with type value=t

  module type V_TREE = TAG_TREE with type value=V.t

  let my_tree_to_v_tree : type a b. (module MY_TREE with type t=a 
                                                     and type tag=b)
                                -> (module V_TREE with type t=a 
                                                   and type tag=b) =
    fun (module T) ->
      let module Vtree = struct
          type t = T.t
          type tag = T.tag
          type value = V.t
          let tag_of_string = T.tag_of_string
          let tag_of_hash = T.tag_of_hash
          let empty = T.empty
          let add t tag vt = 
            T.add t tag (Child vt)
        end in
      (module Vtree: V_TREE with type t=T.t 
                             and type tag=T.tag)

  module AO_store : AO_STORE with type adt=adt 
                              and type value=t = struct
    (* Immutable collection of all versionedt *)
    module S = Irmin_git.AO(Git_unix.FS)(AO_value)
    include S

    type adt=OM.t

    let create config =
      let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
          "level" Irmin.Private.Conf.(some int) None
      in
      let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
      let level = Irmin.Private.Conf.get config level in
      Git_unix.FS.create ?root ?level ()

    (* Creates a Git backend *)
    let create () = create @@ Irmin_git.config Config.root

    let add_and_link (type a) (module T:MY_TREE with type t=a) 
                     t v (tree:a) : (K.t*a) Lwt.t=
      (S.add t v) >>= fun k ->
      let tag = T.tag_of_hash k in
      T.add tree tag v >>= fun tree' ->
      Lwt.return (k,tree')

  (*
   * We can memoize the results of add_adt and read_adt for faster
   * processing. add_adt can use physicaly equality on OM.t objects
   * for faster lookups. read_adt memoization is straightforward.
   *)
  let rec add_adt : type a. (module MY_TREE with type t=a) ->
           t -> adt -> (a -> (K.t*a) Lwt.t) =
    fun  (module T) t (adt:adt) ->
      let vtree = my_tree_to_v_tree (module T) in
      let module Vtree = (val vtree : V_TREE with type t=T.t 
                                              and type tag=T.tag) in
      let add_to_store (v:my_t) = 
        fun tr ->
          add_and_link (module T:MY_TREE with type t = a) 
                       t (Me v) tr >>= fun (k,tr') -> 
          Lwt.return (k,tr') in
      let of_vadt vadt = fun tr ->
        V.of_adt (module Vtree: V_TREE with type t=T.t) vadt tr in
      let add_adt = add_adt (module T:MY_TREE with type t = a) t in
      (*
       * We momentarily override Lwt's bind and return so as to pass
       * the tree around without making a mess.
       *)
      let (>>=) m f = fun tr -> 
        m tr >>= fun (k,tr') -> f k tr' in
      begin 
        match adt with
         | OM.Black (lt,n,vadt,rt) -> 
           (add_adt lt >>= fun lt' ->
            add_adt rt >>= fun rt' ->
            of_vadt vadt >>= fun v ->
            add_to_store @@ Black (lt',(n,(v,rt'))))
          | OM.Red (lt,n,vadt,rt) -> 
           (add_adt lt >>= fun lt' ->
            add_adt rt >>= fun rt' ->
            of_vadt vadt >>= fun v ->
            add_to_store @@ Red (lt',(n,(v,rt'))))
         | OM.Empty -> add_to_store @@ Empty
      end

  let rec read_adt t (k:K.t) : adt Lwt.t =
    find t k >>= fun aop ->
    let to_vadt v = V.to_adt v in
    let a = from_just aop "to_adt" in
    (match a with
      | Me (Black (lt, (n, (v, rt)))) ->
        (read_adt t lt >>= fun lt' ->
         read_adt t rt >>= fun rt' ->
         to_vadt v >>= fun vadt ->
         Lwt.return @@ OM.Black (lt', n, vadt, rt'))
      | Me (Red (lt, (n, (v, rt)))) ->
        (read_adt t lt >>= fun lt' ->
         read_adt t rt >>= fun rt' ->
         to_vadt v >>= fun vadt ->
         Lwt.return @@ OM.Red (lt', n, vadt, rt'))
      | Me Empty -> Lwt.return @@ OM.Empty
      | Child _ -> failwith "read_adt.Exhaustiveness")

    let find_or_fail t (k:K.t) : AO_value.t Lwt.t =
      find t k >>= fun vop ->
      Lwt.return @@ from_just vop "find_or_fail"
  end

  module rec BC_value: IRMIN_STORE_VALUE with type t = t 
                                          and type adt=adt = struct
    include AO_value

    type adt=OM.t
    
    let of_adt : type a. (module MY_TREE with type t = a) -> adt
                         -> (a) -> (t*a) Lwt.t = 
      fun (module T) adt ->
        let vtree = my_tree_to_v_tree (module T) in
        let module Vtree = (val vtree : V_TREE with type t=T.t 
                                          and type tag=T.tag) in
        let of_vadt vadt = fun tr ->
          V.of_adt (module Vtree) vadt tr >>= fun (v,tr') ->
          Lwt.return (v,tr') in
        (*
         * Momentarily overriding Lwt's bind and return with our own
         * bind and return to pass around the tree.
         *)
        let return x = fun tr -> Lwt.return (x,tr) in
        let lift m = 
          fun tr -> m >>= fun x -> 
                    Lwt.return (x,tr) in
        let (>>=) m f = fun (tr) -> 
            m tr >>= fun (k,tr') -> f k tr' in
        lift (AO_store.create ()) >>= fun ao_store -> 
        let aostore_add =
          AO_store.add_adt (module T) ao_store in
        match adt with
         | OM.Black (lt,n,vadt,rt) -> 
           (aostore_add lt >>= fun lt' ->
            aostore_add rt >>= fun rt' ->
            of_vadt vadt >>= fun v ->
            return @@ Me (Black (lt',(n,(v,rt')))))
         | OM.Red (lt,n,vadt,rt) -> 
           (aostore_add lt >>= fun lt' ->
            aostore_add rt >>= fun rt' ->
            of_vadt vadt >>= fun v ->
            return @@ Me (Red (lt',(n,(v,rt')))))
         | OM.Empty -> return @@ Me Empty

    let to_adt (t:t) : adt Lwt.t =
      let to_vadt v = V.to_adt v in
      AO_store.create () >>= fun ao_store ->
      let aostore_read k =
        AO_store.read_adt ao_store k in
      match t with
        | Me (Black (lt,(n,(v,rt)))) ->
          (aostore_read lt >>= fun lt' ->
           aostore_read rt >>= fun rt' ->
           to_vadt v >>= fun vadt ->
           Lwt.return @@ OM.Black (lt',n,vadt,rt'))
        | Me (Red (lt,(n,(v,rt)))) ->
          (aostore_read lt >>= fun lt' ->
           aostore_read rt >>= fun rt' ->
           to_vadt v >>= fun vadt ->
           Lwt.return @@ OM.Red (lt',n,vadt,rt'))
        | Me Empty -> Lwt.return @@ OM.Empty
        | Child _ -> failwith "to_adt.exhaustiveness"


    (*
    let rec sorted_list_diff l v = 
      let cmp = Atom.compare in 
      match l,v with
      | [],[] -> ([],[])
      | _,[] -> ([],l)
      | [],_ -> (v,l)
      | x::xs, y::ys when cmp x y = 0 -> sorted_list_diff xs ys
      | x::xs, y::ys when cmp x y < 0 -> sorted_list_diff xs v |>
                                          fun (a,r) -> (a,x::r)
      | x::xs, y::ys when cmp x y > 0 -> sorted_list_diff l ys |>
                                          fun (a,r) -> (y::a,r)
      | _,_ -> failwith "Just to satisfy exhaustiveness checker"

    let naively_compute_diff l v = 
      sorted_list_diff (OM.elements l) (OM.elements v)

    let rec compute_diff l v = 
      match l,v with
        | Empty, _  -> Lwt_main.run @@
          begin
            to_adt v >>= fun v ->
            Lwt.return (OM.elements v, [])
          end
        | _, Empty -> Lwt_main.run @@
          begin 
            to_adt l >>= fun l ->
            Lwt.return ([], OM.elements l)
          end
        | Black (lk1,(a1,rk1)), Black (lk2,(a2,rk2))
        | Red (lk1,(a1,rk1)), Red (lk2,(a2,rk2))
        | Black (lk1,(a1,rk1)), Red (lk2,(a2,rk2)) 
        | Red (lk1,(a1,rk1)), Black (lk2,(a2,rk2)) -> 
          begin 
            match lk1=lk2, Atom.compare a1 a2, rk1=rk2 with
            | true, 0, true -> ([],[])
            | false, 0, true -> Lwt_main.run @@
              begin
                AO_store.create () >>= fun ao_store ->
                AO_store.find_or_fail ao_store lk1 >>= fun l1 ->
                AO_store.find_or_fail ao_store lk2 >>= fun l2 ->
                Lwt.return @@ compute_diff l1 l2 
              end
            | true, 0, false -> Lwt_main.run @@
              begin
                AO_store.create () >>= fun ao_store ->
                AO_store.find_or_fail ao_store rk1 >>= fun r1 ->
                AO_store.find_or_fail ao_store rk2 >>= fun r2 ->
                Lwt.return @@ compute_diff r1 r2 
              end
            | true, _, _ -> Lwt_main.run @@ 
              begin 
                AO_store.create () >>= fun ao_store ->
                AO_store.read_adt ao_store rk1 >>= fun r1 ->
                AO_store.read_adt ao_store rk2 >>= fun r2 ->
                Lwt.return @@ naively_compute_diff 
                  (OM.Black (OM.Empty, a1, r1))
                  (OM.Black (OM.Empty, a2, r2))
              end
            | _, _, true -> Lwt_main.run @@ 
              begin 
                AO_store.create () >>= fun ao_store ->
                AO_store.read_adt ao_store lk1 >>= fun l1 ->
                AO_store.read_adt ao_store lk2 >>= fun l2 ->
                Lwt.return @@ naively_compute_diff 
                  (OM.Black (l1, a1, OM.Empty))
                  (OM.Black (l2, a2, OM.Empty))
              end
            | _, _, _ -> Lwt_main.run @@
              begin 
                AO_store.create () >>= fun ao_store ->
                AO_store.read_adt ao_store lk1 >>= fun l1 ->
                AO_store.read_adt ao_store lk2 >>= fun l2 ->
                AO_store.read_adt ao_store rk1 >>= fun r1 ->
                AO_store.read_adt ao_store rk2 >>= fun r2 ->
                Lwt.return @@ naively_compute_diff 
                  (OM.Black (l1, a1, r1))
                  (OM.Black (l2, a2, r2))
              end
          end

    let rec apply_diff adds removes v = match adds,removes with
      | x::xs, y::ys -> apply_diff xs ys @@ OM.remove y (OM.add x v)
      | x::xs, [] -> apply_diff xs [] @@ OM.add x v
      | [], y::ys -> apply_diff [] ys @@ OM.remove y v
      | [], [] -> v

    let rec merge ~(old:t Irmin.Merge.promise) (v1:t) (v2:t) =
      if v1=v2 then Irmin.Merge.ok v1
      else begin
        let _ = printf "Merge called\n" in
        let open Irmin.Merge.Infix in
        old () >>=* fun old ->
        let old = from_just old "merge" in
        let merged_v = ref v1 in (* Hack! see below. *)
        let (adds,removes) = compute_diff old v2 in
        to_adt v1 >>= fun v1 ->
        let v = apply_diff adds removes v1 in
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
      end
      *)

    let merge ~old v1 v2 = failwith "Unimpl."
    let merge = Irmin.Merge.(option (v t merge))
  end

  and BC_store : IRMIN_STORE with type value = t = struct
    module Store = Irmin_unix.Git.FS.KV(BC_value)
    module Sync = Irmin.Sync(Store)
    module Status = Store.Status

    type t = Store.t (* = branch *)
    type repo = Store.repo
    type tree = Store.tree
    type path = string list
    type value = boxed_t

    module Tree = 
      struct
        type t = Store.tree

        type tag = string list

        type value = boxed_t

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

    let read t (p:path) = 
      Store.find t p (*>>= fun vop ->
      Lwt.return (match vop with
      | None -> None
      | Some (Me m) -> Some m
      | Some (Child _) -> failwith "BC_store.read.exhaustiveness")*)

    let string_of_path p = String.concat "/" p

    let info s = Irmin_unix.info "[repo %s] %s" Config.root s

    let with_tree t path ~info f = Store.with_tree t path f
                                    ~info:info
                                    ~strategy:`Set

    let status t = Store.status t

    let rec update ?msg t (p:path) (v:boxed_t) = 
      let msg = match msg with
        | Some s -> s
        | None -> "Setting "^(string_of_path p) in
      Store.set t p v ~info:(info msg)
  end

  (*
   * The following to make rbmap an irmin data structure
   *)
  let of_adt = BC_value.of_adt

  let to_adt = BC_value.to_adt

  let merge = BC_value.merge

  let of_string = AO_value.of_string

  let pp = AO_value.pp


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
    
    let with_init_version_do (v : adt) (m : 'a t) =
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
            let tmod = (module Tree : MY_TREE 
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
           (fun (vop : boxed_t option) ->
              let v = from_just vop "get_latest_version" in
              (BC_value.to_adt v) >>= fun td -> 
              Lwt.return (td, st)) : 
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
                let tmod = (module Tree : MY_TREE 
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
