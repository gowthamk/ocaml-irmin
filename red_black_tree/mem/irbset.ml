open Lwt.Infix
open Irmin_unix
open Printf

module type Config = sig
  val root: string
  val shared: string
  val init: unit -> unit
end

let from_just op msg = match op with
  | Some x -> x
  | None -> failwith @@ msg^": Expected Some. Got None."

module MakeVersioned (Config: Config) (Atom : Rbset.ATOM)  = struct
    module OM = Rbset.Make(Atom)
    module K = Irmin.Hash.SHA1

    type targ = (K.t * (Atom.t * K.t)) 
    and t =
    | Black of targ
    | Red of targ
    | Empty

    type earg = (K.t * (Atom.t * enum))
    and enum = 
    | More of earg 
    | End 

  module AO_value : Irmin.Contents.Conv with type t = t = struct
    type vt = t
    type t = vt

    let mktarg t = let open Irmin.Type in (pair K.t (pair Atom.t K.t))

    let mkt targ =
      let open Irmin.Type in
      variant "t" (fun b r e -> function
          | Black a  -> b a
          | Red a -> r a
          | Empty -> e)
      |~ case1 "Black" targ (fun x -> Black x)
      |~ case1 "Red" targ (fun x -> Red x)
      |~ case0 "Empty" Empty
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

    (* Creates a Git backend *)
    let create () = create @@ Irmin_git.config Config.root

    let on_add = ref (fun k v -> Lwt.return ())

    let add t v = 
      S.add t v >>= fun k ->
      (!on_add) k v >>= fun _ ->
      Lwt.return k

  (*
   * We can memoize the results of add_adt and read_adt for faster
   * processing. add_adt can use physicaly equality on OM.t objects
   * for faster lookups. read_adt memoization is straightforward.
   *)
  let rec add_adt t (a:OM.t) : K.t Lwt.t =
    add t =<<
      (match a with
       | OM.Black (lt,n,rt) -> 
         (add_adt t lt >>= fun lt' ->
          add_adt t rt >>= fun rt' ->
          Lwt.return @@ Black (lt',(n,rt')))
        | OM.Red (lt,n,rt) -> 
         (add_adt t lt >>= fun lt' ->
          add_adt t rt >>= fun rt' ->
          Lwt.return @@ Red (lt',(n,rt')))
       | OM.Empty -> Lwt.return @@ Empty)

  let rec read_adt t (k:K.t) : OM.t Lwt.t =
    find t k >>= fun aop ->
    let a = from_just aop "to_adt" in
    (match a with
      | Black (lt, (n, rt)) ->
        (read_adt t lt >>= fun lt' ->
         read_adt t rt >>= fun rt' ->
         Lwt.return @@ OM.Black (lt', n, rt'))
      | Red (lt, (n, rt)) ->
        (read_adt t lt >>= fun lt' ->
         read_adt t rt >>= fun rt' ->
         Lwt.return @@ OM.Red (lt', n, rt'))
      | Empty -> Lwt.return @@ OM.Empty)

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
 
  module BC_value: IRMIN_STORE_VALUE with type t = t = struct
    include AO_value
    
    let of_adt (a:OM.t) : t Lwt.t  =
      AO_store.create () >>= fun ao_store -> 
      let aostore_add adt =
        AO_store.add_adt ao_store adt in
      match a with
       | OM.Black (lt,n,rt) -> 
         (aostore_add lt >>= fun lt' ->
          aostore_add rt >>= fun rt' ->
          Lwt.return @@ Black (lt',(n,rt')))
       | OM.Red (lt,n,rt) -> 
         (aostore_add lt >>= fun lt' ->
          aostore_add rt >>= fun rt' ->
          Lwt.return @@ Red (lt',(n,rt')))
       | OM.Empty -> Lwt.return @@ Empty

    let to_adt (t:t) : OM.t Lwt.t =
      AO_store.create () >>= fun ao_store ->
      let aostore_read k =
        AO_store.read_adt ao_store k in
      match t with
        | Black (lt,(n,rt)) ->
          (aostore_read lt >>= fun lt' ->
           aostore_read rt >>= fun rt' ->
           Lwt.return @@ OM.Black (lt',n,rt'))
        | Red (lt,(n,rt)) ->
          (aostore_read lt >>= fun lt' ->
           aostore_read rt >>= fun rt' ->
           Lwt.return @@ OM.Red (lt',n,rt'))
        | Empty -> Lwt.return @@ OM.Empty


    let rec sorted_list_diff l v = 
      let cmp = Atom.compare in 
      match l,v with
      | [],[] -> ([],[])
      | _,[] -> ([],l)
      | [],_ -> (v,[])
      | x::xs, y::ys when cmp x y = 0 -> sorted_list_diff xs ys
      | x::xs, y::ys when cmp x y < 0 -> sorted_list_diff xs v |>
                                          fun (a,r) -> (a,x::r)
      | x::xs, y::ys when cmp x y > 0 -> sorted_list_diff l ys |>
                                          fun (a,r) -> (y::a,r)
      | _,_ -> failwith "Just to satisfy exhaustiveness checker"

    let naively_compute_diff l v = 
      let t1 = Sys.time () in
      let res = sorted_list_diff (OM.elements l) (OM.elements v) in
      let t2 = Sys.time () in
      begin 
        real_merge_time := !real_merge_time +. (t2-.t1);
        res
      end

    let rec compute_diff l v = 
      match l,v with
        | Empty, _  -> Lwt_main.run @@
          begin
            to_adt v >>= fun v ->
            Lwt.return @@ naively_compute_diff OM.Empty v
            (*Lwt.return (OM.elements v, [])*)
          end
        | _, Empty -> Lwt_main.run @@
          begin 
            to_adt l >>= fun l ->
            Lwt.return @@ naively_compute_diff l OM.Empty
            (*Lwt.return ([], OM.elements l)*)
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

    let apply_diff adds removes v = 
      let t1 = Sys.time () in
      let res = apply_diff adds removes v in
      let t2 = Sys.time () in
      begin 
        real_merge_time := !real_merge_time +. (t2-.t1);
        res
      end

    let rec merge ~(old:t Irmin.Merge.promise) (v1:t) (v2:t) =
      let _ = Gc.full_major () in
      let t1 = Sys.time () in 
      let res =
        if v1=v2 then Irmin.Merge.ok v1
        else begin 
          let open Irmin.Merge.Infix in
          old () >>=* fun old ->
          let old = from_just old "merge" in
          let (adds,removes) = compute_diff old v2 in
          to_adt v1 >>= fun v1 ->
          of_adt @@ apply_diff adds removes v1 >>= fun merged_v ->
          (*to_adt (from_just old "merge") >>= fun oldv  ->
          to_adt v1 >>= fun v1  ->
          to_adt v2 >>= fun v2 ->
          let v = OM.merge oldv v1 v2 in
          of_adt v >>= fun merged_v ->*)
          Irmin.Merge.ok merged_v
        end in
      let t2 = Sys.time () in
      let _ = merge_time := !merge_time +. (t2-.t1) in
      let _ = merge_count := !merge_count + 1 in
      res

    let merge = Irmin.Merge.(option (v t merge))
  end

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
      Lwt_unix.sleep @@ 0.1 *. (float @@ Random.int 20) >>= fun _ ->
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
