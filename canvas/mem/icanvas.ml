open Printf
open Lwt.Infix
open Irmin_unix
module OM = Canvas.Make
module Canvas = OM
open OM
module K = Irmin.Hash.SHA1
module G = Git_unix.Mem
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

  module AO_store = 
  struct
    module S = Irmin_git.AO(G)(AO_value)
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

    let on_add = ref (fun k v -> (*printf "%s\n" 
                                   (Fmt.to_to_string K.pp k); *)
                                 Lwt.return ())
    let add t v =
      (S.add t v) >>=
        (fun k -> ((!on_add) k v) >>= (fun _ -> Lwt.return k))

    module PHashtbl = Hashtbl.Make(struct 
        type t = OM.t
        let equal x y = x == y
        let hash x = Hashtbl.hash_param 2 10 x
      end)

    let (read_cache: (K.t, OM.t) Hashtbl.t) = Hashtbl.create 5051

    let (write_cache: K.t PHashtbl.t) = PHashtbl.create 5051

    let rec add_adt t (a:OM.t) : K.t Lwt.t =
      try 
        Lwt.return @@ PHashtbl.find write_cache a
      with Not_found -> begin 
        add t =<<
          (match a with
           | OM.N {r;g;b} -> Lwt.return @@ N {r;g;b}
           | OM.B {tl_t;tr_t;bl_t;br_t} -> 
             (add_adt t tl_t >>= fun tl_t' ->
              add_adt t tr_t >>= fun tr_t' ->
              add_adt t bl_t >>= fun bl_t' ->
              add_adt t br_t >>= fun br_t' ->
              Lwt.return @@ B {tl_t=tl_t'; tr_t=tr_t'; 
                               bl_t=bl_t'; br_t=br_t'}))
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

    let read t k =
      find t k >>= fun vop ->
      Lwt.return @@ from_just vop "AO_store.read"
  end

  module type IRMIN_STORE_VALUE  =
    sig
      include Irmin.Contents.S
      val of_adt : Canvas.t -> t Lwt.t
      val to_adt : t -> Canvas.t Lwt.t
    end

  let merge_time = ref 0.0
  let merge_count = ref 0

  module BC_value =
    (struct
       include AO_value

       let of_adt (a:OM.t) : t Lwt.t  =
         AO_store.create () >>= fun ao_store -> 
         let aostore_add adt =
           AO_store.add_adt ao_store adt in
         match a with
          | OM.N {r;g;b} -> Lwt.return @@ N {r;g;b}
          | OM.B {tl_t;tr_t;bl_t;br_t} -> 
            (aostore_add tl_t >>= fun tl_t' ->
             aostore_add tr_t >>= fun tr_t' ->
             aostore_add bl_t >>= fun bl_t' ->
             aostore_add br_t >>= fun br_t' ->
             Lwt.return @@ B {tl_t=tl_t'; tr_t=tr_t'; 
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

       let rec merge_keys k k1 k2 = 
         if k = k1 then Lwt.return k2
         else if k = k2 then Lwt.return k1
         else begin 
           AO_store.create () >>= fun ao_store ->
           AO_store.read ao_store k >>= fun old ->
           AO_store.read ao_store k1 >>= fun v1 ->
           AO_store.read ao_store k2 >>= fun v2 ->
           do_merge old v1 v2 >>= fun v ->
           AO_store.add ao_store v
         end
       and do_merge old v1 v2 = 
         if old = v1 then Lwt.return v2
         else if old = v2 then Lwt.return v1
         else match (old,v1,v2) with
           | (_, B _, N _) 
           | (_, N _, B _) 
           | (_, N _, N _)
           | (N _, B _, B _) ->
             begin 
               to_adt old >>= fun old ->
               to_adt v1 >>= fun v1 ->
               to_adt v2 >>= fun v2 ->
               of_adt @@ OM.merge old v1 v2
             end
           | (B x, B x1, B x2) ->
             begin 
               merge_keys x.tl_t x1.tl_t x2.tl_t >>= fun tl_t' ->
               merge_keys x.tr_t x1.tr_t x2.tr_t >>= fun tr_t' ->
               merge_keys x.bl_t x1.bl_t x2.bl_t >>= fun bl_t' ->
               merge_keys x.br_t x1.br_t x2.br_t >>= fun br_t' ->
               Lwt.return @@ B {tl_t=tl_t'; tr_t=tr_t'; 
                                bl_t=bl_t'; br_t=br_t'}
             end

       let rec merge ~old:(old : t Irmin.Merge.promise)  (v1 : t)
         (v2 : t) = 
         let _ = Gc.full_major () in
         let t1 = Sys.time () in 
         let res = 
           if v1 = v2 then Irmin.Merge.ok v1
           else begin
             let open Irmin.Merge.Infix in
             old() >>=* fun old ->
             do_merge (from_just old "merge.old") 
                            v1 v2 >>= fun merged_v ->
             Irmin.Merge.ok merged_v
           end in
         let t2 = Sys.time () in
         let _ = merge_time := !merge_time +. (t2-.t1) in
         let _ = merge_count := !merge_count + 1 in
         res

       let merge = let open Irmin.Merge in option (v t merge)
     end : (IRMIN_STORE_VALUE with type  t =  madt))

  module BC_store =
    struct
      module Store = (Irmin_unix.Git.Mem.KV)(BC_value)
      module Sync = (Irmin.Sync)(Store)
      module Head = Store.Head
      type t = Store.t
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

      let rec update ?msg  t (p : path) (v : BC_value.t) =
        let msg =
          match msg with
          | Some s -> s
          | None -> "Setting " ^ (string_of_path p) in
        Store.set t p v ~info:(info msg)

      let pp = Fmt.using Store.status Store.Status.pp
    end
  module Vpst :
    sig
      type 'a t
      type branch
      val return : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
      val with_init_version_do : Canvas.t -> 'a t -> 'a
      val fork_version : ?parent:branch -> 'a t -> branch t
      val set_parent: branch -> unit t
      val get_latest_version : unit -> Canvas.t t
      val sync_next_version : ?v:Canvas.t -> Canvas.t t
      val liftLwt : 'a Lwt.t -> 'a t
      val print_info: unit t
    end =
    struct
      type branch = BC_store.t
      type st =
        {
        master: branch;
        parent: branch;
        local: branch;
        name: string ;
        next_id: int }
      type 'a t = st -> ('a * st) Lwt.t
      let info s = Irmin_unix.info "[repo %s] %s" Config.root s
      let path = ["state"]
      let return (x : 'a) = (fun st -> Lwt.return (x, st) : 'a t)
      let bind (m1 : 'a t) (f : 'a -> 'b t) =
        (fun st -> (m1 st) >>= (fun (a, st') -> f a st') : 'b t)

      let with_init_version_do (v : Canvas.t) (m : 'a t) =
        Lwt_main.run
          ((BC_store.init ()) >>=
             (fun repo ->
                (BC_store.master repo) >>=
                  (fun m_br ->
                     (BC_value.of_adt v) >>=
                       (fun (v' : BC_value.t) ->
                          (BC_store.update ~msg:"initial version" m_br
                             path v')
                            >>=
                            (fun () ->
                               (BC_store.clone m_br "1_local") >>=
                                 (fun t_br ->
                                    let st =
                                      {
                                        master = m_br;
                                        parent = m_br;
                                        local = t_br;
                                        name = "1";
                                        next_id = 1
                                      } in
                                    (m st) >>=
                                      (fun (a, _) -> Lwt.return a)))))))
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

      let get_latest_version () =
        (fun (st : st) ->
           (BC_store.read st.local path) >>=
             (fun (vop : BC_value.t option) ->
                let v = from_just vop "get_latest_version" in
                (BC_value.to_adt v) >>= (fun td -> Lwt.return (td, st))) : 
        Canvas.t t)


      let set_parent parent = fun (st:st) ->
        Lwt.return ((), {st with parent=parent})

      let sync_next_version ?v = fun (st:st) ->
        try
          (* 1. Commit to the  local branch *)
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
        with _ -> failwith "Some error occured"
        
      let liftLwt (m : 'a Lwt.t) =
        (fun st -> m >>= (fun a -> Lwt.return (a, st)) : 'a t)

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

