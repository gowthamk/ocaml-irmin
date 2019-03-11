open Lwt.Infix
open Irmin_unix

module G = Git_unix.FS
module K = Irmin.Hash.SHA1
module M = Irmin.Merge

type path = string list

let from_just = function (Some x) -> x
  | None -> failwith "Expected Some. Got None."

module type MERGEABLE = sig
  include Irmin.Contents.S with module Path = Irmin.Path.String_list
  val to_string: t -> string
end

module type ICANVAS = sig
  include MERGEABLE
  val of_canvas: Canvas.t -> t Lwt.t
  val to_canvas: t -> Canvas.t Lwt.t
end

module ICanvas: ICANVAS = struct
  type icanvas = 
    | N of Canvas.pixel
    | B of {tl_t:K.t; tr_t:K.t;
            bl_t:K.t; br_t:K.t}

  module AO_value (* : Tc.S0 *) = struct
    type t = icanvas

    let equal t1 t2 = t1 = t2 (* structural equality *)
    let compare = compare
    let hash = Hashtbl.hash

    let to_json = let f = Char.code in function
      | N px -> `A [`String "N"; `O [("r", Tc.Int.to_json @@ f px.r);
                                     ("g", Tc.Int.to_json @@ f px.g);
                                     ("b", Tc.Int.to_json @@ f px.b)]] 
      | B x -> `A [`String "B"; `O [("tl_t", K.to_json x.tl_t);
                                    ("tr_t", K.to_json x.tr_t);
                                    ("bl_t", K.to_json x.bl_t);
                                    ("br_t", K.to_json x.br_t)]]
    let of_json = function
      | `A [`String "N"; `O [("r",r); ("g",g); ("b",b)]] -> 
          let f = Char.chr in 
            N {r=f @@ Tc.Int.of_json r; 
               g=f @@ Tc.Int.of_json g;
               b=f @@ Tc.Int.of_json b}
      | `A [`String "B"; `O [("tl_t",tl_t); ("tr_t",tr_t); 
                             ("bl_t",bl_t); ("br_t",br_t)]] -> 
          let f = K.of_json in
            B {tl_t= f tl_t; tr_t=f tr_t; bl_t=f bl_t; br_t=f br_t}
      | j -> Ezjsonm.parse_error j "ICanvas.AO_value.of_json"
    
    let to_string t = Ezjsonm.to_string (to_json t)
    let of_string s = of_json (Ezjsonm.from_string s)
    let write t buf =
      let str = to_string t in
      let len = String.length str in
      Cstruct.blit_from_string str 0 buf 0 len;
      Cstruct.shift buf len
    let read buf =
      Mstruct.get_string buf (Mstruct.length buf)
      |> of_string
    let size_of t =
      let str = to_string t in
      String.length str
  end

  module AO_store = struct
    module S = Irmin_git.AO(G)(K)(AO_value)
    include S

    let create config =
      let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
        "level" Irmin.Private.Conf.(some int) None
      in
      let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
      let level = Irmin.Private.Conf.get config level in
      G.create ?root ?level ()

    (* Somehow pulls the config set by Store.init *)
    (* And creates a Git backend *)
    let create () = create @@ Irmin_git.config ()
  end

  (* canvas functions *)
  let blank = N Canvas.default_pixel

  let empty = 
    AO_store.create () >>= fun ao_store ->
    AO_store.add ao_store blank

  let plain px = 
    AO_store.create () >>= fun ao_store ->
    AO_store.add ao_store @@ N px

  let cons tl tr bl br  = 
    let new_c = B {tl_t=tl; tr_t=tr; bl_t=bl; br_t=br} in
      AO_store.create () >>= fun ao_store ->
      AO_store.add ao_store new_c

  let b_of_n px = plain px >>= fun k -> cons k k k k 

  let rec of_canvas = function 
    | Canvas.N px -> plain px
    | Canvas.B x ->
        of_canvas x.tl_t >>= fun tl_key -> 
        of_canvas x.tr_t >>= fun tr_key -> 
        of_canvas x.bl_t >>= fun bl_key -> 
        of_canvas x.br_t >>= fun br_key -> 
        cons tl_key tr_key bl_key br_key

  let rec to_canvas t_key = 
    AO_store.create () >>= fun ao_store ->
    AO_store.read ao_store t_key >>= fun t ->
    match from_just t with
      | N px -> Lwt.return @@ Canvas.plain px 
      | B x -> 
          to_canvas x.tl_t >>= fun tl -> 
          to_canvas x.tr_t >>= fun tr ->
          to_canvas x.bl_t >>= fun bl -> 
          to_canvas x.br_t >>= fun br ->
          Lwt.return @@ Canvas.make_b (tl,tr,bl,br)

  let rec merge old_k v1_k v2_k : K.t Lwt.t = 
    if v1_k=v2_k then Lwt.return v1_k
    else if v1_k=old_k then Lwt.return v2_k
    else if v2_k=old_k then Lwt.return v1_k
    else 
      AO_store.create () >>= fun ao_store ->
      AO_store.read ao_store old_k >>= fun old ->
      AO_store.read ao_store v1_k >>= fun v1 ->
      AO_store.read ao_store v2_k >>= fun v2 ->
      match (from_just old, from_just v1, from_just v2) with
        | (_, B _, N px2) -> b_of_n px2 >>= fun v2'_k -> 
            merge old_k v1_k v2'_k
        | (_, N px1, B _) -> b_of_n px1 >>= fun v1'_k -> 
            merge old_k v1'_k v2_k
        | (N px, B _, B _) -> b_of_n px >>= fun old'_k -> 
            merge old'_k v1_k v2_k
        | (B x, B x1, B x2) ->
            merge x.tl_t x1.tl_t x2.tl_t >>= fun tl_t' ->
            merge x.tr_t x1.tr_t x2.tr_t >>= fun tr_t' ->
            merge x.bl_t x1.bl_t x2.bl_t >>= fun bl_t' ->
            merge x.br_t x1.br_t x2.br_t >>= fun br_t' ->
              cons tl_t' tr_t' bl_t' br_t'
        | (_, N px1, N px2) -> 
            (* pixels are merged by mixing colors *)
            let px' = Canvas.color_mix px1 px2 in 
              plain px'
        
  type t = K.t

  let equal t1 t2 = true
  let compare = compare
  let hash = Hashtbl.hash

  let to_json k = `A [`String (K.to_hum k)]
  let of_json = function
    | `A [`String kstr] -> K.of_hum kstr
    | j -> Ezjsonm.parse_error j "MList_contents.C.of_json"
  
  let to_string t = Ezjsonm.to_string (to_json t)
  let of_string s = of_json (Ezjsonm.from_string s)
  let write t buf =
    let str = to_string t in
    let len = String.length str in
    Cstruct.blit_from_string str 0 buf 0 len;
    Cstruct.shift buf len
  let read buf =
    Mstruct.get_string buf (Mstruct.length buf)
    |> of_string
  let size_of t =
    let str = to_string t in
    String.length str

  module Path = Irmin.Path.String_list

  let merge : Path.t -> t option Irmin.Merge.t = 
    let merge' path ~old (v1: t option) (v2: t option)
          : t option Irmin.Merge.result Lwt.t =
        let old = match Lwt_main.run @@ old () with
          | `Ok (Some x) -> x 
          | _ -> failwith "Impossible" (* ToDo: Dali merges 
                 always have a common ancestor. Change types
                 to reflect this guarantee *) in
          try 
            merge (from_just old) (from_just v1) 
              (from_just v2) >>= fun k ->
            Lwt.return @@ `Ok (Some k)
          with e -> 
            Lwt.return @@ `Conflict "Canvas: merge failed!"
    in
      merge'
end

module BC_store = struct
  module Path = Irmin.Path.String_list
  module Store = Irmin_git.FS(ICanvas)(Irmin.Ref.String)(Irmin.Hash.SHA1)
  type repo = Store.Repo.t
  type branch = string -> Store.t

  type path = string list

  let init ?root ?bare () =
    let config = Irmin_git.config ?root ?bare () in
    Store.Repo.create config

  let master (repo:repo) : branch Lwt.t = Store.master task repo
  let clone_force t name = Store.clone_force task (t "cloning") name
  let get_branch r ~branch_name = Store.of_branch_id task branch_name r
  let merge b ~into = Store.merge_exn "" b ~into
  let get_branch_name b = Store.name (b "name")
  let update = Store.update
  let read = Store.read
end

module Vpst : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val with_init_version_do: Canvas.t -> 'a t -> 'a
  val fork_version : 'a t -> unit t
  val get_latest_version: unit -> Canvas.t t
  val sync_next_version: ?v:Canvas.t -> Canvas.t t
  val liftLwt : 'a Lwt.t -> 'a t
end= struct
  type branch = BC_store.branch
  type st = {master    : branch;
             local     : branch;
             name      : string;
             next_id   : int}
  type 'a t = st -> ('a * st) Lwt.t

  (* The path at which the db is stored on all branches. *)
  let path = ["state"]

  let return (x : 'a) : 'a t = 
    fun st -> Lwt.return (x,st)

  let bind (m1: 'a t) (f: 'a -> 'b t) : 'b t = 
    fun st -> (m1 st >>= fun (a,st') -> f a st')

  let with_init_version_do (v: Canvas.t) (m: 'a t) =
    Lwt_main.run 
      begin
        BC_store.init () >>= fun repo -> 
        BC_store.master repo >>= fun m_br -> 
        let m_store = m_br "creating state on master" in
        ICanvas.of_canvas v >>= fun k ->
        BC_store.update m_store path k >>= fun () ->
        BC_store.clone_force m_br "1_local" >>= fun t_br ->
        let st = {master=m_br; local=t_br; name="1"; next_id=1} in
        m st >>= fun (a,_) -> Lwt.return a
      end
    
  let fork_version (m: 'a t) :unit t = fun (st: st) ->
    let thread_f () = 
      let child_name = st.name^"_"^(string_of_int st.next_id) in
      let parent_m_br = st.master in
      (* Ideally, the following has to happen: *)
      (* BC_store.clone_force parent_m_br m_name >>= fun m_br -> *)
      (* But, we currently default to an SC mode. Master is global. *)
      let m_br = parent_m_br in
      (* fork_version forking the master, when executed in the context
       * of a branch is counter-intuitive. FixMe. *)
      BC_store.clone_force m_br (child_name^"_local") >>= fun t_br ->
      let new_st = {master = m_br; local  = t_br; 
                    name = child_name; next_id = 1} in
        m new_st in
    begin
      Lwt.async thread_f;
      Lwt.return ((), {st with next_id=st.next_id+1})
    end

  let get_latest_version () : Canvas.t t = fun (st: st) ->
    let bc_store = st.local "reading local state" in
    BC_store.read bc_store path >>= fun k ->
    ICanvas.to_canvas @@ from_just k >>= fun td ->
    Lwt.return (td,st)

  let sync_next_version ?v : Canvas.t t = fun (st: st) ->
    (* How do you commit the next version? Simply update path? *)
    (* 1. Commit to the local branch *)
    let bc_store = st.local "committing local state" in
    (match v with | None -> Lwt.return ()
      | Some v -> 
          ICanvas.of_canvas v >>= fun k -> 
          BC_store.update bc_store path k) >>= fun () ->
    (* 2. Merge local master to the local branch *)
    BC_store.merge st.master ~into:st.local >>= fun () ->
    (* 3. Merge local branch to the local master *)
    BC_store.merge st.local ~into:st.master >>= fun () ->
    get_latest_version () st
   
  let liftLwt (m: 'a Lwt.t) : 'a t = fun st ->
    m >>= fun a -> Lwt.return (a,st)

end


