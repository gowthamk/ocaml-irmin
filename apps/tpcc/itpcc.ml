open Irmin_unix
open Printf
open Msigs
open Tpcc
open Lwt.Infix

let from_just op msg = match op with
  | Some x -> x
  | None -> failwith @@ msg^": Expected Some. Got None."

module MakeVersioned (Config: CONFIG)  = struct

  module OM = Tpcc
  module K = Irmin.Hash.SHA1

  module VersionedTpcc = Versioned_types.Make(Config)

  module Serialization = Versioned_types.Serialization

  open VersionedTpcc

  type adt = OM.db

  module IWarehouseTable = Irbmap.MakeVersioned(Config)
                                (Id)(Warehouse)
                                (WarehouseTable)(IWarehouse)
  module IDistrictTable = Irbmap.MakeVersioned(Config) 
                                (IdPair)(District)
                                (DistrictTable)(IDistrict)
  module IOrderTable= Irbmap.MakeVersioned(Config)
                                (IdTriple)(Order)
                                (OrderTable)(IOrder)
  module INewOrderTable = Irbmap.MakeVersioned(Config) 
                                (IdTriple)(NewOrder)
                                (NewOrderTable)(INewOrder)
  module IOrderLineTable = Irbmap.MakeVersioned(Config) 
                                (IdQuad)(OrderLine)
                                (OrderLineTable)(IOrderLine)
  module IItemTable = Irbmap.MakeVersioned(Config) 
                                (Id)(Item)
                                (ItemTable)(IItem)
  module IHistTable = Irbmap.MakeVersioned(Config)
                                (IdQuin)(Hist)
                                (HistTable)(IHist)
  module IStockTable = Irbmap.MakeVersioned(Config)
                                (IdPair)(Stock)
                                (StockTable)(IStock)
  module ICustomerTable = Irbmap.MakeVersioned(Config)
                                (IdTriple)(Customer)
                                (CustomerTable)(ICustomer)

  type madt = {warehouse_table: IWarehouseTable.t; 
               district_table: IDistrictTable.t;
               order_table: IOrderTable.t;
               neworder_table: INewOrderTable.t;
               orderline_table: IOrderLineTable.t;
               item_table: IItemTable.t;
               hist_table: IHistTable.t;
               stock_table: IStockTable.t;
               customer_table: ICustomerTable.t}

  type t = 
    | DB of madt
    | IWarehouseTable of IWarehouseTable.t
    | IDistrictTable of IDistrictTable.t 
    | IOrderTable of IOrderTable.t
    | INewOrderTable of INewOrderTable.t
    | IOrderLineTable of IOrderLineTable.t
    | IItemTable of IItemTable.t
    | IHistTable of IHistTable.t
    | IStockTable of IStockTable.t
    | ICustomerTable of ICustomerTable.t

  type boxed_t = t

  let (madt : madt Irmin.Type.t) = 
    let open Irmin.Type in 
    record "madt" 
      (fun warehouse_table district_table 
           order_table neworder_table
           orderline_table item_table 
           hist_table stock_table customer_table -> 
           {warehouse_table;
            district_table;
            order_table;
            neworder_table;
            orderline_table;
            item_table;
            hist_table;
            stock_table;
           customer_table})
    |+ field "warehouse_table" IWarehouseTable.t 
              (fun t -> t.warehouse_table)
    |+ field "district_table" IDistrictTable.t
              (fun t -> t.district_table)
    |+ field "order_table" IOrderTable.t
              (fun t -> t.order_table)
    |+ field "neworder_table" INewOrderTable.t
              (fun t -> t.neworder_table)
    |+ field "orderline_table" IOrderLineTable.t
              (fun t -> t.orderline_table)
    |+ field "item_table" IItemTable.t
              (fun t -> t.item_table)
    |+ field "hist_table" IHistTable.t
              (fun t -> t.hist_table)
    |+ field "stock_table" IStockTable.t
              (fun t -> t.stock_table)
    |+ field "customer_table" ICustomerTable.t
              (fun t -> t.customer_table)
    |> sealr

  let (t: t Irmin.Type.t) = 
    let open Irmin.Type in
    variant "t" (fun db w d o no ol i h s c -> function
        | DB a  -> db a
        | IWarehouseTable a -> w a
        | IDistrictTable a -> d a
        | IOrderTable a -> o a
        | INewOrderTable a -> no a
        | IOrderLineTable a -> ol a
        | IItemTable a -> i a
        | IHistTable a -> h a
        | IStockTable a -> s a
        | ICustomerTable a -> c a)
    |~ case1 "DB" madt (fun x -> DB x)
    |~ case1 "IWarehouseTable" IWarehouseTable.t (fun x -> IWarehouseTable x)
    |~ case1 "IDistrictTable" IDistrictTable.t (fun x -> IDistrictTable x)
    |~ case1 "IOrderTable" IOrderTable.t (fun x -> IOrderTable x)
    |~ case1 "INewOrderTable" INewOrderTable.t (fun x -> INewOrderTable x)
    |~ case1 "IOrderLineTable" IOrderLineTable.t (fun x -> IOrderLineTable x)
    |~ case1 "IItemTable" IItemTable.t (fun x -> IItemTable x)
    |~ case1 "IHistTable" IHistTable.t (fun x -> IHistTable x)
    |~ case1 "IStockTable" IStockTable.t (fun x -> IStockTable x)
    |~ case1 "ICustomerTable" ICustomerTable.t (fun x -> ICustomerTable x)
    |> sealv

  module AO_value : Irmin.Contents.Conv with type t = t = struct 
    type t = boxed_t

    let t = t

    include Serialization(struct 
                            type t = boxed_t
                            let t = t
                          end)
  end

  module type MY_TREE = TAG_TREE with type value=t
  module type IWAREHOUSE_TREE = TAG_TREE with type value=IWarehouseTable.t
  module type IDISTRICT_TREE = TAG_TREE with type value=IDistrictTable.t
  module type IORDER_TREE = TAG_TREE with type value=IOrderTable.t
  module type INEWORDER_TREE = TAG_TREE with type value=INewOrderTable.t
  module type IORDERLINE_TREE = TAG_TREE with type value=IOrderLineTable.t
  module type IITEM_TREE = TAG_TREE with type value=IItemTable.t
  module type IHIST_TREE = TAG_TREE with type value=IHistTable.t
  module type ISTOCK_TREE = TAG_TREE with type value=IStockTable.t
  module type ICUSTOMER_TREE = TAG_TREE with type value=ICustomerTable.t

  let transform_tree : type a b c. (module MY_TREE with type t=a 
                                                    and type tag=b)
                -> (module IRMIN_DATA_STRUCTURE with type t = c) 
                -> (c -> t)
                -> (module TAG_TREE with type t=a 
                                     and type tag=b 
                                     and type value=c) =
    fun (module T) (module V) f ->
      let module Vtree = struct
          type t = T.t
          type tag = T.tag
          type value = V.t
          let tag_of_string = T.tag_of_string
          let tag_of_hash = T.tag_of_hash
          let empty = T.empty
          let set_prefix = T.set_prefix
          let add t tag vt = 
            T.add t tag (f vt)
        end in
      (module Vtree: TAG_TREE with type t=T.t 
                               and type tag=T.tag 
                               and type value=V.t)

  module AO_store  = struct
    module S = Irmin_git.AO(Git_unix.FS)(AO_value)
    include S

    type adt=OM.db

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
      let k = K.digest AO_value.t v in
      S.mem t k >>= fun b ->
      if b then Lwt.return (k,tree)
      else begin
        (S.add t v) >>= fun k ->
        let tag = T.tag_of_hash k in
        T.add tree tag v >>= fun tree' ->
        Lwt.return (k,tree')
      end

  end

  module rec BC_value: IRMIN_STORE_VALUE with type t = t 
                                          and type adt=adt = struct
    include AO_value

    type adt=OM.db

    let of_vadt: type a b c. 
              (module IRMIN_DATA_STRUCTURE with type adt=a 
                                            and type t = b) 
            -> (module TAG_TREE with type value = b 
                                 and type t = c)
            -> a -> c -> (b*c) Lwt.t =
      fun (module V) (module T) vadt tr ->
        V.of_adt (module T) vadt tr

    let rec of_adt : type a. (module MY_TREE with type t=a) ->
                        (OM.db) -> (a -> (boxed_t*a) Lwt.t) =
      fun  (module T) (adt:adt) ->
        (*
         * We momentarily override Lwt's bind and return so as to pass
         * the tree around without making a mess.
         *)
        let (>>=) m f = fun tr -> 
          m tr >>= fun (k,tr') -> f k tr' in
        let return x = fun tr -> Lwt.return (x,tr) in
        begin
          let iwh_tree = transform_tree (module T) 
                           (module IWarehouseTable)
                           (fun iwt -> IWarehouseTable iwt) in
          let module IWarehouseTree = 
              (val iwh_tree : IWAREHOUSE_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IWarehouseTable) (module IWarehouseTree)
                  adt.warehouse_table >>= fun iwarehouse_table ->
          let _ = printf "Warehouse table added\n" in
          let _ = flush_all() in
          let idistrict_tree = transform_tree (module T) 
                           (module IDistrictTable)
                           (fun iwt -> IDistrictTable iwt) in
          let module IDistrictTree = 
              (val idistrict_tree : IDISTRICT_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IDistrictTable) (module IDistrictTree)
                  adt.district_table >>= fun idistrict_table ->
          let _ = printf "District table added\n" in
          let _ = flush_all() in
          let iorder_tree = transform_tree (module T) 
                           (module IOrderTable)
                           (fun iwt -> IOrderTable iwt) in
          let module IOrderTree = 
              (val iorder_tree : IORDER_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IOrderTable) (module IOrderTree)
                  adt.order_table >>= fun iorder_table ->
          let _ = printf "Order table added\n" in
          let _ = flush_all() in
          let ineworder_tree = transform_tree (module T) 
                           (module INewOrderTable)
                           (fun iwt -> INewOrderTable iwt) in
          let module INewOrderTree = 
              (val ineworder_tree : INEWORDER_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module INewOrderTable) (module INewOrderTree)
                  adt.neworder_table >>= fun ineworder_table ->
          let _ = printf "NewOrder table added\n" in
          let _ = flush_all() in
          let iorderline_tree = transform_tree (module T) 
                           (module IOrderLineTable)
                           (fun iwt -> IOrderLineTable iwt) in
          let module IOrderLineTree = 
              (val iorderline_tree : IORDERLINE_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IOrderLineTable) (module IOrderLineTree)
                  adt.orderline_table >>= fun iorderline_table ->
          let _ = printf "OrderLine table added\n" in
          let _ = flush_all() in
          let iitem_tree = transform_tree (module T) 
                           (module IItemTable)
                           (fun iwt -> IItemTable iwt) in
          let module IItemTree = 
              (val iitem_tree : IITEM_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IItemTable) (module IItemTree)
                  adt.item_table >>= fun iitem_table ->
          let _ = printf "Item table added\n" in
          let _ = flush_all() in
          let ihist_tree = transform_tree (module T) 
                           (module IHistTable)
                           (fun iwt -> IHistTable iwt) in
          let module IHistTree = 
              (val ihist_tree : IHIST_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IHistTable) (module IHistTree)
                  adt.hist_table >>= fun ihist_table ->
          let _ = printf "Hist table added\n" in
          let _ = flush_all() in
          let istock_tree = transform_tree (module T) 
                           (module IStockTable)
                           (fun iwt -> IStockTable iwt) in
          let module IStockTree = 
              (val istock_tree : ISTOCK_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IStockTable) (module IStockTree)
                  adt.stock_table >>= fun istock_table ->
          let _ = printf "Stock table added\n" in
          let _ = flush_all() in
          let icustomer_tree = transform_tree (module T) 
                           (module ICustomerTable)
                           (fun iwt -> ICustomerTable iwt) in
          let module ICustomerTree = 
              (val icustomer_tree : ICUSTOMER_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ICustomerTable) (module ICustomerTree)
                  adt.customer_table >>= fun icustomer_table ->
          let _ = printf "Customer table added\n" in
          let _ = flush_all() in
          return @@ DB {warehouse_table=iwarehouse_table;
                        district_table=idistrict_table;
                        order_table=iorder_table;
                        neworder_table=ineworder_table;
                        orderline_table=iorderline_table;
                        item_table=iitem_table;
                        hist_table=ihist_table;
                        stock_table=istock_table;
                        customer_table=icustomer_table;}
        end

    let madt_to_adt (t:madt) : OM.db Lwt.t =
      IWarehouseTable.to_adt t.warehouse_table >>= fun warehouse_table ->
      IDistrictTable.to_adt t.district_table >>= fun district_table -> 
      IOrderTable.to_adt t.order_table >>= fun order_table ->
      INewOrderTable.to_adt t.neworder_table >>= fun neworder_table ->
      IOrderLineTable.to_adt t.orderline_table >>= fun orderline_table ->
      IItemTable.to_adt t.item_table >>= fun item_table ->
      IHistTable.to_adt t.hist_table >>= fun hist_table ->
      IStockTable.to_adt t.stock_table >>= fun stock_table ->
      ICustomerTable.to_adt t.customer_table >>= fun customer_table ->
      let open Tpcc in
      Lwt.return @@ {warehouse_table=warehouse_table;
                     district_table=district_table;
                     order_table=order_table;
                     neworder_table=neworder_table;
                     orderline_table=orderline_table;
                     item_table=item_table;
                     hist_table=hist_table;
                     stock_table=stock_table;
                     customer_table=customer_table;}

    let to_adt (t:t) : OM.db Lwt.t =match t with
      | DB madt -> madt_to_adt madt
      | _ -> failwith "Exhaustiveness in Itpcc.to_adt"


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

    let string_of_path p = String.concat "/" p

    let info s = Irmin_unix.info "[repo %s] %s" Config.root s

    let rec update ?msg t (p:path) (v:boxed_t) = 
      let msg = match msg with
        | Some s -> s
        | None -> "Setting "^(string_of_path p) in
      Store.set t p v ~info:(info msg)

    let init ?root ?bare () =
      let config = Irmin_git.config Config.root in
      Store.Repo.v config

    let master (repo:Store.repo) = Store.master repo

    module Tree = 
      struct
        type t = Store.tree

        type tag = string list

        type value = boxed_t

        let empty () = Store.Tree.empty

        let prefix = ref []

        let set_prefix p = prefix := p

        let tag_of_string str = (!prefix)@[str]

        let tag_of_hash k = 
          let sha_str = Fmt.to_to_string Irmin.Hash.SHA1.pp k in
          let fname_k = String.sub sha_str 0 10 in
            tag_of_string fname_k

        let add t k v = 
          if k = ["head"] 
          then Store.Tree.add t k v
          else Lwt.return t
          (*Store.Tree.mem t k >>= fun b ->
          if b then Lwt.return t
          else Store.Tree.add t k v
          Lwt.return t*)
      end

    let clone t name = Store.clone t name

    let get_branch r ~branch_name = Store.of_branch r branch_name

    let merge s ~into = Store.merge s ~into

    (*let update t k v = Store.set t k v*)

    let read t (p:path) = 
      Store.find t p 

    let with_tree t path ~info f = Store.with_tree t path f
                                    ~info:info
                                    ~strategy:`Set

    let status t = Store.status t
  end

  module type VPST = sig
    type 'a t
    val return : 'a -> 'a t
    val bind: 'a t -> ('a -> 'b t) -> 'b t
    val with_init_version_do: OM.db -> 'a t -> 'a 
    val with_remote_version_do: string -> 'a t -> 'a
    val with_persistent_version_do: 'a t -> 'a
    val get_latest_version: unit -> OM.db t
    val sync_next_version: ?v:OM.db -> string list 
                                  -> string -> OM.db t
    val liftLwt: 'a Lwt.t -> 'a t
    val pull_remote: string -> unit t
    val get_size_on_disk: unit -> int t
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
      OM.db t)

    let with_persistent_version_do (m : 'a t) =
      Lwt_main.run
        begin 
          BC_store.init () >>= fun repo ->
          BC_store.master repo >>= fun m_br ->
          (* TODO: name has to be random to avoid collisions *)
          let st = { master = m_br; name = "1"; 
                     next_id = 1; seq_no = 1 } in
          m st >>= (fun (a,_) -> Lwt.return a)
        end

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

    let sync_next_version ?v uris txn_name = fun (st:st) ->
      (* 1. Commit to the master branch *)
      (match v with 
       | None -> Lwt.return ()
       | Some v -> 
         begin
           let module Tree = BC_store.Tree in
           let tr = Tree.empty () in
           let _ = Tree.set_prefix ["state"; txn_name] in
           let tmod = (module Tree : MY_TREE 
                        with type t = BC_store.tree) in
           BC_value.of_adt tmod v tr >>= fun (v',tr') ->
           let _ = printf "of_adt done\n" in
           let _ = flush_all () in
           BC_store.update st.master ["state"; "head"] v'
           (*BC_store.with_tree st.master ["state"; txn_name]
             ~info:(info @@
                    sprintf "%d. %s committing" st.seq_no txn_name)
             begin fun trop ->
               let module Tree = BC_store.Tree in
               let tr = match trop with
                 | Some tr -> tr
                 | None -> Tree.empty () in
                let tmod = (module Tree : MY_TREE 
                             with type t = BC_store.tree) in
                BC_value.of_adt tmod v tr >>= fun (v',tr') ->
                let _ = printf "of_adt done\n" in
                let _ = flush_all () in
                let _ = head := Some v' in
                let _ = printf "Returning tree\n" in
                let _ = flush_all () in
                Lwt.return @@ Some tr'
             end >>= fun _ ->
           BC_store.update st.master ["state"; "head"] @@
              from_just !head "Head version" *)
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
        
    let liftLwt (m: 'a Lwt.t) : 'a t = fun st ->
      m >>= fun a -> Lwt.return (a,st)

    let get_size_on_disk () = fun st ->
        Lwt.return (7000000, st)
	end
 
end
