open Printf
open Msigs
open Lwt.Infix
module Id = Tpcc.Id

type id = Id.t

let id = Id.t

let counter_merge = Tpcc.counter_merge

let from_just op msg = match op with
  | Some x -> x
  | None -> failwith @@ msg^": Expected Some. Got None."

module Serialization(S:sig 
                        type t 
                        val t: t Irmin.Type.t 
                      end) = struct 
  open S

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

module MakeVersionedDS(Config:CONFIG)
                      (OM:MERGEABLE)
                      (AO_value:Irmin.Contents.Conv with type t = OM.t) =
struct
  module S = Irmin_git.AO(Git_unix.FS)(AO_value)

  include AO_value

  type adt = OM.t

  let create config =
    let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
        "level" Irmin.Private.Conf.(some int) None
    in
    let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
    let level = Irmin.Private.Conf.get config level in
    Git_unix.FS.create ?root ?level ()

  (* Creates a Git backend *)
  let create () = create @@ Irmin_git.config Config.root

  module type MY_TREE = TAG_TREE with type value=AO_value.t

  let add_and_link: type a. (module MY_TREE with type t=a) 
                    -> S.t -> AO_value.t -> a -> (K.t*a) Lwt.t =
    fun (module T) t v tree ->
      (S.add t v) >>= fun k ->
      let tag = T.tag_of_hash k in
      T.add tree tag v >>= fun tree' ->
          Lwt.return (k,tree')
     
  let of_adt : type a. (module MY_TREE with type t=a) ->
                    adt -> a -> (t*a) Lwt.t = 
    fun (module T) adt tr ->
      begin
        create () >>= fun ao_store ->
        add_and_link (module T) 
                ao_store adt tr >>= fun (_,tr') ->
        Lwt.return (adt,tr')
      end

  let to_adt (t:t) : adt Lwt.t =
    Lwt.return t

  let merge ~(old:t Irmin.Merge.promise) (v1:t) (v2:t) =
    if v1=v2 then Irmin.Merge.ok v1
    else begin 
      let open Irmin.Merge.Infix in
      old () >>=* fun old ->
      let old = from_just old "merge" in
      let v = OM.merge ~ancestor:old v1 v2 in
      Irmin.Merge.ok v
    end
      
  let merge = Irmin.Merge.(option (v t merge))

end

module Make(Config:CONFIG) = 
struct
  
  module Warehouse : IRMIN_DATA_STRUCTURE 
    with type adt = Tpcc.Warehouse.t = 
  struct

    module OM = Tpcc.Warehouse 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpcc.Warehouse in
        record "t" (fun w_id w_ytd -> {w_id; w_ytd})
        |+ field "w_id" id (fun t -> t.w_id)
        |+ field "w_ytd" int32 (fun t -> t.w_ytd)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module District: IRMIN_DATA_STRUCTURE 
    with type adt = Tpcc.District.t = 
  struct

    module OM = Tpcc.District 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
    let t =
        let open Irmin.Type in
        let open Tpcc.District in
        ((((record "madt"
              (fun d_id ->
                 fun d_w_id -> fun d_ytd -> { d_id; d_w_id; d_ytd }))
             |+ (field "d_id" id (fun t -> t.d_id)))
            |+ (field "d_w_id" id (fun t -> t.d_w_id)))
           |+ (field "d_ytd" int32 (fun t -> t.d_ytd)))
          |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module Order: IRMIN_DATA_STRUCTURE 
    with type adt = Tpcc.Order.t = 
  struct

    module OM = Tpcc.Order 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
    let t =
        let open Irmin.Type in
        let open Tpcc.Order in
        (((((((record "t"
                 (fun o_id ->
                    fun o_w_id ->
                      fun o_d_id ->
                        fun o_c_id ->
                          fun o_ol_cnt ->
                            fun o_carrier_id ->
                              {
                                o_id;
                                o_w_id;
                                o_d_id;
                                o_c_id;
                                o_ol_cnt;
                                o_carrier_id
                              }))
                |+ (field "o_id" id (fun t -> t.o_id)))
               |+ (field "o_w_id" id (fun t -> t.o_w_id)))
              |+ (field "o_d_id" id (fun t -> t.o_d_id)))
             |+ (field "o_c_id" id (fun t -> t.o_c_id)))
            |+ (field "o_ol_cnt" int32 (fun t -> t.o_ol_cnt)))
           |+ (field "o_carrier_id" bool (fun t -> t.o_carrier_id)))
          |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module NewOrder: IRMIN_DATA_STRUCTURE 
    with type adt = Tpcc.NewOrder.t = 
  struct

    module OM = Tpcc.NewOrder 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
    let t =
        let open Irmin.Type in
        let open Tpcc.NewOrder in
        ((((record "t"
              (fun no_o_id ->
                 fun no_d_id ->
                   fun no_w_id -> { no_o_id; no_d_id; no_w_id }))
             |+ (field "no_o_id" id (fun t -> t.no_o_id)))
            |+ (field "no_d_id" id (fun t -> t.no_d_id)))
           |+ (field "no_w_id" id (fun t -> t.no_w_id)))
          |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module OrderLine: IRMIN_DATA_STRUCTURE 
    with type adt = Tpcc.OrderLine.t = 
  struct

    module OM = Tpcc.OrderLine 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
    let t =
        let open Irmin.Type in
        let open Tpcc.OrderLine in
        ((((((((((record "t"
                    (fun ol_o_id ->
                       fun ol_d_id ->
                         fun ol_w_id ->
                           fun ol_num ->
                             fun ol_amt ->
                               fun ol_i_id ->
                                 fun ol_supply_w_id ->
                                   fun ol_qty ->
                                     fun ol_delivery_d ->
                                       {
                                         ol_o_id;
                                         ol_d_id;
                                         ol_w_id;
                                         ol_num;
                                         ol_amt;
                                         ol_i_id;
                                         ol_supply_w_id;
                                         ol_qty;
                                         ol_delivery_d
                                       }))
                   |+ (field "ol_o_id" id (fun t -> t.ol_o_id)))
                  |+ (field "ol_d_id" id (fun t -> t.ol_d_id)))
                 |+ (field "ol_w_id" id (fun t -> t.ol_w_id)))
                |+ (field "ol_num" int32 (fun t -> t.ol_num)))
               |+ (field "ol_amt" int32 (fun t -> t.ol_amt)))
              |+ (field "ol_i_id" id (fun t -> t.ol_i_id)))
             |+
             (field "ol_supply_w_id" id (fun t -> t.ol_supply_w_id)))
            |+ (field "ol_qty" int32 (fun t -> t.ol_qty)))
           |+ (field "ol_delivery_d" (option float) (fun t -> t.ol_delivery_d)))
          |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end
  module Item: IRMIN_DATA_STRUCTURE 
    with type adt = Tpcc.Item.t = 
  struct

    module OM = Tpcc.Item 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
    let t =
        let open Irmin.Type in
        let open Tpcc.Item in
        ((((record "t"
              (fun i_id ->
                 fun i_name ->
                   fun i_price -> { i_id; i_name; i_price }))
             |+ (field "i_id" id (fun t -> t.i_id)))
            |+ (field "i_name" string (fun t -> t.i_name)))
           |+ (field "i_price" int32 (fun t -> t.i_price)))
          |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end
  module Hist: IRMIN_DATA_STRUCTURE 
    with type adt = Tpcc.Hist.t = 
  struct

    module OM = Tpcc.Hist 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
    let t =
        let open Irmin.Type in
        let open Tpcc.Hist in
        (((((((record "t"
                 (fun h_c_id ->
                    fun h_c_d_id ->
                      fun h_c_w_id ->
                        fun h_d_id ->
                          fun h_w_id ->
                            fun h_amt ->
                              {
                                h_c_id;
                                h_c_d_id;
                                h_c_w_id;
                                h_d_id;
                                h_w_id;
                                h_amt
                              }))
                |+ (field "h_c_id" id (fun t -> t.h_c_id)))
               |+ (field "h_c_d_id" id (fun t -> t.h_c_d_id)))
              |+ (field "h_c_w_id" id (fun t -> t.h_c_w_id)))
             |+ (field "h_d_id" id (fun t -> t.h_d_id)))
            |+ (field "h_w_id" id (fun t -> t.h_w_id)))
           |+ (field "h_amt" int32 (fun t -> t.h_amt))) 
          |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module Stock: IRMIN_DATA_STRUCTURE 
    with type adt = Tpcc.Stock.t = 
  struct

    module OM = Tpcc.Stock 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
    let t =
        let open Irmin.Type in
        let open Tpcc.Stock in
        ((((((record "t"
                (fun s_i_id ->
                   fun s_w_id ->
                     fun s_qty ->
                       fun s_ytd ->
                         fun s_order_cnt ->
                           {
                             s_i_id;
                             s_w_id;
                             s_qty;
                             s_ytd;
                             s_order_cnt
                           }))
               |+ (field "s_i_id" id (fun t -> t.s_i_id)))
              |+ (field "s_w_id" id (fun t -> t.s_w_id)))
             |+ (field "s_qty" int32 (fun t -> t.s_qty)))
            |+ (field "s_ytd" int32 (fun t -> t.s_ytd)))
           |+ (field "s_order_cnt" int32 (fun t -> t.s_order_cnt))) 
          |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module Customer: IRMIN_DATA_STRUCTURE 
    with type adt = Tpcc.Customer.t = 
  struct

    module OM = Tpcc.Customer 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
    let t =
        let open Irmin.Type in
        let open Tpcc.Customer in
        ((((((((record "t"
                  (fun c_id ->
                     fun c_d_id ->
                       fun c_w_id ->
                         fun c_bal ->
                           fun c_ytd_payment ->
                             fun c_payment_cnt ->
                               fun c_delivery_cnt ->
                                 {
                                   c_id;
                                   c_d_id;
                                   c_w_id;
                                   c_bal;
                                   c_ytd_payment;
                                   c_payment_cnt;
                                   c_delivery_cnt
                                 }))
                 |+ (field "c_id" id (fun t -> t.c_id)))
                |+ (field "c_d_id" id (fun t -> t.c_d_id)))
               |+ (field "c_w_id" id (fun t -> t.c_w_id)))
              |+ (field "c_bal" int32 (fun t -> t.c_bal)))
             |+
             (field "c_ytd_payment" int32 (fun t -> t.c_ytd_payment)))
            |+
            (field "c_payment_cnt" int32 (fun t -> t.c_payment_cnt)))
           |+
           (field "c_delivery_cnt" int32 (fun t -> t.c_delivery_cnt))) 
          |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end
end
