module Id = struct 
  type t = int64

  let t = Irmin.Type.int64

  let compare = Int64.compare
end

type id = Id.t

let id = Id.t

let counter_merge lca v1 v2 = 
  let (+) a b = Int32.add a b in
  let (-) a b = Int32.sub a b in
  lca + (v1-lca) + (v2-lca)

module Warehouse = struct
  type t = {w_id: id; mutable w_ytd: int32}

  let merge ~ancestor:lca {w_ytd=y1} {w_ytd=y2} =  
    {lca with w_ytd = (counter_merge lca.w_ytd y1 y2)}
end

module District = struct 
  type t = {d_id: id; d_w_id: id; 
            mutable d_ytd: int32;}

  let merge ~ancestor:lca {d_ytd=y1} {d_ytd=y2} =  
    {lca with d_ytd = counter_merge lca.d_ytd y1 y2}
end

module Order = struct

  type t = {o_id:id; o_w_id: id; o_d_id: id; 
            o_c_id: id; o_ol_cnt: int32; 
            mutable o_carrier_id: bool}

  let merge ~ancestor:lca ({o_carrier_id=b1} as v1) 
                          ({o_carrier_id=b2} as v2) =  
    if lca.o_carrier_id = true 
    then (assert (lca=v1 && v1=v2); lca)
    else {lca with o_carrier_id = b1 || b2}

end

module NewOrder = struct 

  type t = {no_o_id: id; no_d_id: id; no_w_id: id}

  let merge ~ancestor:lca v1 v2  =  
    failwith "new_order is immutable"
end

module OrderLine = struct 

  type t = {ol_o_id: id; ol_d_id: id; 
            ol_w_id: id; ol_num: int32; ol_amt: int32; 
            ol_i_id: id; ol_supply_w_id: id;
            ol_qty: int32; mutable ol_delivery_d: float option}

  let merge ~ancestor:lca {ol_delivery_d=d1} {ol_delivery_d=d2} = 
    let d = match d1,d2 with 
      | None,None -> None 
      | Some d1,None -> Some d1 
      | None,Some d2 -> Some d2
      | Some d1, Some d2 -> Some (min d1 d2) in
    {lca with ol_delivery_d=d}
end

module Item = struct
  type t = {i_id: id; i_name: string; i_price: int32}

  let merge ~ancestor v1 v2 = failwith "Item is immutable"
end

module Hist = struct 
  type t = {h_c_id: id; h_c_d_id: id; h_c_w_id: id; 
            h_d_id: id; h_w_id: id; h_amt: int32}

  let merge ~ancestor v1 v2 = failwith "Hist is immutable"

end

module Stock = struct 

  type t = {s_i_id: id; s_w_id: id; mutable s_qty: int32; 
            mutable s_ytd: int32; mutable s_order_cnt: int32}

  let merge ~ancestor:{s_i_id; s_w_id; s_qty=q; 
                       s_ytd=y; s_order_cnt=c}
          {s_qty=q1; s_ytd=y1; s_order_cnt=c1}
          {s_qty=q2; s_ytd=y2; s_order_cnt=c2} =
    let q' = counter_merge q q1 q2 in
    let y' = counter_merge y y1 y2 in
    let c' = counter_merge c c1 c2 in
    {s_i_id; s_w_id; s_qty=q'; s_ytd=y'; s_order_cnt=c'}
end

module Customer = struct

  type t = {c_id: id; c_d_id: id; c_w_id: id; 
            mutable c_bal: int32; 
            mutable c_ytd_payment:int32; 
            mutable c_payment_cnt:int32; 
            mutable c_delivery_cnt:int32;}

  (* Due to delivery txn, customer merge is non-trivial. We shall do
   * real merging of customer at the DB level. Following is called
   * from the db level merge function.*)

  let merge ~ancestor:{c_id; c_d_id; c_w_id; c_bal=b; 
                       c_ytd_payment=y; c_payment_cnt=p;
                       c_delivery_cnt=d} 
                      {c_bal=b1; c_ytd_payment=y1; 
                       c_payment_cnt=p1; c_delivery_cnt=d1}
                      {c_bal=b2; c_ytd_payment=y2; 
                       c_payment_cnt=p2; c_delivery_cnt=d2} =
    let b' = counter_merge b b1 b2 in
    let y' = counter_merge y y1 y2 in
    let p' = counter_merge p p1 p2 in
    let d' = counter_merge d d1 d2 in
    {c_id; c_d_id; c_w_id; c_bal=b'; c_ytd_payment=y';
     c_payment_cnt=p'; c_delivery_cnt=d'}
end

module IdPair = struct 
  type t = id*id

  let t = let open Irmin.Type in pair id id

  let compare (x1,y1) (x2,y2) = 
    match Id.compare x1 x2, Id.compare y1 y2 with
      | 0, v2 -> v2
      | v1, _ -> v1
end

module IdTriple = struct 
  type t = id*(id*id)

  let t = let open Irmin.Type in pair id (pair id id)

  let compare (x1,(y1,z1)) (x2,(y2,z2)) = 
    match Id.compare x1 x2, Id.compare y1 y2, Id.compare z1 z2 with
      | 0, 0, v3 -> v3
      | 0, v2, _ -> v2
      | v1, _, _ -> v1
end

module IdQuad = struct 
  type t = id*(id*(id*id))

  let t = let open Irmin.Type in pair id (pair id (pair id id))

  let compare (w1,(x1,(y1,z1))) (w2,(x2,(y2,z2))) = 
    match Id.compare w1 w2, Id.compare x1 x2, 
          Id.compare y1 y2, Id.compare z1 z2 with
      | 0, 0, 0, v4 -> v4
      | 0, 0, v3, _ -> v3
      | 0, v2, _, _ -> v2
      | v1, _, _, _ -> v1
end

module IdQuin = struct 
  type t = id*(id*(id*(id*id)))

  let t = let open Irmin.Type in 
    (pair id (pair id (pair id (pair id id))))

  let compare (u1,(w1,(x1,(y1,z1)))) (u2, (w2,(x2,(y2,z2)))) = 
    match Id.compare u1 u2, Id.compare w1 w2, Id.compare x1 x2, 
          Id.compare y1 y2, Id.compare z1 z2 with
      | 0, 0, 0, 0, v5 -> v5
      | 0, 0, 0, v4, _ -> v4
      | 0, 0, v3, _, _ -> v3
      | 0, v2, _, _, _ -> v2
      | v1, _, _, _, _ -> v1
end

module WarehouseTable = Rbmap.Make(Id)(Warehouse)

module DistrictTable = Rbmap.Make(IdPair)(District)

module OrderTable = Rbmap.Make(IdQuad)(Order)

module NewOrderTable = Rbmap.Make(IdTriple)(NewOrder)

module OrderLineTable = Rbmap.Make(IdQuad)(OrderLine)

module ItemTable = Rbmap.Make(Id)(Item)

module HistTable = Rbmap.Make(IdQuin)(Hist)

module StockTable = Rbmap.Make(IdPair)(Stock)

module CustomerTable = Rbmap.Make(IdTriple)(Customer)

type db = {warehouse_table: WarehouseTable.t; 
           district_table: DistrictTable.t;
           order_table: OrderTable.t;
           neworder_table: NewOrderTable.t;
           orderline_table: OrderLineTable.t;
           item_table: ItemTable.t;
           hist_table: HistTable.t;
           stock_table: StockTable.t;
           customer_table: CustomerTable.t}
