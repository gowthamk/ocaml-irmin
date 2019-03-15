open Printf
open Tpcc
module U = Utils

let _max_items = 1000
let _max_ware = 3
let _cust_per_dis = 300
let _dist_per_ware = 10
let _ord_per_dist = 300
let _least_undelivered_o_id = 210

let seed = 80032723

let _ = Random.init seed

let (>>=) = Txn.bind


let load_item i = 
  let open Item in 
  let i_id = Id.of_int i in
  let i_name= sprintf "item %d" i in
  let i_price = Random.int32 2000l in
  let item = {i_id; i_name; i_price} in
  Insert.item_table item

let load_ware i =
  let open Warehouse in
  let w_id = Id.of_int i in
  let w_ytd = 300000l in
  Insert.warehouse_table {w_id; w_ytd}

let load_stock i j =
  let open Stock in
  let s_w_id = Id.of_int i in
  let s_i_id = Id.of_int j in
  let s_qty = U.bounded_random_int32 10l 100l in
  let s_ytd = 0l in
  let s_order_cnt = 0l in
  Insert.stock_table {s_w_id; s_i_id; s_qty;
                      s_ytd; s_order_cnt}

let load_cust i j k = 
  let open Customer in
  let c_w_id = Id.of_int i in
  let c_d_id = Id.of_int j in
  let c_id = Id.of_int k in
  let c_bal = Int32.neg 10l in
  let c_ytd_payment = 10l in
  let c_payment_cnt = 1l in
  let c_delivery_cnt = 0l in
  Insert.customer_table {c_w_id; c_d_id; c_id;
                         c_bal; c_ytd_payment; 
                         c_payment_cnt; c_delivery_cnt}

let load_dist i j = 
  let open District in
  let d_w_id = Id.of_int i in
  let d_id = Id.of_int j in
  let d_ytd = 3000l in
  Insert.district_table {d_w_id; d_id; d_ytd}

let load_order i j k =
  let open Order in
  let open NewOrder in
  let open OrderLine in
  let o_w_id = Id.of_int i in
  let o_d_id = Id.of_int j in
  let o_id = Id.of_int k in
  let o_c_id = Id.of_int @@ Random.int _cust_per_dis in
  let o_ol_cnt = U.bounded_random_int32 5l 15l in
  let o_carrier_id = if i >= _least_undelivered_o_id 
                     then false else true in
  Insert.order_table {o_w_id; o_d_id; o_id; o_c_id;
                      o_ol_cnt; o_carrier_id} >>= fun _ ->
  (if i >= _least_undelivered_o_id 
  then Insert.neworder_table 
      {no_o_id=o_id; no_d_id=o_d_id; no_w_id=o_w_id}
  else Txn.return ()) >>= fun _ ->
  U.fold 
    (fun l pre -> 
      pre>>= fun _ -> 
      let ol_i_id = Id.of_int @@ Random.int _max_items in
      let ol_num = Int32.of_int l in
      let ol_supply_w_id = o_w_id in
      let ol_amt = if i>= _least_undelivered_o_id 
                   then 0l else U.bounded_random_int32 1l 100l in 
      let ol_qty = 5l in
      let ol_delivery_d = if i >= _least_undelivered_o_id 
                          then None else Some (Unix.time ()) in
      Insert.orderline_table {ol_w_id=o_w_id; ol_d_id=o_d_id; 
                              ol_o_id=o_id; ol_i_id; ol_num;
                              ol_supply_w_id; ol_amt; ol_qty;
                              ol_delivery_d})
    (Int32.to_int o_ol_cnt)
    (Txn.return ())

let load_items () =
  U.fold (fun i pre -> pre >>= fun () -> 
                      load_item i)
    _max_items (Txn.return ())

let load_ware () = 
  U.fold (fun i pre -> pre >>= fun () -> 
                      load_ware i)
    _max_ware (Txn.return ())

let load_stock () =
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 ->
        pre2 >>= fun () -> load_stock i j)
      _max_items pre1)
    _max_ware (Txn.return ())

let load_dist () =
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 ->
        pre2 >>= fun () -> load_dist i j)
      _dist_per_ware pre1)
    _max_ware (Txn.return ())

let load_cust () = 
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 -> 
      U.fold (fun k pre3 ->
          pre3 >>= fun () -> load_cust i j k)
        _cust_per_dis pre2)
      _dist_per_ware pre1)
    _max_ware (Txn.return ())

let load_ord () = 
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 -> 
      U.fold (fun k pre3 ->
          pre3 >>= fun () -> load_order i j k)
        _ord_per_dist pre2)
      _dist_per_ware pre1)
    _max_ware (Txn.return ())

let populate () = 
	load_items() >>= fun _ ->
	load_ware() >>= fun _ ->
	load_stock() >>= fun _ ->
	load_dist() >>= fun _ ->
	load_cust() >>= fun _ ->
  load_ord ()

let empty () =
  {warehouse_table= WarehouseTable.empty; 
   district_table= DistrictTable.empty;
   order_table= OrderTable.empty;
   neworder_table= NewOrderTable.empty;
   orderline_table= OrderLineTable.empty;
   item_table= ItemTable.empty;
   hist_table= HistTable.empty;
   stock_table= StockTable.empty;
   customer_table= CustomerTable.empty}

