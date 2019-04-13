open Utils
module SHA1 = Digestif.SHA1

let _max_block_size = 100

module UserId = struct
  type t = Uuidm.t

  let (random: unit -> t) = 
    let st = Random.get_state () in
    Uuidm.v4_gen st

  let genesis = from_just
    (Uuidm.of_string "00000000-0000-0000-0000-000000000000")
    "UserId.genesis"

  let compare = Uuidm.compare

  let to_string = Uuidm.to_string

  let of_string = Uuidm.of_string
end

let genesis_uid = UserId.genesis

module Txn = struct
  type t = {timestamp: float;
            sender: UserId.t; 
            receiver: UserId.t; 
            amount: int32}

  let compare t1 t2 = 
    let diff = t1.timestamp -. t2.timestamp in
    if diff <= epsilon_float && 
       diff >= (-1.0 *. epsilon_float) then 0
    else if diff < 0.0 then -1
    else 1
end

module TxnSet = Rbset.Make(Txn)

module Block = struct
  type t = {index: int32;
            txns: TxnSet.t; 
            timestamp: float; 
            proof:int64}

  let compare b1 b2 = Int32.compare b1.index b2.index
end

type t = {txns: TxnSet.t; chain: Block.t list}

let new_txn s_id r_id amt t = 
  let open Txn in
  let ts = Unix.gettimeofday () in
  let txn = {timestamp=ts;
             sender=s_id; 
             receiver=r_id; 
             amount=amt} in
  {t with txns = TxnSet.add txn t.txns}


let valid_proof last_proof proof = 
  let str1 = Int64.to_string last_proof in
  let str2 = Int64.to_string proof in
  let str = str1^str2 in
  let hex = SHA1.to_hex @@ SHA1.digest_string str in
  String.sub hex 0 4 = "0000"

let proof_of_work last_proof = 
  let rec loop_iter i = 
    if i >= Int64.max_int then failwith "No proof!"
    else if valid_proof last_proof i then i
    else loop_iter (Int64.add i 1L) in
  loop_iter 0L

let valid_txn chain txn = true

let valid_block chain block = true

let confirmed_txn chain txn = true

let mine my_uid t = 
  let (s,txns') = TxnSet.split_k _max_block_size t.txns in
  let confirmed_txns = TxnSet.filter (valid_txn t.chain) s in
  let open Block in
  let last_block = List.hd t.chain in
  let last_index = last_block.index in
  let last_proof = last_block.proof in
  let proof = proof_of_work last_proof in
  let ts = Unix.gettimeofday () in
  let open Txn in
  let reward_txn = {timestamp=ts;
                    sender=genesis_uid;
                    receiver=my_uid;
                    amount=25l;} in
  let block = {index = Int32.add last_index 1l;
               txns=TxnSet.add reward_txn 
                        confirmed_txns;
               timestamp=ts;
               proof=proof} in
  {txns=txns'; chain=block::t.chain}

let init my_uid = 
  let ts = Unix.gettimeofday () in
  let open Txn in
  let open Block in
  let init_txn = {timestamp=ts; 
                  sender=genesis_uid;
                  receiver=my_uid;
                  amount=25l} in
  let genesis_block = {index=1l;
                       txns=TxnSet.singleton init_txn;
                       timestamp=ts;
                       proof=100L} in
  {txns=TxnSet.empty; chain=[genesis_block]}

let get_valid_extension oldc newc = 
  try
    let new_blocks = get_prefix newc ~suffix:oldc in
    let _ = List.fold_right 
      (fun block chain -> if valid_block chain block 
                          then block::chain
                          else raise (Invalid_argument ""))
      new_blocks oldc in
    Some new_blocks
  with Invalid_argument _ -> None

let dissolve (block:Block.t) ~into:t = 
  let open Block in
  TxnSet.fold
    (fun txn t' -> if confirmed_txn t'.chain txn then t' 
                   else {t' with txns=TxnSet.add txn t'.txns})
    block.txns t

let dissolve blocks ~into:t = 
  List.fold_left (fun t' block -> dissolve block ~into:t') t blocks

let merge old v1 v2 = 
  let len = List.length in
  let oldc, v1c, v2c = old.chain, v1.chain, v2.chain in
  match get_valid_extension oldc v1c, 
        get_valid_extension oldc v2c with
    | None, Some new2 -> v2
    | Some _, None -> v1
    | Some _, Some _ when v1=v2 -> v1
    | Some _, Some new2 when len v1c > len v2c -> 
            dissolve new2 ~into:v1
    | Some new1, Some _ when len v1c < len v2c -> 
            dissolve new1 ~into:v2
    | Some new1, Some new2 -> 
      let open Block in
      let add_proof acc block = Int64.add acc block.proof in
      let proof_sum1 = List.fold_left add_proof 0L new1 in
      let proof_sum2 = List.fold_left add_proof 0L new2 in
      if proof_sum1 > proof_sum2 
      then dissolve new2 ~into:v1 
      else dissolve new1 ~into:v2
    | None, None -> failwith "--- this cannot happen ---"
