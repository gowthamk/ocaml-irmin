open Printf
open Msigs
module U = Utils

let _ = U.print_header "--------- TPC-C -------"

module MkConfig (Vars: sig val root: string end) : CONFIG = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end

(*
 * --------------------------------------------------------
 * Note: this monkey is setup to do diff experiments. For more
 * general experiments, change the repo root path, and uncomment
 * bob_url
 * --------------------------------------------------------
 *)

module CInit = MkConfig(struct let root = "/tmp/repos/tpcc2.git" end)
module Itpcc = Itpcc.MakeVersioned(CInit)
module Vpst = Itpcc.Vpst

let uris = []

let (>>=) = Vpst.bind

let _n_txns = ref 500
let lat = ref 0.0

let execute_script () =
  Unix.execvp "./get_size.sh" (Array.make 0 "")

let do_txn (fp: out_channel) i (pre: Tpcc.db Vpst.t) 
                                      : Tpcc.db Vpst.t =
  pre >>= fun db ->
  let txn_name = sprintf "txn_%d" i in
  let _ = printf "Transaction %d started\n" i in
  let _ = flush_all () in
  let t1 = Sys.time() in
  let n = (Random.int 5) + 1 in
  let db' = 
    try 
      if n<=2 then Db.do_new_order db 
      else if n<=4 then Db.do_payment db 
      else Db.do_delivery db 
    with Not_found -> 
      (Db.dump_keys db; raise Not_found) in
  let _ = Gc.minor () in
  Vpst.sync_next_version ~v:db' uris txn_name >>= fun db' ->
  let t2 = Sys.time () in
  begin
    printf "Transaction %d done (%fs)\n" i (t2-.t1);
    match Unix.fork () with
      | 0 -> execute_script ()
      | _ -> printf "Continuing\n";
    lat := !lat +. (t2-.t1);
    if (i+1) mod 5 = 0 
    then begin
      Vpst.get_size_on_disk () >>= fun bytes ->
      let _ = fprintf fp "%d, %f\n" (i+1) (!lat /. (float (i+1))) in
      let _ = flush_all() in
      Vpst.return db'
    end
    else Vpst.return db'
  end


(*let populate_db () = 
  begin 
    let (_,db) = Db.populate () (Db.empty ()) in
    let _ = printf "In memory database populated.\n" in
    let _ = printf "Writing to disk ... \n" in
    let _ = flush_all () in
    Vpst.with_init_version_do db
      begin
        Vpst.return ()
      end;
    printf "Done. DB populated.\n";
  end*)

let experiment_f (fp: out_channel) : unit =
  begin
    CInit.init ();
    let (_,init_db) = Db.populate () (Db.empty ()) in
    let _ = printf "In memory database populated.\n" in
    let _ = printf "Writing to disk ... \n" in
    let _ = flush_all () in
    Vpst.with_init_version_do init_db
    (*Vpst.with_persistent_version_do*)
      begin 
        Vpst.return (printf "Done. DB populated.\n";
                     flush_all ()) >>= fun _ ->
        U.fold (do_txn fp) !_n_txns 
          (Vpst.get_latest_version ()) >>= fun _ ->
        Vpst.return ()
      end;
    printf "Done. Transactions executed.\n";
  end

let main () =
  begin
    Logs.set_reporter @@ Logs.format_reporter ();
    Logs.set_level @@ Some Logs.Info;
    Gc.set { (Gc.get()) with Gc.major_heap_increment=50; 
                             Gc.max_overhead = 1000000; 
                             Gc.allocation_policy = 1;
                             (*Gc.verbose = 0x015*) };
    if Array.length Sys.argv > 1 then
      _n_txns := int_of_string @@ Sys.argv.(1);
    let fp = open_out_gen [Open_creat; Open_append] 
        0o777 "txn_results.csv" in
    fprintf fp "Txn no., Avg. Latency (s)\n";
    (* populate_db () *)
    experiment_f (fp);
    close_out fp;
  end;;

main ();;
