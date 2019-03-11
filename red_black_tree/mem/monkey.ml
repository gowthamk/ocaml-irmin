open Printf

(* Utility functions *)
(* U is a module with two functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"

  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")

  let (>>=) = Lwt.Infix.(>>=)

  let rec loop_until_y (msg:string) : unit Lwt.t = 
    Lwt_io.printf "%s" msg >>= fun _ ->
    Lwt_io.read_line Lwt_io.stdin >>= fun str ->
    if str="y" then Lwt.return ()
    else loop_until_y msg

  let fold f n b = 
    let rec fold_aux f i b = 
      if i >= n then b 
      else fold_aux f (i+1) @@ f i b in
    fold_aux f 0 b
end 

(* Canvas *)
let _ =
  U.print_header "Red-Black Set"

module MkConfig (Vars: sig val root: string end) : Irbset.Config = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end

module Atom = struct
  type t = int32
  let t = Irmin.Type.int32
  let compare x y = Int32.to_int @@ Int32.sub x y
  let to_string = Int32.to_string
  let of_string = Int32.of_string
end

module CInit = MkConfig(struct let root = "/tmp/repos/rbset.git" end)
module IRBSet =Irbset.MakeVersioned(CInit)(Atom)
module R = Rbset.Make(Atom)
module RBSet = R
module Vpst = IRBSet.Vpst

let uris = [(*"git+ssh://opam@172.18.0.3/tmp/repos/rbset.git";
            "git+ssh://opam@172.18.0.4/tmp/repos/rbset.git"*)]

let seed = 564294298

let _ = Random.init seed

let (>>=) = Vpst.bind

let loop_until_y msg = Vpst.liftLwt @@ U.loop_until_y msg

let do_an_insert t = 
  RBSet.add (Random.int32 Int32.max_int) t

let do_a_remove t = 
  if RBSet.is_empty t then t
  else RBSet.remove (RBSet.choose t) t

let do_an_oper t = 
  if Random.int 9 < 8 then
    do_an_insert t
  else
    do_a_remove t

let comp_time = ref 0.0

let sync_time = ref 0.0

let _n_ops_per_round = ref 30

let _n_rounds = ref 10

let loop_iter i (pre: RBSet.t Vpst.t) : RBSet.t Vpst.t = 
  pre >>= fun t ->
  let t1 = Sys.time() in
  let t' = if i<2 then 
              U.fold (fun _ t -> do_an_insert t) !_n_ops_per_round t
           else 
              U.fold (fun _ t -> do_an_oper t) !_n_ops_per_round t in
  let t2 = Sys.time() in
  Vpst.sync_next_version ~v:t' >>= fun v ->
  let t3 = Sys.time () in
  let _ = flush_all() in
  begin 
    comp_time := !comp_time +. (t2 -. t1);
    sync_time := !sync_time +. (t3 -. t2);
    printf "Round %d\n" i;
    flush_all();
    Vpst.return v
  end

let n_done = ref 0

let work_loop () : RBSet.t Vpst.t = 
  U.fold loop_iter !_n_rounds (Vpst.get_latest_version ()) >>= fun v ->
  n_done := !n_done + 1;
  Vpst.return v

let reset () =
  begin 
    comp_time := 0.0;
    IRBSet.merge_time := 0.0;
    IRBSet.merge_count := 0;
    IRBSet.real_merge_time :=0.0;
  end

let rec wait_till_done () : unit Vpst.t = 
  if !n_done = 3 then Vpst.return ()
  else begin 
    Vpst.liftLwt @@ Lwt_unix.sleep 1.0 >>= fun _ ->
    wait_till_done ()
  end

let experiment_f (fp: out_channel) : unit =
  begin
    CInit.init ();
    Vpst.with_init_version_do RBSet.Empty
      begin 
        Vpst.fork_version (work_loop ()) >>= fun br1 ->
        Vpst.fork_version ~parent:br1 (work_loop ()) >>= fun br2 ->
        Vpst.set_parent br2 >>= fun () ->
        (work_loop ()) >>= fun _ ->
        wait_till_done ()
      end;
    let mtime = !IRBSet.merge_time in
    let ctime = !comp_time in
    let stime = !sync_time in
    let mcount = !IRBSet.merge_count in
    let real_mtime = !IRBSet.real_merge_time in
    let total_rounds = 3 * !_n_rounds in
    let ctime_per_round = ctime/.(float total_rounds) in
    let mdivisor = mcount (*if mcount > total_rounds then mcount 
                   else total_rounds*) in
    let avg_mtime = real_mtime/.(float mdivisor) in
    fprintf fp "%d,%d,%fs,%fs,%fs,%d,%fs,%fs,%fs\n" 
                !_n_rounds !_n_ops_per_round 
                mtime ctime stime mcount real_mtime 
                ctime_per_round avg_mtime;
    reset ()
  end

let main () =
  begin
    Logs.set_reporter @@ Logs.format_reporter ();
    Logs.set_level @@ Some Logs.Error;
    _n_rounds := int_of_string @@ Sys.argv.(1);
    _n_ops_per_round := int_of_string @@ Sys.argv.(2);
    if Array.length Sys.argv > 3 then
      Random.init @@ int_of_string @@ Sys.argv.(3)
    else Random.self_init ();
    let fp = open_out_gen [Open_append; Open_creat] 
              0o777 "results.csv" in
    experiment_f fp
  end;;

main ();;
