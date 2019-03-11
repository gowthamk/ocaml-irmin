open Printf

(* Utility functions *)
(* U is a module with two functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"

  let print_header h = 
    begin 
      Printf.printf "%s\n" h;
      flush_all();
    end

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
  U.print_header "Mergeable List"

module MkConfig (Vars: sig val root: string end) : Ilist.Config = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end

module Atom = struct
  type t = int64
  let t = Irmin.Type.int64
  let compare x y = Int64.to_int @@ Int64.sub x y
  let to_string = Int64.to_string
  let of_string = Int64.of_string
end
(*
 * --------------------------------------------------------
 * Note: this alice monkey is setup to do diff experiments.
 * For more general experiments, change the repo root path, and
 * uncomment bob_url
 * --------------------------------------------------------
 *)
module CInit = MkConfig(struct let root = "/tmp/repos/mlist2.git" end)
module Ilist =Ilist.MakeVersioned(CInit)(Atom)
module Mlist = Mlist.Make(Atom)
module Vpst = Ilist.Vpst

let uris = [(*"git+ssh://opam@172.18.0.3/tmp/repos/mlist.git";
            "git+ssh://opam@172.18.0.4/tmp/repos/mlist.git"*)]

let seed = 564294298

let _ = Random.init seed

let (>>=) = Vpst.bind

let loop_until_y msg = Vpst.liftLwt @@ U.loop_until_y msg

let do_an_insert t = 
  let pos = if Mlist.is_empty t then 0 
            else Random.int (List.length t) in
  let elt = Random.int64 Int64.max_int in
  Mlist.insert t pos elt

let do_a_remove t = 
  let pos = Random.int (List.length t) in
  Mlist.remove t pos

let do_an_oper t = 
  if Random.int 9 < 8 then
    do_an_insert t
  else
    do_a_remove t

let comp_time = ref 0.0

let sync_time = ref 0.0

let avg_tree_size = ref 0

let _n_ops_per_round = ref 30

let _n_rounds = ref 10

let loop_iter i (pre: Mlist.t Vpst.t) : Mlist.t Vpst.t = 
  pre >>= fun t ->
  let t1 = Sys.time() in
  let t' = if i<2 then 
              U.fold (fun _ t -> do_an_insert t) !_n_ops_per_round t
           else 
              U.fold (fun _ t -> do_an_oper t) !_n_ops_per_round t in
  let t2 = Sys.time() in
  Vpst.sync_next_version ~v:t' uris >>= fun v ->
  let t3 = Sys.time () in
  let _ = flush_all() in
  let size = List.length t' in
  begin 
    comp_time := !comp_time +. (t2 -. t1);
    sync_time := !sync_time +. (t3 -. t2);
    avg_tree_size := ((!avg_tree_size * i)+size)/( i+1);
    printf "Round %d\n" i;
    flush_all();
    Vpst.return v
  end

let work_loop () : Mlist.t Vpst.t = 
  U.fold loop_iter !_n_rounds (Vpst.get_latest_version ())

let experiment_f ((*fp: out_channel*)) : unit =
  begin
    CInit.init ();
    Vpst.with_init_version_do []
      begin 
        (*Vpst.fork_version (work_loop ()) >>= fun br1 ->
        Vpst.fork_version ~parent:br1 (work_loop ()) >>= fun br2 ->
        Vpst.set_parent br2 >>= fun () ->*)
        (work_loop ()) >>= fun _ ->
        Vpst.liftLwt @@ Lwt_unix.sleep 0.2
      end;
    printf "Done\n";
    let kb = (!avg_tree_size * 93)/1024 in
    printf "%d\n" kb;
    (*
    let mtime = !Ilist.merge_time in
    let ctime = !comp_time in
    let stime = !sync_time in
    let mcount = !Ilist.merge_count in
    let real_mtime = !Ilist.real_merge_time in
    let total_rounds = 3 * !_n_rounds in
    let ctime_per_round = ctime/.(float total_rounds) in
    let mdivisor = if mcount > total_rounds then mcount 
                   else total_rounds in
    let avg_mtime = real_mtime/.(float mdivisor) in
    fprintf fp "%d,%d,%fs,%fs,%fs,%d,%fs,%fs,%fs\n" 
                !_n_rounds !_n_ops_per_round 
                mtime ctime stime mcount real_mtime 
                ctime_per_round avg_mtime;
    reset ()
     *)
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
    (*let fp = open_out_gen [Open_append; Open_creat] 
              0o777 "results.csv" in*)
    experiment_f ()
  end;;

main ();;
