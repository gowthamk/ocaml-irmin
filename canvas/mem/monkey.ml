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
  U.print_header "Canvas"

module MkConfig (Vars: sig val root: string end) : Icanvas.Config = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end

module CInit = MkConfig(struct let root = "/tmp/repos/canvas.git" end)
module MInit = Icanvas.MakeVersioned(CInit)
module M = Canvas.Make
module Vpst = MInit.Vpst

let canvas_size = 512

let (>>=) = Vpst.bind

let mk t = {M.max_x=canvas_size; M.max_y=canvas_size; M.t=t}

let loop_until_y msg = Vpst.liftLwt @@ U.loop_until_y msg

let do_an_oper t = 
  let (x,y) = (Random.int canvas_size, 
               Random.int canvas_size) in
  let loc = {M.x=x; M.y=y} in 
  let (r,g,b) = (Random.int32 256l, 
                 Random.int32 256l, 
                 Random.int32 256l) in 
  (*let _ = printf "<%d,%d,%d>\n" r g b in*)
  let color = {M.r=r; M.g=g; M.b=b} in
  M.set_px t loc color

let comp_time = ref 0.0

let sync_time = ref 0.0

let _n_ops_per_round = ref 30

let _n_rounds = ref 10

let loop_iter i (pre: M.t Vpst.t) : M.t Vpst.t = 
  pre >>= fun t ->
  let t1 = Sys.time() in
  let c' =  U.fold (fun _ c -> do_an_oper c) !_n_ops_per_round (mk t) in
  let t2 = Sys.time () in
  Vpst.sync_next_version ~v:c'.M.t >>= fun v ->
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

let work_loop () : M.t Vpst.t = 
  U.fold loop_iter !_n_rounds (Vpst.get_latest_version ()) >>= fun v ->
  n_done := !n_done + 1;
  Vpst.return v

let reset () =
  begin 
    comp_time := 0.0;
    MInit.merge_time := 0.0;
    MInit.merge_count := 0;
    M.merge_time :=0.0;
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
    Vpst.with_init_version_do M.blank
      begin 
        Vpst.fork_version (work_loop ()) >>= fun br1 ->
        Vpst.fork_version ~parent:br1 (work_loop ()) >>= fun br2 ->
        Vpst.set_parent br2 >>= fun () ->
        (work_loop ()) >>= fun _ ->
        wait_till_done ()
      end;
    let mtime = !MInit.merge_time in
    let ctime = !comp_time in
    let stime = !sync_time in
    let mcount = !MInit.merge_count in
    let real_mtime = !M.merge_time in
    let total_rounds = 3 * !_n_rounds in
    let ctime_per_round = ctime/.(float total_rounds) in
    let mdivisor1 = if mcount > total_rounds then mcount 
                    else total_rounds in
    let mdivisor2 = max mcount 1 in
    let avg_mtime1 = real_mtime/.(float mdivisor1) in
    let avg_mtime2 = real_mtime/.(float mdivisor2) in
    fprintf fp "%d,%d,%fs,%fs,%fs,%d,%fs,%fs,%fs,%fs\n" 
                !_n_rounds !_n_ops_per_round 
                mtime ctime stime mcount real_mtime 
                ctime_per_round avg_mtime1 avg_mtime2;
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
    let fp = open_out_gen [Open_append] 0o777 "results.csv" in
    experiment_f fp
  end;;

main ();;