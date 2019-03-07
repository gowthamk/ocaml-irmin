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

let alice_uri = "git+ssh://opam@172.18.0.2/tmp/repos/canvas.git"

let uris = [alice_uri]

let seed = 2485059249

let canvas_size = 64

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

let loop_iter i (pre: M.t Vpst.t) : M.t Vpst.t = 
  pre >>= fun t ->
  let t1 = Sys.time() in
  let c' =  U.fold (fun _ c -> do_an_oper c) 10 (mk t) in
  let t2 = Sys.time () in
  Vpst.sync_next_version ~v:c'.M.t uris >>= fun v ->
  let t3 = Sys.time () in
  begin 
    comp_time := !comp_time +. (t2 -. t1);
    sync_time := !sync_time +. (t3 -. t2);
    Vpst.return v
  end

let main_loop : M.t Vpst.t = 
  U.fold loop_iter 10 (Vpst.get_latest_version ())

let bob_f : unit Vpst.t = 
  loop_until_y "Ready?" >>= fun () ->
  main_loop >>= fun v ->
  let _ = printf "Done\n" in 
  let _ = printf "Computational time: %fs\n" !comp_time in
  let _ = printf "Merge time: %fs\n" !MInit.merge_time in
  let _ = printf "Number of merges: %d\n" !MInit.merge_count in
  let _ = printf "Average merge time: %fs\n" 
            (if !MInit.merge_count > 0 
             then (!MInit.merge_time)/.(float !MInit.merge_count)
             else 0.0) in
  let _ = printf "Sync time: %fs\n" !sync_time in
  Vpst.return ()

let main () =
  let _ = CInit.init () in
  let (f: unit Vpst.t -> unit) = 
            Vpst.with_remote_version_do alice_uri in
  Logs.set_reporter @@ Logs.format_reporter ();
  Logs.set_level @@ Some Logs.Error;
   (*
    * Alice starts with a blank canvas.
    *)
  f bob_f;;

main ();;
