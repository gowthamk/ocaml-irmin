open Printf

(* Utility functions *)
(* U is a module with two functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"

  let print_header h = Printf.printf "%s\n" h

  let (>>=) = Lwt.Infix.(>>=)

  let rec loop_until_y (msg:string) : unit Lwt.t = 
    Lwt_io.printf "%s" msg >>= fun _ ->
    Lwt_io.read_line Lwt_io.stdin >>= fun str ->
    if str="y" then Lwt.return ()
    else loop_until_y msg
end 

(* Canvas *)
let _ =
  (U.print_header "Canvas"; flush_all())

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

let bob_uri = "git+ssh://opam@172.18.0.3/tmp/repos/canvas.git"

let (>>=) = Vpst.bind

let mk t = {M.max_x=128; M.max_y=128; M.t=t}

let loop_until_y msg = Vpst.liftLwt @@ U.loop_until_y msg

let alice_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun t0 -> 
  (*Vpst.fork_version bob_f >>= fun () ->*)
  let loc = {M.x=93;M.y=127} in
  let c0 = mk t0 in
  (* 
   * Alice sets the rgb value of the pixel at (93,127) to
   * (23,23,23).
   *)
  let c0' = M.set_px c0 loc @@ M.rgb 23l in
  loop_until_y "2. Is Bob ready?" >>= fun () ->
  Vpst.sync_next_version ~v:c0'.M.t [] >>= fun t1 ->
  Vpst.liftLwt @@ 
    Lwt_io.printf "3. Alice published <93,127>\n" >>= fun () ->
  (* Alice syncs with Bob. Sees Bob's coloring at (98,17) pixel. *)
  (*Vpst.liftLwt @@ Lwt_unix.sleep 0.4 >>= fun () ->*)
  loop_until_y "4. Sync with Bob?" >>= fun () ->
  Vpst.pull_remote bob_uri >>= fun () -> 
  Vpst.get_latest_version () >>= fun t1 ->
  let loc = {M.x=45; M.y=78} in
  let c1 = mk t1 in
  (*
   * Alice now sets the color of (45,78) pixel to 17.
   *)
  let c1' = M.set_px c1 loc @@ M.rgb 17l in
  Vpst.sync_next_version ~v:c1'.M.t [] >>= fun t2 ->
  Vpst.liftLwt @@ 
    Lwt_io.printf "5. Alice published <45,78>\n" >>= fun () ->
  (*Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->*)
  (* 
   * Alice syncs with Bob. Gets latest updates. 
   * Since Bob has also colored (45,78) pixel, the resultant
   * color of the pixel is the overlap of Alice's and Bob's.
   *)
  loop_until_y "8. Should I sync with Bob again?" >>= fun () ->
  Vpst.pull_remote bob_uri >>= fun () -> 
  Vpst.get_latest_version () >>= fun t2 ->
  let _ = Printf.printf "Alice: \n" in
  let _ = M.print @@ mk t2 in
  (*Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->*)
  Vpst.return ()

let main () =
  let (f: M.t -> unit Vpst.t -> unit) = Vpst.with_init_version_do in
   (*
    * Alice starts with a blank canvas.
    *)
  f M.blank alice_f;;

main ();;
