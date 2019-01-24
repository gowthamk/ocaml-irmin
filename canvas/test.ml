open Printf

(* Utility functions *)
(* U is a module with two functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end 

(* Canvas *)
let _ =
  U.print_header "Canvas";
let module MkConfig (Vars: sig val root: string end) : Icanvas.Config = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end in 

let module CInit = MkConfig(struct let root = "/tmp/repos/canvas.git" end) in 
let module MInit = Icanvas.MakeVersioned(CInit) in 
let module M = Canvas.Make in 
let module Vpst = MInit.Vpst in 

let (>>=) = Vpst.bind  in


let mk t = {M.max_x=128; M.max_y=128; M.t=t} in 

let bob_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun t0 -> 
  let loc = {M.x=98;M.y=17} in
  let c0 = mk t0 in
  (* 
   * Bob sets the rgb value of the pixel at (98,17) to
   * (23,23,23).
   *)
  let c0' = M.set_px c0 loc @@ M.rgb @@ Char.chr 23 in
  (* Bob syncs with Alice. Observes no changes. *)
  Vpst.sync_next_version ~v:c0'.M.t >>= fun t1 ->
  let loc = {M.x=45; M.y=78} in
  let c1 = mk t1 in
  (*
   * Bob now colors (45,78) pixel with an rgb value of
   * (111,111,111).
   *)
  let c1' = M.set_px c1 loc @@ M.rgb @@ Char.chr 111 in
  Vpst.liftLwt @@ Lwt_unix.sleep 1.0 >>= fun () ->
  (* 
   * Bob syncs with Alice. Gets latest updates. 
   * Since Alice has also colored (45,78) pixel, but with an 
   * rgb value of (17,17,17), the resultant color of the pixel 
   * is the overlap of both colors.
   *)
  Vpst.sync_next_version ~v:c1'.M.t >>= fun t2 ->
  let _ = Printf.printf "Bob: \n" in
  let _ = M.print @@ mk t2 in
  Vpst.return () in 
  
let alice_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun t0 -> 
  (*
   * Alice invites Bob for collaboration.
   *)
  Vpst.fork_version bob_f >>= fun () ->
  let loc = {M.x=93;M.y=127} in
  let c0 = mk t0 in
  (* 
   * Alice sets the rgb value of the pixel at (93,127) to
   * (23,23,23).
   *)
  let c0' = M.set_px c0 loc @@ M.rgb @@ Char.chr 23 in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.4 >>= fun () ->
  (* Alice syncs with Bob. Sees Bob's coloring at (98,17) pixel. *)
  Vpst.sync_next_version ~v:c0'.M.t >>= fun t1 ->
  let loc = {M.x=45; M.y=78} in
  let c1 = mk t1 in
  (*
   * Alice now sets the color of (45,78) pixel to 17.
   *)
  let c1' = M.set_px c1 loc @@ M.rgb @@ Char.chr 17 in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* 
   * Alice syncs with Bob. Gets latest updates. 
   * Since Bob has also colored (45,78) pixel, the resultant
   * color of the pixel is the overlap of Alice's and Bob's.
   *)
  Vpst.sync_next_version ~v:c1'.M.t >>= fun t2 ->
  let _ = Printf.printf "Alice: \n" in
  let _ = M.print @@ mk t2 in
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.return () in 

let main () =
  let (f: M.t -> unit Vpst.t -> unit) = Vpst.with_init_version_do in
   (*
    * Alice starts with a blank canvas.
    *)
   f M.blank alice_f in 

main ();;
