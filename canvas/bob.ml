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

let (>>=) = Vpst.bind

let mk t = {M.max_x=128; M.max_y=128; M.t=t}

let loop_until_y msg = Vpst.liftLwt @@ U.loop_until_y msg

let bob_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun t0 -> 
  let loc = {M.x=98;M.y=17} in
  let c0 = mk t0 in
  (* 
   * Bob sets the rgb value of the pixel at (98,17) to
   * (23,23,23).
   *)
  let c0' = M.set_px c0 loc @@ M.rgb @@ Char.chr 23 in
  (* Bob publishes his version *)
  Vpst.sync_next_version ~v:c0'.M.t >>= fun t1 ->
  Vpst.liftLwt @@ 
    Lwt_io.printf "1. Bob published <98,17>\n" >>= fun () ->
  let loc = {M.x=45; M.y=78} in
  let c1 = mk t1 in
  (*
   * Bob now colors (45,78) pixel with an rgb value of
   * (111,111,111).
   *)
  let c1' = M.set_px c1 loc @@ M.rgb @@ Char.chr 111 in
  (*Vpst.liftLwt @@ Lwt_unix.sleep 1.0 >>= fun () ->*)
  loop_until_y "6. Sync with Alice?" >>= fun () ->
  (* 
   * Bob syncs with Alice. Gets latest updates. 
   * Since Alice has also colored (45,78) pixel, but with an 
   * rgb value of (17,17,17), the resultant color of the pixel 
   * is the overlap of both colors.
   *)
  Vpst.pull_remote alice_uri >>= fun () -> 
  Vpst.sync_next_version ~v:c1'.M.t >>= fun t2 ->
  Vpst.liftLwt @@ 
    Lwt_io.printf "7. Bob merged & published <45,78>\n" >>= fun () ->
  let _ = Printf.printf "Bob: \n" in
  let _ = M.print @@ mk t2 in
  Vpst.return ()
  
let main () =
  let (f: unit Vpst.t -> unit) = 
            Vpst.with_remote_version_do alice_uri in
   (*
    * Alice starts with a blank canvas.
    *)
  f bob_f;;

main ();;
