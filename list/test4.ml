

(* Utility functions *)
(* U is a module with two functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end 

(* Set - AVL Tree *)
let _ =
  U.print_header "List";
let module MkConfig (Vars: sig val root: string end) : Ilist.Config = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end in 
let module IntAtom = struct
  type t = int64
  let compare = Pervasives.compare
  let t = Irmin.Type.int64
  let to_string = Int64.to_string
  let of_string = Int64.of_string
  let resolve x y = Int64.of_int 0
  let merge3 ~ancestor x y = Int64.of_int 0
end in 

let module CInit = MkConfig(struct let root = "/tmp/repos/init.git" end) in 
let module MInit = Ilist.MakeVersioned(CInit)(IntAtom) in 
let module M = Mvector_list.Make(IntAtom) in 
let module Vpst = MInit.Vpst in 

let (>>=) = Vpst.bind  in

let thread2_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  (* Thread2 adds 40 and 60 *)
  let c0' = MInit.OM.delete c0 0   in
  (* Thread2 syncs with master. Observes no changes. *)
  Vpst.sync_next_version ~v:c0' >>= fun c1 ->
  (* Thread2 blocks for 0.5s *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.5 >>= fun () ->
  (* Thread2 decrements by 9. *)
  let c1' = MInit.OM.delete c1 1   in 
  Vpst.sync_next_version ~v:c1' >>= fun c2 ->
  let _ = Printf.printf "merged : %s\n" (U.string_of_list IntAtom.to_string c2) in 
  (*let _ = (M.print_set print_int64 c2) in*)
  Vpst.return ()  in 


 let thread1_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  (* Thread1 forks thread2 *)
  Vpst.fork_version thread2_f >>= fun () ->
  (* Thread1 blocks for 0.1s *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Increments the counter twice - by 2 and 3, resp. *)
  let c0' = MInit.OM.delete c0 2  in
  Vpst.sync_next_version ~v:c0' >>= fun c1 ->
  (* Thread1 blocks on some operation *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Decrements by 4. *)
  let c1' = MInit.OM.delete c1 3 in
  (* Syncs with the master again. Publishes 11. *)
  Vpst.sync_next_version ~v:c1' >>= fun c2 ->
  (*let _ = Printf.printf "thread1: %d\n" c2 in*)
  let _ = Printf.printf "merged : %s\n" (U.string_of_list IntAtom.to_string c2) in 
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.sync_next_version ~v:c2 >>= fun c3->
  let _ = Printf.printf "merged : %s\n" (U.string_of_list IntAtom.to_string c3) in 
  (*let _ = Printf.printf "thread1 (before exiting): %d\n" c3 in*)
  Vpst.return ()   in 

  let main () =
   let original = [Int64.of_int 1; Int64.of_int 2; Int64.of_int 3; Int64.of_int 4] in
   Vpst.with_init_version_do original thread1_f in 

main ();;
