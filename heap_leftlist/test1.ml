

(* Utility functions *)
(* U is a module with two functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end 

(* Set - AVL Tree *)
let _ =
  U.print_header "Heap-LeftList";
let module MkConfig (Vars: sig val root: string end) : Iheaplist.Config = struct
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
end in 

let module CInit = MkConfig(struct let root = "/tmp/repos/init.git" end) in 
let module MInit = Iheaplist.MakeVersioned(CInit)(IntAtom) in 
let module M = Mheap_leftlist.Make(IntAtom) in 
let module Vpst = MInit.Vpst in 

let (>>=) = Vpst.bind  in

let thread2_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  (* Thread2 adds 40 and 60 *)
  let c0' = c0 |> (MInit.OM.insert (Int64.of_int 40)) |> (MInit.OM.insert (Int64.of_int 60)) in
  (* Thread2 syncs with master. Observes no changes. *)
  Vpst.sync_next_version ~v:c0' >>= fun c1 ->
  (* Thread2 blocks for 0.5s *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.5 >>= fun () ->
  (* Thread2 decrements by 9. *)
  let c1' = c1 |> (MInit.OM.insert (Int64.of_int 6))  in 
  Vpst.sync_next_version ~v:c1' >>= fun c2 ->
  let _ = Printf.printf "merged : %s\n" (U.string_of_list IntAtom.to_string (MInit.OM.elements c2)) in 
  (*let _ = (M.print_set print_int64 c2) in*)
  Vpst.return ()  in 


 let thread1_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  (* Thread1 forks thread2 *)
  Vpst.fork_version thread2_f >>= fun () ->
  (* Thread1 blocks for 0.1s *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Increments the counter twice - by 2 and 3, resp. *)
  let c0' = c0 |> (MInit.OM.insert (Int64.of_int 4)) |> (MInit.OM.insert (Int64.of_int 3)) |> (MInit.OM.insert (Int64.of_int 2))  in
  Vpst.sync_next_version ~v:c0' >>= fun c1 ->
  (* Thread1 blocks on some operation *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Decrements by 4. *)
  let c1' = c1 |> (MInit.OM.insert (Int64.of_int 1)) in
  (* Syncs with the master again. Publishes 11. *)
  Vpst.sync_next_version ~v:c1' >>= fun c2 ->
  (*let _ = Printf.printf "thread1: %d\n" c2 in*)
  let _ = Printf.printf "merged : %s\n" (U.string_of_list IntAtom.to_string (MInit.OM.elements c2)) in 
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.sync_next_version ~v:c2 >>= fun c3->
  let _ = Printf.printf "merged : %s\n" (U.string_of_list IntAtom.to_string (MInit.OM.elements c3)) in 
  (*let _ = Printf.printf "thread1 (before exiting): %d\n" c3 in*)
  Vpst.return ()   in 

  let main () =
   (*
    * thread1 starts with a blank canvas.
    *)
   let original = MInit.OM.empty |> MInit.OM.insert (Int64.of_int 10) |> MInit.OM.insert (Int64.of_int 5) |> MInit.OM.insert (Int64.of_int 20) in
   Vpst.with_init_version_do original thread1_f in 

main ();;
