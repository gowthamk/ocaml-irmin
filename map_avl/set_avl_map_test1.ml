

(* Utility functions *)
(* U is a module with two functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end 

(* Set - AVL Tree *)
let _ =
  U.print_header "Set - AVL tree";
let module MkConfig (Vars: sig val root: string end) : Imap_avl.Config = struct
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
  let resolve x y =  x + y 
  let merge3 ~ancestor x y = ancestor + (x - ancestor) + (y - ancestor)
  let equal x y = Pervasives.compare x y = 0 
  let to_string = Int64.to_string
  let of_string = Int64.of_string
end in 

 let module StringKey = struct
    include String
    let to_string (s:t):t = s
    let compare = Pervasives.compare
    let t = Irmin.Type.string
      let of_string s = s
end in 

let module CInit = MkConfig(struct let root = "/tmp/repos/init.git" end) in 
let module MInit = Imap_avl.MakeVersioned(CInit)(StringKey)(IntAtom) in 
let module M = Mmap_avltree.Make(StringKey)(IntAtom) in 
let module Vpst = MInit.Vpst in 

let (>>=) = Vpst.bind  in

let thread2_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  (* Thread2 adds 40 and 60 *)
  let c0' = c0 |> (MInit.OM.add "A" (Int64.of_int 40)) |> (MInit.OM.add "D" (Int64.of_int 60)) in
  (* Thread2 syncs with master. Observes no changes. *)
  Vpst.sync_next_version ~v:c0' >>= fun c1 ->
  (* Thread2 blocks for 0.5s *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.5 >>= fun () ->
  (* Thread2 decrements by 9. *)
  let c1' = c1 |> (MInit.OM.remove "C")  in 
  Vpst.sync_next_version ~v:c1' >>= fun c2 ->
  MInit.OM.iter (fun k a -> Printf.printf "%s : %s\n" k (IntAtom.to_string a) ) c2
  Vpst.return ()  in 


 let thread1_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  (* Thread1 forks thread2 *)
  Vpst.fork_version thread2_f >>= fun () ->
  (* Thread1 blocks for 0.1s *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Increments the counter twice - by 2 and 3, resp. *)
  let c0' = c0 |> (MInit.OM.add "Z" (Int64.of_int 4)) |> (MInit.OM.add "D" (Int64.of_int 70)) in
  Vpst.sync_next_version ~v:c0' >>= fun c1 ->
  (* Thread1 blocks on some operation *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Decrements by 4. *)
  let c1' = c1 |> (MInit.OM.add (Int64.of_int 1)) in
  (* Syncs with the master again. Publishes 11. *)
  Vpst.sync_next_version ~v:c1' >>= fun c2 ->
  (*let _ = Printf.printf "thread1: %d\n" c2 in*)
  let _ = Printf.printf "merged : %s\n" (U.string_of_list IntAtom.to_string (MInit.OM.elements c2)) in 
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.sync_next_version ~v:c2 >>= fun c3 ->
  Printf.printf "merged = apply q' on v1:\n";
  MInit.OM.iter (fun k a -> Printf.printf "%s : %s\n" k (IntAtom.to_string a) ) (MInit.OM.elements c3) 
  Vpst.return ()   in 

  let main () =
   let original = MInit.OM.empty |> MInit.OM.add "C" (Int64.of_int 10) |> MInit.OM.add "A" (Int64.of_int 5) |> MInit.OM.add "D" (Int64.of_int 20) in
   Vpst.with_init_version_do original thread1_f in 
   main();;