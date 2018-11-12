(* Utility functions *)
(* U is a module with two functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end 

(* Set - AVL Tree *)
let _ =
  U.print_header "Treedoc";
let module MkConfig (Vars: sig val root: string end) : Itreedoc.Config = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end in 

let module StringAtom = struct
  type t = string
  let compare = Pervasives.compare
  let t = Irmin.Type.string
  let to_string = String.copy
  let of_string = String.copy
end in 

let module CInit = MkConfig(struct let root = "/tmp/repos/init.git" end) in 
let module MInit = Itreedoc.MakeVersioned(CInit)(StringAtom) in 
let module M = Treedoc.Make(StringAtom) in 
let module Vpst = MInit.Vpst in 
let print_doc t = 
      begin
      M.in_order_iter (fun s -> Printf.printf "%s " s) t;
      Printf.printf "\n"
      end  in 

let (>>=) = Vpst.bind  in

let bob_f : unit Vpst.t = 
  let open Treedoc in
  Vpst.get_latest_version () >>= fun t0 -> 
  (*
   * Bob thinks that the fox that jumped is rather wheatish 
   * brown in color. Inserts "wheatish" before "brown".
   *)
  let t0' = M.insert t0 [M.L;M.L;M.R] "wheatish" in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Bob syncs with Alice. Gets nothing new. *)
  Vpst.sync_next_version ~v:t0' >>= fun t1 ->
  (*
   * Bob wants the describe this statement as a "thought".
   *)
  let t1' = M.insert t1 [M.L;M.L;M.L;M.L] "thought:" in
  let _ = Printf.printf "Bob (before syncing with Alice): \n" in
  let _ = print_doc t1' in
  (* Bob syncs with Alice again *)
  Vpst.liftLwt @@ Lwt_unix.sleep 1.0 >>= fun () ->
  Vpst.sync_next_version ~v:t1' >>= fun t2 ->
  let _ = Printf.printf "Bob (after syncing with Alice): \n" in
  let _ = print_doc t2 in
  Vpst.return () in 
  
let alice_f : unit Vpst.t = 
  let open Treedoc in 
  Vpst.get_latest_version () >>= fun t0 -> 
  (*
   * Alice invites Bob for collaboration.
   *)
  Vpst.fork_version bob_f >>= fun () ->
  (*
   * Alice replaces "fox" with "rooster".
   *)
  let t0' =  M.update t0 [M.L;M.R] "rooster" in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.4 >>= fun () ->
  (* Alice syncs with Bob. Gets Bob's both insertions. *)
  Vpst.sync_next_version ~v:t0' >>= fun t1 ->
  (*
   * While the fox was wheatish brown, the rooster isn't. 
   * So Alice removes "wheatish".
   *)
  let t1' = M.update t1 [M.L;M.L;M.R] "" in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Syncs with Bob again. Nothing new. *)
  Vpst.sync_next_version ~v:t1' >>= fun t2 ->
  let _ = Printf.printf "Alice: \n" in
  let _ = print_doc t2 in
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.return () in 

let main () = 
  (*
   * The initial document.
   *)
  let init_doc = M.of_list 
             ["a"; "quick"; "brown"; "fox"; "jumped";
              "over"; "a"; "lazy"; "dog"] in
   (*
    * Alice is the owner of the document, and starts the session.
    *)
   Vpst.with_init_version_do init_doc alice_f in 

main();;
