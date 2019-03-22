

(* Utility functions *)
(* U is a module with two functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end 

let _ =
  U.print_header "Heap-LeftList";
let module MkConfig (Vars: sig val root: string end) : Iheap_leftlist.Config = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end in 

let module IntAtom = struct
  type t = int32
  let compare = Pervasives.compare
  let t = Irmin.Type.int32
  let to_string = Int32.to_string
  let of_string = Int32.of_string
end in 

let module CInit = MkConfig(struct let root = "/tmp/repos/init.git" end) in 
let module HInit = Iheap_leftlist.MakeVersioned(CInit)(IntAtom) in 
let module H = Heap_leftlist.Make(IntAtom) in 
let module Vpst = MInit.Vpst in 

let (>>=) = Vpst.bind  in

let work_thread2 : H.t Vpst.t = 
  let c0 = H.empty;
  let c0' = C0 |> (M.insert (Int32.of_int 2)) |> (M.insert (Int32.of_int 10)) |> (M.insert (Int32.of_int 9)) in
  (Vpst.get_latest_version ()) >>= fun v ->
  Vpst.return v

let work_thread1 : H.t Vpst.t = 
  let c0 = H.empty;
  let c0' = C0 |> (M.insert (Int32.of_int 3)) |> (M.insert (Int32.of_int 6)) in
  (Vpst.get_latest_version ()) >>= fun v ->
  Vpst.return v

let rec wait_till_done () : unit Vpst.t = 
  if !n_done = 3 then Vpst.return ()
  else begin 
    Vpst.liftLwt @@ Lwt_unix.sleep 1.0 >>= fun _ ->
    wait_till_done ()
  end

let experiment_f : unit =
  begin
    CInit.init ();
    Vpst.with_init_version_do H.empty
      begin 
        Vpst.fork_version (work_thread1 ()) >>= fun br1 ->
        Vpst.fork_version ~parent:br1 (work_thread2 ()) >>= fun br2 ->
        Vpst.set_parent br2 >>= fun () ->
        (work_loop ()) >>= fun _ ->
        wait_till_done ()
      end;

let thread2_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  let c0' = c0 |> (M.insert (Int32.of_int 2)) |> (M.insert (Int32.of_int 10)) in
  Vpst.sync_next_version ~v:c0' >>= fun c1 ->
  Vpst.liftLwt @@ Lwt_unix.sleep 0.5 >>= fun () ->
  let c1' = c1 |> (MInit.OM.insert (Int32.of_int 9))  in 
  Vpst.sync_next_version ~v:c1' >>= fun c2 ->
  let _ = Printf.printf "merged : %s\n" (U.string_of_list IntAtom.to_string (MInit.OM.elements c2)) in 
  Vpst.return () in 

 let thread1_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  Vpst.fork_version thread2_f >>= fun () ->
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  let c0' = c0 |> (MInit.OM.insert (Int64.of_int 4)) |> (MInit.OM.insert (Int64.of_int 3)) |> (MInit.OM.insert (Int64.of_int 2))  in
  Vpst.sync_next_version ~v:c0' >>= fun c1 ->
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  let c1' = c1 |> (MInit.OM.insert (Int64.of_int 1)) in
  Vpst.sync_next_version ~v:c1' >>= fun c2 ->
  let _ = Printf.printf "merged : %s\n" (U.string_of_list IntAtom.to_string (MInit.OM.elements c2)) in 
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.sync_next_version ~v:c2 >>= fun c3->
  let _ = Printf.printf "merged : %s\n" (U.string_of_list IntAtom.to_string (MInit.OM.elements c3)) in 
  Vpst.return ()   in 
  ();;
