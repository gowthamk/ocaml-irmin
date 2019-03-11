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

  let fold f n b = 
    let rec fold_aux f i b = 
      if i >= n then b 
      else fold_aux f (i+1) @@ f i b in
    fold_aux f 0 b
end 

(* Canvas *)
let _ =
  U.print_header "Red-Black Set"

module MkConfig (Vars: sig val root: string end) : Irbset.Config = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end

module Atom = struct
  type t = int32
  let t = Irmin.Type.int32
  let compare x y = Int32.to_int @@ Int32.sub x y
  let to_string = Int32.to_string
  let of_string = Int32.of_string
end

module CInit = MkConfig(struct let root = "/tmp/repos/rbset.git" end)
module IRBSet =Irbset.MakeVersioned(CInit)(Atom)
module R = Rbset.Make(Atom)
module RBSet = R
module Vpst = IRBSet.Vpst

let uris = ["git+ssh://opam@172.18.0.3/tmp/repos/rbset.git";
            "git+ssh://opam@172.18.0.4/tmp/repos/rbset.git"]

let seed = 564294298

let _ = Random.init seed

let (>>=) = Vpst.bind

let loop_until_y msg = Vpst.liftLwt @@ U.loop_until_y msg

let do_an_insert t = 
  RBSet.add (Random.int32 900000l) t

let do_a_remove t = 
  if RBSet.is_empty t then t
  else RBSet.remove (RBSet.choose t) t

let do_an_oper t = 
  match Random.int 3 with
    | 0 | 1 -> do_an_insert t
    | _ -> do_a_remove t

let loop_iter i (pre: RBSet.t Vpst.t) : RBSet.t Vpst.t = 
  pre >>= fun t ->
  let t' = if i<2 then 
              U.fold (fun _ t -> do_an_insert t) 10 t
           else 
              U.fold (fun _ t -> do_an_oper t) 10 t in
  Vpst.sync_next_version ~v:t' uris

let main_loop : RBSet.t Vpst.t = 
  U.fold loop_iter 1000 (Vpst.get_latest_version ())

let alice_f : unit Vpst.t = 
  loop_until_y "Ready?" >>= fun () ->
  Vpst.get_latest_version () >>= fun _ ->
  let _ = printf "Latest version gotten\n" in
  main_loop >>= fun v -> 
  let elts = RBSet.elements v in
  let _ = Printf.printf "Alice: \n" in
  begin
    List.iter (fun x -> 
        printf "%ld; " x) elts;
    Vpst.return ()
  end

let main () =
  let _ = CInit.init () in
  let (f: RBSet.t -> unit Vpst.t -> unit) = Vpst.with_init_version_do in
   (*
    * Alice starts with an empty set
    *)
  f RBSet.empty alice_f;;

main ();;
