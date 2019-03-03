open Lwt.Infix
open Irmin_unix
open Printf

type t = Empty | Node of int32 

let t = 
  let open Irmin.Type in
  variant "t" (fun b e -> function
      | Node a  -> b a
      | Empty -> e)
  |~ case1 "Node" int32 (fun x -> Node x)
  |~ case0 "Empty" Empty
  |> sealv

let pp_json = Irmin.Type.pp_json ~minify:false t

let () =
  let p = Empty in
  let s = Fmt.strf "%a\n" pp_json p in
  let _ = printf "%s\n" s in
  let decoder = Jsonm.decoder (`String s) in
  let res = Irmin.Type.decode_json t decoder in
  match res with
    | Ok _ -> printf "Result Ok\n"
    | Error (`Msg s) -> printf "Result Error: %s\n" s

