open Lwt.Infix
open Irmin_unix
open Printf

type t = {x: char}

let t = 
  let open Irmin.Type in
  record "point" (fun x -> {x})
  |+ field "x" char (fun p -> p.x)
  |> sealr

let pp_json = Irmin.Type.pp_json ~minify:false t

let () =
  let p = {x=Char.chr 1;} in
  let s = Fmt.strf "%a\n" pp_json p in
  let _ = printf "%s\n" s in
  let decoder = Jsonm.decoder (`String s) in
  let res = Irmin.Type.decode_json t decoder in
  match res with
    | Ok _ -> printf "Result Ok\n"
    | Error (`Msg s) -> printf "Result Error: %s\n" s
