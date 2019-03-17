let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"

let print_header h = 
  begin 
    Printf.printf "%s\n" h;
    flush_all();
  end

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

let bounded_random_int32 x y = 
  Int32.add (Random.int32 (Int32.sub y x)) x

let bounded_random_int x y =
  (Random.int (y-x)) + x
