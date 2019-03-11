  let (yt : (int,bool) Hashtbl.t) = Hashtbl.create 117

  let (zt : (int,bool) Hashtbl.t) = Hashtbl.create 117

  let contains t a = 
    (match Hashtbl.find_opt t a with
      | Some true -> true
      | Some false -> false
      | None -> false)

  let populate_table t l = 
    List.iter (fun a -> Hashtbl.add t a true) l

  (*
   * IMPORTANT: merge assumes no duplicates
   *)

  let rec merge_vertical xs ys yt = 
    match xs,ys with
      | x::xs', y::ys' 
          when x=y -> x::(merge_vertical xs' ys' yt)
      | x::xs', y::ys' -> if contains yt x 
                          then y::(merge_vertical xs ys' yt)
                          else merge_vertical xs' ys yt
      | [],_ -> ys
      | _,[] -> []

  let rec merge_horizontal ys zs =
    match ys,zs with
      | y::ys', z::zs' -> 
          if y<z then y::z::(merge_horizontal ys' zs') 
          else z::y::(merge_horizontal ys' zs')
      | [], _ -> zs
      | _, [] -> ys

  let rec merge xs ys zs =
    match xs,ys,zs with
      | x::xs', y::ys', z::zs' 
          when x=y && y=z -> x::(merge xs' ys' zs')
      | x::xs', y::ys', z::zs' 
          when x=y -> if contains zt x 
                      then z::(merge xs ys zs')
                      else merge xs' ys' zs
      | x::xs', y::ys', z::zs' 
          when x=z -> if contains yt x 
                      then y::(merge xs ys' zs)
                      else merge xs' ys zs'
      | x::xs', y::ys', z:: zs' -> 
          if contains yt x && contains zt x 
          then if y<z then y::z::(merge xs ys' zs') 
               else z::y::(merge xs ys' zs')
          else merge xs' ys zs
      | [], _::_,_::_ | [], _::_, [] | [], [], _::_ -> merge_horizontal ys zs
      | _::_, _::_, [] -> merge_vertical xs ys yt
      | _::_,[],_::_ -> merge_vertical xs zs zt
      | _,[],[] -> []

  let merge xs ys zs = 
    begin 
      populate_table yt ys;
      populate_table zt zs;
      merge xs ys zs
    end


