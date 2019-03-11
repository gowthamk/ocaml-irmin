module S = Set.Make(struct 
                      type t = int
                      let compare a b = a -b
                    end)

let rec merge xs ys zs =
  match xs,ys,zs with
    | x::xs', y::ys', z:: zs' 
        when x=y && y=z -> x::(merge xs' ys' zs')
    | x::xs', y::ys', z:: zs' 
        when x=y -> z::(merge xs ys zs')
    | x::xs', y::ys', z:: zs' 
        when x=z -> y::(merge xs ys' zs)
    | x::xs', y::ys', z:: zs' 
        when y=z -> y::(merge xs ys' zs')
    | _, y::ys', z:: zs' -> if y<z then y::z::(merge xs ys' zs') 
                            else z::y::(merge xs ys' zs')
        
    | [],[],[] -> []

let retained_sublists xs ys zs = 
  let (s1,s2,s3) = (S.of_list xs, S.of_list ys, S.of_list zs) in
  let common_s = S.inter s3 (S.inter s1 s2) in
  let new_in_s1 = S.diff s2 s1 in
  let new_in_s2 = S.diff s3 s1 in
  let elts = S.union common_s (S.union new_in_s1 new_in_s2) in
  let xs' = List.filter (fun x -> S.mem x elts) xs in
  let ys' = List.filter (fun y -> S.mem y elts) ys in
  let zs' = List.filter (fun z -> S.mem z elts) zs in
  (xs',ys',zs')


let rec common_prefix_split xs ys zs = 
  match xs,ys,zs with
    | x::xs', y::ys', z::zs' when x=y && y=z -> 
      let (prefix, suffixes) = common_prefix_split xs' ys' zs' in 
      (x::prefix, suffixes)
    | _,_,_ -> ([],(xs,ys,zs))

let common_suffix_split xs ys zs = 
  let rev = List.rev in
  let (xiffus, (sx,sy,sz)) = common_prefix_split 
                              (rev xs) (rev ys) (rev zs) in
    ((rev sx, rev sy, rev sz), rev xiffus)

let merge xs ys zs = 
  let (cp, (xs',ys',zs')) = common_prefix_split xs ys zs in
  let ((xm,ym,zm), cs) = common_suffix_split xs' ys' zs' in
  let (xm',ym',zm') = retained_sublists xm ym zm in
  merge xm' ym' zm'

