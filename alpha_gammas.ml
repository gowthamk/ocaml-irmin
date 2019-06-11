module G = TreeGraph
module E = G.Edge
(*
 * List
 *)
module MList:(MERGEABLE1 with type 'a t = 'a list) = 
struct
  type 'a t = 'a list [@@deriving merge]

  let%rel r_mem l = Set.of_list l

  let%rel r_ob = function
    | [] -> Set.empty
    | x::xs -> 
      let s1 = Set.times (Set.singleton x) 
                         (Set.singleton `Cons)
                         (r_mem xs) in
      let s2 = r_ob xs in
      Set.union s1 s2

  let concretize (s_mem, s_ob) = 
    if Set.is_empty s_ob then Set.elements s_mem
    else
      let g = concretize_order s_ob ~arity:1 (<) in
      let rec mk_list x = match G.succ x with
        | [] -> [x]
        | [y] -> x::(mk_list y)
        | _ -> failwith "MList.concretize: impossible case!" in
      mk_list (G.top g)

end

module MRBSet:(MERGEABLE1 with type 'a t = 'a RBset.t) = 
struct
  (* Below is 'a RBSet.t reproduced *)
  type 'a t = 
    | Black of 'a t * 'a * 'a t
    | Red of 'a t * 'a * 'a t
    | Empty [@@deriving merge]

  let%rel r_mem t = Set.of_list @@ RBset.elements t

  let concretize s_mem = Set.fold RBSet.add s_mem Empty
end


module MRBMap(V:MERGEABLE):
  (MERGEABLE1 with type 'a t = ('a,V.t) RBset.t) = 
struct
  (* Below is 'a t, which is equivalent to ('a,V.t) RBSet.t *)
  type 'a t =
    | Black of 'a t * 'a * V.t * 'a t
    | Red of 'a t * 'a * V.t * 'a t
    | Empty [@@deriving merge]

  let%rel r_mem t = Set.of_list @@ RBMap.all_pairs t

  let concretize s_mem = 
    Set.fold (fun (k,v) t -> RBMap.add k v t) s_mem Empty
end

module MBinaryTree:(MERGEABLE1 with type 'a t = 'a BinaryTree.t) =
struct
  (* Below is 'a BinaryTree.t reproduced *)
  type 'a t =
    | N of 'a t * 'a * 'a t
    | E [@@deriving merge]

  let%rel r_mem t = Set.of_list @@ BinaryTree.elements t

  let%rel r_to = function
    | E -> Set.empty
    | N(l, x, r) ->
      let s1 = Set.times3 (Set.singleton x)
                (Set.singleton `Left) (r_mem l) in
      let s2 = Set.times3 (Set.singleton x)
                (Set.singleton `Right) (r_mem r) in
      List.fold_left Set.union (r_to l) [s1; s2; r_to r]

  let concretize (s_mem, s_to) = 
    if Set.is_empty s_to 
    then 
      match Set.elements s_mem with 
        | [] -> E
        | [x] -> N(E, x, E)
        | _ -> failwith "MBinaryTree.concretize: impossible case!"
    else
      let g = concretize_order s_to ~arity:1 (<) in
      let rec mk_tree x = match G.succ x with
        | [] -> N(E,x,E)
        | [y] -> (match E.label @@ G.find_edge g x y with 
                   | `Left -> N(mk_tree y, x, E)
                   | `Right -> N(E, x, mk_tree y))
        | [y;z] -> (match (E.label @@ G.find_edge x y, 
                           E.label @@ G.find_edge x z) with
                     | (`Left, `Right) -> N(mk_tree y, x, mk_tree z)
                     | (`Right, `Left) -> N(mk_tree z, x, mk_tree y))
        | _ -> failwith ".concretize: impossible case!" in
      mk_tree (G.top g)
end

module MDiGraph:(MERGEABLE1 with type 'a t = 'a DiGraph.t) = 
struct 
  type 'a t = 'a DiGraph.t [@@deriving merge]

  let%rel r_vertex t = DiGraph.fold_vertex 
      (fun x s -> Set.union s (Set.singleton x)) t Set.empty

  let%rel r_edge t = DiGraph.fold_edges 
      (fun x y s -> Set.union s (Set.singleton (x,y))) t Set.empty

  let concretize (s_vertex, s_edge) = 
    Set.fold (fun (x,y) g -> 
                DiGraph.add_edge g x y) s_edge @@
    Set.fold (fun x g -> DiGraph.add_vertex g x) s_vertex @@
    DiGraph.create ()
end

module MLeftistBinaryHeap:(MERGEABLE1 with type 'a t = 'a LeftistBinaryHeap.t) =
struct 
  type 'a t =
    | N of 'a t * 'a * 'a t
    | E [@@deriving merge]

  let%rel r_mem t = Set.of_list @@ BinaryTree.elements t

  let%rel r_ans = function
    | E -> Set.empty
    | N(l, x, r) ->
      let s1 = Set.times3 (Set.singleton x) (Set.singleton `Down) 
                          (Set.union (r_mem l) (r_mem r)) in
      Set.union (r_ans r) @@ Set.union (r_ans l) s1

  let concretize (s_mem, s_ans) = 
    if Set.is_empty s_ans then MBinaryTree.concretize (s_mem, s_ans)
    else
      let g = concretize_order s_ans ~arity:2 (<) in
      let rec mk_heap x = match G.succ x with
        | [] -> N(E,x,E)
        | [y] -> N(mk_heap y, x, E)
        | [y;z] -> N(mk_heap y, x, mk_heap z)
        | _ -> failwith "MLeftistBinaryHeap.concretize: \
                         \impossible case!" in
      mk_heap (G.top g)
end

module MPair(V1:MERGEABLE1)(V2:MERGEABLE1):
  MERGEABLE2 with type ('a,'b) t = ('a V1.t * 'b V2.t) =
struct
  type ('a,'b) t = 'a V1.t * 'b V2.t [@@deriving merge]

  let%rel r_pair (x,y) = Set.of_list @@ [(1,x); (2,y)]

  let concretize s_pair = match Set.elements s_pair with
    | [(1,x); (2,y)] | [(2,y); (1,x)] -> (x,y)
    | _ -> failwith "MPair.concretize: Impossible case!"
end

module MQueue:MERGEABLE1 with type 'a t = ('a,'a) MPair(MList)(MList).t = 
struct 
  type 'a t = ('a,'a) MPair(MList)(MList).t
end
