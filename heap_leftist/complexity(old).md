# Complexity of Old algorithm for calculating edit distance, operation transform and merge:
## Edit distance:
```ocaml
let op_diff xt yt =
      let rec heap_diff hx hy =
        match (hx, hy) with
        | (E, E) -> []
        | (E, _) ->
            let (m, hy) = pop_min hy in (Insert m) :: (heap_diff hx hy)
        | (_, E) ->
            let (m, hx) = pop_min hx in (Delete m) :: (heap_diff hx hy)
        | (_, _) ->
            let a1 = find_min hx in
            let a2 = find_min hy in
            let c = Atom.compare a1 a2 in
            if c = 0
            then
              let hy = delete_min hy in
              let hx = delete_min hx in heap_diff hx hy
            else
              if c < 0
              then
                (let hx = delete_min hx in (Delete a1) :: (heap_diff hx hy))
              else
                (let hy = delete_min hy in (Insert a2) :: (heap_diff hx hy)) in
      heap_diff xt yt
 ```
  
 The above algorithm calculates the edit distance between two heaps, these are the cases involved:

-  E, E (Both are empty heap) -> O(1) (As the edit distance will be empty)
-  E, _ (Ancestor is empty and other one which emerged from ancestor is not empty) -> O(n) where n will be the no: of nodes in _.
- _, E -> O(n)
- H1, H2 -> In worst case it will be O(n1+n2) where n1 will be the number of nodes in H1 and n2 will be number of nodes in H2. As in 
worst case we need to go through all the nodes when they all are distinct to find the edit distance.
   
   
    
## Operation transform:
```ocaml
let op_transform p q =
  let rec transform_aux xs ys =
    match (xs, ys) with
    | ([], []) -> ([], [])
    | ([], _) -> ([], ys)
    | (_, []) -> (xs, [])
    | (hx::rxs, hy::rys) ->
        let handle kx ky on_conflict =
          let c = Atom.compare kx ky in
          if c = 0
          then on_conflict ()
          else
            if c < 0
            then (let (a, b) = transform_aux rxs ys in ((hx :: a), b))
            else (let (a, b) = transform_aux xs rys in (a, (hy :: b))) in
        (match (hx, hy) with
         | (Insert x, Insert y)|(Delete x, Delete y) ->
             let on_conflict () = transform_aux rxs rys in
             handle x y on_conflict
         | (Insert x, Delete y) ->
             let on_conflict () =
               let (a, b) = transform_aux rxs rys in ((hx :: hx :: a), b) in
             handle x y on_conflict
         | (Delete x, Insert y) ->
             let on_conflict () =
               let (a, b) = transform_aux rxs rys in (a, (hy :: hy :: b)) in
             handle x y on_conflict) in
  transform_aux p q
```
 
 
Complexity for calculating (p', q') from (p,q):
- ([],[]) -> O(1) As both the lists p and q are empty
- ([], _) -> O(n) As one of the list is not empty so we need to traverse through the other list to get p'. Here n is the number of elements in q.
- (_, []) -> O(n) As one of the list is not empty so we need to traverse through the other list to get q'. Here n is the number of elements in p.
- (_, _)  -> O(n1+n2) In worst case we need to traverse through all the elements in both the list. 
      
## Merge operation
```ocaml
let merge3 ~ancestor  l r =
let p = op_diff ancestor l in
let q = op_diff ancestor r in
let (_, q') = op_transform p q in apply l q'
```
      
As merging use both the above algorithm to calculate the three way merge value so in worst case its algorihtm will be O(n1+n2).
      
### My idea for improvement:
As the old algorithm doesn't take into account the best design decision of leftlist heap so its complexity is not as good as O(log n).
The properties are:-

(1) The node is always smaller than its child node. No restriction on the child nodes. 

(2) Always the rank of left node should be bigger than the right node of the heap. 

- rank : rank is the length of the path between the node and the right most leaf.
- Because the rank is the major thing, we always insert in the right node and when right node's rank become bigger we just swap it with the 
  left node.
- So while calculatig the merged heap, edit distance or operation transform we should think of the algorithm which traverse through 
only the right branch and just do the swaping at last instead of going throught the whole heap that is left, node and right.

(3) So insertion complexity is O(log n) because we always insert in the right branch. And later take care of rank restriction by just swapping left and right.

(4) If we use the rank while calulating the edit distance

