# Queue 
As an abstract data type, queue support two operations: *_push_* and *_pop_*. These push and pop operations use *_add x q_* and 
*_take q_*. *_add x q_* adds x to the queue q and *_take q_* pops out the element from the queue in FIFO order. 

Queue of mergeable items are mergeable if they preserve the following properties:
* Elements present in both the merging queues will be present in the final queue. No element is popped out unless it is popped in at least one queue.
* An element popped in one queue will not reappear in the merged queue. 
* Ordering of elements in the queue is reatined because ordering is important in the queue because we know elements can be only popped out in FIFO order.
* Merges converge 
* Hence, merge queues are CRDTs.

The merge operation for queue is composed of two separate functions, edit_seq and op_transform. The functions are described below.
* edit_seq takes a pair of queue, v and v' and computes shortest sequence of queue operations that need to be applied on v to obtain v'.Such a sequence is called an edit sequence.
```
type edit =
 | Add of atom
 | Take of atom
 ```
* op_transform takes a pair of edit sequences, s1 and s2, that map a queue v to two different queue. v1 and v2, and trasnform s1 to s1' such that s1' has the same effect on v2 as s1 had on v.

## Algorithm 
There are two algorithms:
One which computes the edit distance between two queue which is basically the sequence of edits. (p and q)
One which computes the operation transform. (p' and q')

### Edit-distance algorithm:
* edit-dist (q1, q2) where we are calculating the distance to reach from q1 to q2
* [-] represents empty-queue and x ::: xs represents x added first to queue followed by the rest of the elements
* [] represents empty list and x :: xs represents x as head of the list and xs as tail of the list
```
edit-dist (q1, q2) =
Base case: ([-], [-]) -> ([-])
case 1: (q ::: qs, [-]) -> (Take q :: edit-dist qs)
case 2: ([-], q ::: qs) -> (Add q :: edit-dist qs) 
case 3: (q ::: qs, q' ::: qs') -> case 30: if (|q ::: qs| > |q' ::: qs'|) then
                                  if (q ::: qs == (q' ::: qs' upto |q' ::: qs'| - 1))
                                  then Take q :: edit-dist qs [-] :: Add q' :: edit_dist [-] qs'
                                  else Take q :: edit-dist qs (q' ::: qs')
                                  
                                  case 31: if (|q ::: qs| = |q' ::: qs'|) then
                                  if (q ::: qs == q' ::: qs') 
                                  then []
                                  else Take q :: edit-dist qs [-] :: Add q' :: edit-dist [-] qs'
                                  
                                  case 32: if (|q ::: qs| < |q' ::: qs'|) then 
                                  if (q ::: qs upto |q ::: qs| - 1) == (q' ::: qs')
                                  then edit-dist [-] take(q' ::: qs' upto |q:::qs|-1)
                                  else Take q :: edit-dist qs [-] :: Add q' :: edit-dist [-] qs'

```
base-case:

![e-e](https://github.com/priyas13/ocaml-irmin/blob/master/queue/e-e.png)

case1: 

![q-e](https://github.com/priyas13/ocaml-irmin/blob/master/queue/q-e.png)

case2: 

![e-q](https://github.com/priyas13/ocaml-irmin/blob/master/queue/q-e.png)

case3:
![eq-eq](https://github.com/priyas13/ocaml-irmin/blob/master/queue/eq-eq.png)

### Operation-transform algorithm:
* op-trans (p, q) where we are calculating (p',q')
```
op-trans (p,q) =
Base case: ([],[]) -> ([],[])
case 1: (p, []) -> (p, [])
case 2: ([], q) -> ([], q)
case 3: (Add nx :: ps, Add ny :: qs) -> 
                  if there are common elements then 
                  then Take ny :: Take-all qs ++ list of Add edits where order is maintained,
                       Take nx :: Take-all qs) ++ list of Add edits where order is maintained
                  else 
                  case 301: nx < ny -> (Take ny :: Take-all qs) ++ Add nx :: Add ny :: op-trans ps qs
                  case 302: nx = ny -> (Take ny :: Take-all qs) ++ Add nx :: op-trans ps qs
                  case 303: nx > ny -> (Take ny :: Take-all qs) ++ Add ny :: Add nx :: op-trans ps qs
case 4: (Add nx :: ps, Take ny :: qs) ->
                  case 40: if there are common elements then
                           then Take-all qs ++ list of Add edits where order is maintained,
                                Take nx :: Take-all ps ++ list of Add edits where order is maintained
                  case 41: if Add ny in qs 
                           then Add nx :: Add-all ps,
                                Take ny :: Take nx :: Take-all xs ++ 
                                list of Add edits for ys ++ 
                                list of Add edits for (Add nx :: ps - Add ny) 
                  case 42: if above two cases are not true 
                           then (Take-all qs ++ op-trans (Add ny :: Add nx :: ps) ys) - Add ny,
                                (Take ny :: Take nx :: Take-all xs ++ op-trans (Add ny :: Add nx :: ps) ys) - Add ny
case 5: (Take nx :: ps, Add ny :: qs) -> 
                  case 50: if there are common elements then
                           then Take nx :: Take ny :: Take-all qs ++ list of Add edits where order is maintained,
                                Take nx :: Take-all ps ++ list of Add edits where order is maintained
                  case 41: if Add nx in qs 
                           then Take nx :: Take ny :: Tale-all qs  ++ (Add nx :: Add-all ps,
                                Take ny :: Take nx :: Take-all xs ++ 
                                list of Add edits for ys ++ 
                                list of Add edits for (Add nx :: ps - Add ny) 
                  case 42: if above two cases are not true 
                           then (Take-all qs ++ op-trans (Add ny :: Add nx :: ps) ys) - Add ny,
                                (Take ny :: Take nx :: Take-all xs ++ op-trans (Add ny :: Add nx :: ps) ys) - Add ny
```
```  
Base case : If both p and q qre [] -> p' = [] and q' = []
case 1: if p is some list l an q is [] -> p' = l and q' = []
case 2: if p is [] and q is some list l -> p' = [] and q' = l
case 3: if p is (p::ps) and q is (q::qs) -> we look at the following cases:
case 30: p = (Add nx :: ps), q = (Add ny :: qs) -> 
         if there are elements in common in both p and q 
         then p' = Take out all the elements of q and then add all the Add edits of p and q maintaining the order,
              q' = Take out all the elements of p and then add all the Add edits of p and q maintaining the order
         else p' = Take out all the elements of q and then compare each element at each corresponding index in both p and q                    and add smaller one first followed by the other,
              q' = Take out all the elements of p and then compare each element at each corresponding index in both p and q                    and add smaller one first followed by the other
case 31:  p = (Add nx :: ps), q = (Take ny :: qs) -> 
         if there are elements in common in both p and q 
         then p' = Take out all the elements of qs and then add all the Add edits of p and qs maintaining the order,
              q' = Take out all the elements of Add ny :: p and then add all the Add edits of p and qs maintaining the order
         else if Add ny is present in qs
              then p' = Add nx :: Add-all ps,
                   q' = Take out all the elements of Add ny :: p and then add all ys and all add-all (p - Add ny)
         else p = Take-all ps and then compare each element at each corresponding index in both (Add ny :: Add nx :: ps) and                   qs using our diff-edit algorithm 
              q = Take ny :: Take nx :: Take-all ps and then compare each element at each corresponding index in both 
                  (Add ny :: Add nx :: ps) and qs using our diff-edit algorithm
case 32:  p = (Take nx :: ps), q = (Add ny :: qs) -> 
         if there are elements in common in both p and q 
         then p' = Take nx :: Take ny ::Take out all the elements of qs and then add all the Add edits of p and qs    maintaining the order,
              q' = Take out all the elements of Add ny :: p and then add all the Add edits of p and qs maintaining the order
         else if Add ny is present in qs
              then p' = Add nx :: Add-all ps,
                   q' = Take out all the elements of Add ny :: p and then add all ys and all add-all (p - Add ny)
         else p = Take-all ps and then compare each element at each corresponding index in both (Add ny :: Add nx :: ps) and                   qs using our diff-edit algorithm 
              q = Take ny :: Take nx :: Take-all ps and then compare each element at each corresponding index in both 
                  (Add ny :: Add nx :: ps) and qs using our diff-edit algorithm
         
```        


