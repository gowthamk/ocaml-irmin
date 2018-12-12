# Queue 
As an abstract data type, queue support two operations: *_push_* and *_pop_*. These push and pop operations use *_add x q_* and 
*_take q_*. *_add x q_* adds x to the queue q and *_take q_* pops out the element from the queue in FIFO order. 

Queue of mergeable items are mergeable if they preserve the following properties:
* Elements present in both the merging queues will be present in the final queue. No element is popped out unless it is popped in at least one queue.
* An element popped in one queue will not reappear in the merged queue. 
* Ordering of elements in the queue is reatined because ordering is important in the queue because we know elements can be only popped out in FIFO order.

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
* [ ] represents empty list and x :: xs represents x as head of the list and xs as tail of the list
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
