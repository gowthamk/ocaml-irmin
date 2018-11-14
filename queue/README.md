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
 | Take 
 ```
* op_transform takes a pair of edit sequences, s1 and s2, that map a queue v to two different queue. v1 and v2, and trasnform s1 to s1' such that s1' has the same effect on v2 as s1 had on v.

## Various cases of mergeable queue
### case1:
![Case 1](https://github.com/priyas13/ocaml-irmin/blob/master/queue/case1.png)
### case2:
![Case 2](https://github.com/priyas13/ocaml-irmin/blob/master/queue/case2.png)
### case3:
![Case 3](https://github.com/priyas13/ocaml-irmin/blob/master/queue/case3.png)
### case4:
![Case 4](https://github.com/priyas13/ocaml-irmin/blob/master/queue/case4.png)
### case5:
![Case 5](https://github.com/priyas13/ocaml-irmin/blob/master/queue/case5.png)
