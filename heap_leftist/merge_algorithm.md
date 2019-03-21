# Merge Algorithm

## Edit distance: Calculates the edit distance between ancestor and the forked version 
(Here E stands for empty tree)

- Base case: E, E => 

      []

- E, T2 => 

      Insert (min T2) :: Edit-distance (E, T2-min)

- T1, E => 

      Delete (min T1) :: Edit-distance (T1-min, E) 

- T1, T2 => 

      (a1 = min T1 and a2 = min T2)
      
      case 0 : a1 = a2 => (Edit-distance (T1-a1) (T2-a2)) 
      
                  (In this case we just care about the rest of the tree except the head
                   Why? Because if a value smaller was inserted to the forked version it
                   would have moved to the head. But at the head we have the same value as ancestor node 
                   so no change needed at head.)
                   
      case 1: a1 < a2 => Delete a1 :: 
                         (Edit-distance (T1-a1) T2)  
                         
                 (In this case we know head of the ancestor tree is less than the head of the 
                  forked branch. That means the forked branch doesnot contain any value equal to the 
                  head of the ancestor node. If it would have contained then that would have moved 
                  to the top. Hence we need to delete the head of ancestor as it is not present 
                  in the forked branch at this point of time. Then calculate the edit distance for the
                  rest of the ancestor and the forked branch.)
                  
      case 2: a1 > a2 => Insert a2 ::
                         (Edit-distance T1 (T2-a2))   
                         
                 (In this case we know that the head of the ancestor is more than the head of the forked heap
                  hence, we should insert the head of the ancestor to the forked branch. Then calculate the edit 
                  distance for the ancestor and the rest of the forked branch.)
                  
Explanation: The main ideas while calculating the edit distance are:

(1) If we see the head of the forked branch is more than the ancestor head that means we need to delete the head of ancestor. Even though if it might be added again in the forked branch, it will always move up. So there is no chance that ancestor head which is less than forked head will be present in somewhere left or right of forked heap. Always the smaller one will be present at the head. That is the reason we add "Delete a1" at the beginning of the edit list.

(2) If we see the head of the forked head is less than the head of the ancestor head that means we need to insert that
    element. Because a smaller element than the head of the ancestor is added in the forked heap. So we insert it and then
    calculate the edit distance for ancestor and rest of the forked heap. So we add "Insert" in the beginning of the edit       list.
    
(3) My edit list traverses through the end of the heap which is creating merge expensive but it also guarantess correctness. I never failed any test case.
                                                           
## Operation Transform: 

We get (p,q) from the above algorithm and now we calulate (p',q'). 
P and q are list of edits where edits are of this form
    * Insert a
    * Delete a
(Here [] stands for empty list)         (Think in diagonal diamond structure)

- Base case: [], [] => 

       [], []       (nothing changed)

- p, [] => 
       
       p, []        (here p' is equal to p and q is empty)

- [], q => 

       [], q        (here q' is equal to q and p is empty)

- p0 :: ps, q0 :: qs => 

  case 0 : p0 = Insert x and q0 = Insert y
       
           case 01: x = y => Op-transform ps qs 
           
              (here both the forked branch inserted same element, we have to only take care                                                 of the tail of p and q in that case)

           case 02: x < y => 
                   (a,b) = first calculate Op-transform ps (q0 :: qs) 
                   Insert nx :: a, b
                   
              (here we just need to insert all the corresponding 
               changes in each of the forked branch. we will 
               ignore the common element through the case0. 
               Hence any new inserted element in both the heap will
               be present in the final list)

           case 03: x > y => 
                   (a,b) = first calculate Op-transform (p0 :: ps) (qs) 
                   a, Insert ny :: b
                   
              (here we just need to insert all the corresponding changes in 
               each of the forked branch. we will 
               ignore the common element through the case0. 
               Hence any new inserted element in both the heap will
               be present in the final list. This is similar to the last case. 
              
case 1 : p0 = Delete x and q0 = Delete y 

      case 10: x = y => Op-tranform ps qs 

       (here both the forked branch deleted same element, 
        hence we just need to focus on the rest of the edit lists)

      case 12: x < y => 
             (a,b) = first calculate Op-transform ps (q0 :: qs) 
             Delete x :: a, b
             
        (if x < y that means x is already deleted in q at this point of time 
         and may be in future it will be added. 
         Why? Because always the minimum element is deleted. 
         As y is minimum element in q at this point of time
         So we will delete x in the forked q branch as well. 
         And then compare the q with the rest of the p. 

      case 13: x > y => 
             (a,b) = first calculate Op-transform (p0 :: ps) (qs) 
             a, Delete ny :: b
        (same description as above in case 12.)
              
  case 2: p0 = Delete x and q0 = Insert y 
  
    case 20: x = y => (Delete y :: Delete y :: a, b)

       (Means same element is deleted in one branch and added in another branch, 
        Hence we need to delete the element in the other branch as element 
        deleted in one branch cannot be present in the merged value. So we add two deletes. 
        Why? Because this the case where an element is added twice. 
        As we allow insertion of the same element more than 
        once we need to delete the minimum element twice. 
        Example: Suppose ancestor is (3,2,4) where 3 is left and 4 is right. 
        Left forked branch delete 2(2 is the min): (3,4)
        Right forked branch insert 2: (3,2,(4,2,E))
        As we could see there are two 2's in the right forked branch. 
        Hence we need to do two deletion. 

    case 02: x < y => 
             (a,b) = first calculate Op-transform ps (q0 :: qs) 
             Delete x :: a, b
             
        (Left forked branch is deleting an element so in the right forked branch 
         we need to delete that element. And also that element is smaller than the 
         element being inserted in right fork, so it might be the case that
         element is present in right forked branch, hence we delete it.
         Hence we add Delete in p' and then calculate the rest.)

    case 03: x > y => 
             (a,b) = first calculate Op-transform (p0 :: ps) (qs) 
             a, Insert y :: b
             
        (Left forked branch is deleting a bigger number and right forked branch 
         is inserting a smaller number, hence we need to add the new inserted 
         element in the left branch and rest we calculate again using any of the rules.)
              
case 3: p0 = Insert x and q0 = Delete y

        case 30: x = y => (a, Delete x :: Delete x :: b)

             (Means same element is deleted in one branch and added in another branch, 
              Hence we need to delete the element in the other branch as element 
              deleted in one branch cannot be present in the merged value. So we add two deletes.
              Why? Because this the case where an element is added twice. 
              As we allow insertion of the same element more than once we need to 
              delete the minimum element twice. 
              Example: Suppose ancestor is (3,2,4) where 3 is left and 4 is right. 
              Right forked branch delete 2(2 is the min): (3,4)
              Left forked branch insert 2: (3,2,(4,2,E))
              As we could see there are two 2's in the left forked branch. Hence we need to do two deletion.

        case 31: x < y => 
             (a,b) = first calculate Op-transform ps (q0 :: qs) 
             Insert x :: a, b

              (Left forked branch is deleting an element so in the 
               right forked branch we need to delete that element. 
               And also that element is smalled than the element being 
               inserted in right fork, so it might be the case that element 
               is present in right forked branch, hence we delete it. Hence we add Delete in p' 
               and then calculate the rest.)

        case 03: x > y => 
                   (a,b) = first calculate Op-transform (p0 :: ps) (qs) 
                   a, Delete y :: b

              (Left forked branch is deleting a bigger number and 
               right forked branch is inserting a smaller number,
               hence we need to add the new inserted element in the left 
               branch and rest we calculate again using any of the rules.)
                      
      
