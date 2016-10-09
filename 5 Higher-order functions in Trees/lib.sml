(* ---------------------------------------------------------------------- *)
(* support code *)

(* This module provides:
   (1) rebalancing, from the homework 4 solution,
   (2) mergesort, from lecture,
   (3) combine, more or less from the homework 4 solution, but 
       with a tighter spec on the depth.
       combine will be used to convert LEN trees to LN trees.  
   (4) some insert functions which will be helpful for converting LN trees to LEN trees.  
*)
structure InternalTree =
struct

  (* invariant: the int at each node is the size of that node *)
  datatype 'a internal_tree = IEmpty | INode of 'a internal_tree * ('a * int) * 'a internal_tree
  
  fun size (t : 'a internal_tree) : int = 
      case t of 
          IEmpty => 0
        | INode(_,(_,s),_) => s

  (* an internal tree is balanced iff
     it is Empty,
     it is Node(l,x,r) where l is balanced and r is balanced and size(l) = size(r) + {-1,0,1}
  *)
  fun balanced (t : 'a internal_tree) : bool = 
      case t of 
          IEmpty => true
        | INode(l,(x,_),r) => 
              balanced l andalso
              balanced r andalso 
              ((size l = size r) orelse 
               (size l = size r + 1) orelse
               (size l = size r - 1))

  (* smart constructor that computes the size *)
  fun inode (l : 'a internal_tree, x : 'a, r : 'a internal_tree) : 'a internal_tree =
      INode (l , (x, size l + size r + 1) , r)
  
  (*
  * Given a tree t and an int i, separates t into "left" and "right"
  * subtrees such that the left subtree contains i elements of t, in
  * order, and the right subtree contain the remaining elements of t,
  * in their original order.
  * Assumes i <= size t.  
  *
  * Assuming d is the depth of t, has O(d) work and span.
  *)
  fun takeanddrop (t : 'a internal_tree, i : int) : 'a internal_tree * 'a internal_tree =
      case (i,t) of
        (0,_) => (IEmpty, t)
      | (_,IEmpty) => raise Fail "not enough elts"
      | (_, INode (l , (x, _) , r)) =>
        (case i <= size l of
           true =>
           let
               val (l1 , l2) = takeanddrop (l , i)
           in
               (l1 , inode (l2 , x , r))
           end
         | false =>
           let
             val (r1 , r2) = takeanddrop (r , i - (size l) - 1)
           in
               (inode (l , x , r1), r2)
           end)
  
  (* For all t, halves t == (l,x,r) where size(l) = size(r) + {-1,0,1} 
     and an in-order traversal of t is the same as an in-order traversal
     of Node(l,x,r).
     
     Assuming d is the depth of t, has O(d) work and span.  
  *)
  fun halves (t : 'a internal_tree) : 'a internal_tree * 'a * 'a internal_tree =
      let
          val (l , vr) = takeanddrop (t , (size t) div 2)
          val (INode (IEmpty, (v,_) , IEmpty) , r) = takeanddrop (vr , 1)
      in
          (l , v , r)
      end
  
  (* compute a balanced tree with the same elements as the original tree.
     Assuming t has size n and the depth of t is k*(log n) for some k, 
     has O(n) work and O((log n)^2) span.  
  *)
  fun rebalance (t : 'a internal_tree) : 'a internal_tree =
      case t
       of IEmpty => IEmpty
        | _ =>
          let
            val (l , x , r) = halves t
          in
              inode (rebalance l , x , rebalance r)
          end
  
  (* if t is sorted, then split(t) == (l,r) such that
     everything in l is LESS than bound (according to compare)
     and everything in r is GREATER than or EQUAL to bound (according to compare).
  
     If comapre takes constant time, and d is the depth of t, then
        O(d) work
        O(d) span
  *)
  fun splitAt (compare : 'a * 'a -> order) (t : 'a internal_tree , bound : 'a) : 'a internal_tree * 'a internal_tree =
   case t of
       IEmpty => (IEmpty , IEmpty)
     | INode (l , (x,_) , r) => 
        (case compare (bound, x) of
             LESS => let val (ll , lr) = splitAt compare (l , bound) 
                     in (ll , inode (lr , x , r))
                     end
           | _ => let val (rl , rr) = splitAt compare (r , bound) 
                      in (inode (l , x , rl) , rr)
                      end)
  
  (* if t1 is sorted and t2 is sorted, then merge(t1,t2) is sorted, and 
     the depth(merge(t1,t2)) <= depth t1 + depth t2.
     Assuming compare takes constant time, 
              n1 is the size of t1,
              n2 is the size of t2,
              and t1 and t2 are each balanced,
       O(n1 + n2) work
       O( (log n1) * (log n2)) span
  *)
  fun merge (compare : 'a * 'a -> order) (t1 : 'a internal_tree , t2 : 'a internal_tree) : 'a internal_tree = 
   case t1 of 
       IEmpty => t2
     | INode (l1 , (x,_) , r1) =>
        let val (l2 , r2) = splitAt compare (t2 , x) 
        in
            inode (merge compare (l1 , l2) , 
                   x, 
                   merge compare (r1 , r2))
        end
  
  (* sort t according to compare.
     Assuming compare takes constant time, and t is balanced
       O(n log n) work
       O( (log n) ^3 ) span
     mergesort t balanced.  
  *)
  fun mergesort' (compare : 'a * 'a -> order) (t : 'a internal_tree) : 'a internal_tree =
      case t of
          IEmpty => IEmpty
        | INode (l , (x,_) , r) => 
            rebalance (merge compare (merge compare (mergesort' compare l , mergesort' compare r), 
                                      inode(IEmpty,x,IEmpty)))

  (* sort t according to compare.
     Assuming compare takes constant time, 
       O(n log n) work
       O( (log n) ^3 ) span
     mergesort t balanced, but t does not need to be balanced to start with.  
  *)
  fun mergesort (compare : 'a * 'a -> order) (t : 'a internal_tree) : 'a internal_tree =
      mergesort' compare (rebalance t)
      

  (* If size(t) >= 1, then extract_left(t) == (t',a) 
     where t' is t without a and a is the leftmost thing in t

     depth(extract_left(t)) <= depth(t)

     If d is the depth of t, then O(d) work and span.  
     *)
  fun extract_left (t : 'a internal_tree) : 'a internal_tree * 'a =
      case t of
          IEmpty => raise Fail "size should have been >= 1"
        | INode(l,(x,_),r) => 
              (case l of 
                   IEmpty => (l,x)
                 | _ => let val (l',a) = extract_left l
                        in
                            (inode(l',x,r),a)
                        end)

  (* If size(t) >= 1, then extract_right(t) == (t',a) 
     where t' is t without a and a >= everything in t'.

     depth(extract_right(t)) <= depth(t)

     If d is the depth of t, then O(d) work and span.  
     *)
  fun extract_right (t : 'a internal_tree) : 'a internal_tree * 'a =
      case t of
          IEmpty => raise Fail "size should have been >= 1"
        | INode(l,(x,_),r) => 
              (case r of 
                   IEmpty => (l,x)
                 | _ => let val (r',a) = extract_right r 
                        in
                            (inode(l,x,r'),a)
                        end)

  (* If d is the depth of l, O(d) work and span.

     An in-order traversal of l then r is the same as an in-order traversal of combine(l,r).  
     
     If l is balanced and r is balanced and size(l) = size(r) + {-1,0,1}
     then the resulting tree is balanced.
     *)
  fun combine (l : 'a internal_tree, r : 'a internal_tree) : 'a internal_tree = 
      case l of 
          IEmpty => r
        | _ => (case Int.compare(size l , size r) of
                    LESS => let val (r',x) = extract_left r in inode(l,x,r') end
                  | EQUAL => let val (r',x) = extract_left r in inode(l,x,r') end
                  | GREATER => let val (l',x) = extract_right l in inode(l',x,r) end)


  (* insert_left(t,x) == a tree whose in-order traversal is 
     x followed by the in-order traversal of t.
     If d is the depth of t, then O(d) work and span.  
     *)
  fun insert_left (t : 'a internal_tree, x : 'a) : 'a internal_tree = 
      case t of 
          IEmpty => inode(IEmpty,x,IEmpty)
        | INode(l,(y,_),r) => inode(insert_left(l,x),y,r)

  (* insert_right(t,x) == a tree whose in-order traversal is 
     the in-order traversal of t followed by x.
     If d is the depth of t, then O(d) work and span.  
     *)
  fun insert_right (t : 'a internal_tree, x : 'a) : 'a internal_tree = 
      case t of 
          IEmpty => inode(IEmpty,x,IEmpty)
        | INode(l,(y,_),r) => inode(l,y,insert_right (r,x))

end 


structure Tree : sig
                     datatype 'a tree = Empty | Leaf of 'a | Node of 'a tree * 'a tree
                     val fromlist : 'a list -> 'a tree
                     val tolist : 'a tree -> 'a list
                     val sort : (('a * 'a -> order) * 'a tree) -> 'a tree
                 end
=
struct
    
    datatype 'a tree = Empty | Leaf of 'a | Node of 'a tree * 'a tree

    fun node(l : 'a tree, r : 'a tree) : 'a tree =
        case (l,r) of 
            (Empty,r) => r
          | (l,Empty) => l
          | (l,r) => Node(l,r)
        
    fun fromlist (l : 'a list) : 'a tree =
        case l of
            [] => Empty
          | [x] => Leaf x
          | _ => let
                     val len = List.length l
                 in
                     Node (fromlist (List.take (l, len div 2)),
                           fromlist (List.drop (l, len div 2)))
                 end

    fun tolist (t : 'a tree) : 'a list = 
        case t of 
            Empty => []
          | Leaf x => [x]
          | Node(l,r) => tolist l @ tolist r

  (* a tree is balanced iff
     it is Empty, or Leaf x, or 
     it is Node(l,r) where l is balanced and r is balanced and size(l) = size(r) + {-1,0,1}
  *)

  (* convert a LEN tree to a LN tree with the same elements.  
     If t is balanced, then internalize t is balanced and  O(n) work and O( (log n)^2) span.  
     *)
  fun internalize (t : 'a tree) : 'a InternalTree.internal_tree = 
      case t of
          Empty => InternalTree.IEmpty
        | Leaf x => InternalTree.inode(InternalTree.IEmpty, x , InternalTree.IEmpty)
        | Node(l,r) => InternalTree.combine (internalize l, internalize r)
  
  (* convert a LN tree to a LEN tree with the same elements.
     If t is balanced, then leafify t is balanced, and O(n) work and O((log n)^2) span.  
     *)
  fun leafify (t : 'a InternalTree.internal_tree) : 'a tree = 
      case t of 
          InternalTree.IEmpty => Empty
        | InternalTree.INode(InternalTree.IEmpty, (x,_),r) => node(Leaf x, leafify r)
        | InternalTree.INode(l, (x,_), InternalTree.IEmpty) => node(leafify l, Leaf x)
        | InternalTree.INode(l, (x,_), r) => 
              (case Int.compare (InternalTree.size l , InternalTree.size r) of
                   LESS => node (leafify (InternalTree.insert_right (l,x)), leafify r) 
                 | EQUAL => node (leafify (InternalTree.insert_right (l,x)), leafify r) 
                 | GREATER => node (leafify l , leafify (InternalTree.insert_left (r,x))))
  
  (* sort t according to compare 
     If t is balanced, and compare takes constant time, 
     then O(n log n) work and O((log n)^3 span).  
     *)
  fun sort (compare : 'a * 'a -> order, t : 'a tree) : 'a tree =
      leafify (InternalTree.mergesort compare (internalize t))



  (* randomized testing of sort *)

  fun size (t : 'a tree) =
      case t of 
          Empty => 0
        | Leaf _ => 1
        | Node (l,r) => size l + size r

  fun balanced (t : 'a tree) : bool = 
      case t of 
          Empty => true
        | Leaf x => true
        | Node (l,r) => balanced l andalso balanced r andalso 
                        (size l = size r orelse
                         size l = size r + 1 orelse
                         size r = size l + 1)

  fun list_sorted (compare : 'a * 'a -> order) (l : 'a list) : bool = 
      case l of 
          [] => true
        | x :: xs => list_sorted compare xs andalso
                     (List.all (fn y => (case compare (x,y) of GREATER => false | _ => true))) xs

  (* test sorting on randomly generated trees of sizes from 0 to 100 *)
  fun test_sort () : unit =
      let val r = Random.rand(15,150)
          fun loop (n : int) : unit = 
              case n of 
                  0 => ()
                | _ => let val tosort = (fromlist (List.tabulate(n, fn _ => Random.randInt r)))
                           val sorted = sort (Int.compare, tosort)
                       in case (balanced tosort, balanced sorted, list_sorted Int.compare (tolist sorted)) of 
                           (false , _ , _ ) => raise Fail "tosort unbalanced"
                         | (_, false, _) => raise Fail "sorted unbalanced"
                         | (_,_,false) => raise Fail "sorted is not sorted"
                         | _ => loop (n - 1)
                       end
      in 
          loop 1000
      end

end

open Tree
