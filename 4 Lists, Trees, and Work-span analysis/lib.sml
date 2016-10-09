(* datatypes *)
datatype tree = Empty | Node of tree * int * tree
datatype rel = LT | GEQ

(* functions on trees *)
fun depth (t : tree) : int =
    case t
     of Empty => 0
      | Node(l,_,r) => 1 + Int.max(depth l, depth r)

fun size (t : tree) : int =
    case t
     of Empty => 0
      | Node(l,_,r) => (size l) + (size r) + 1

fun tolist (t : tree) : int list =
    case t
     of Empty => []
      | Node(l,x,r) => (tolist l) @ [x] @ (tolist r)

fun isbalanced (t : tree) : bool =
    case t
     of Empty => true
      | Node(l,_,r) =>
        let
          val dl = depth l
          val dr = depth r
        in
          Int.abs(dl - dr) <= 1 andalso isbalanced l andalso isbalanced r
        end

fun inteq(x1:int,x2:int) : bool =
    case Int.compare(x1,x2) of EQUAL => true | _ => false

fun treeeq(t1:tree, t2:tree) : bool =
    case (t1,t2) of
        (Empty,Empty) => true
      | (Node(l1,x1,r1),Node(l2,x2,r2)) =>
            treeeq(l1,l2) andalso inteq(x1,x2) andalso treeeq(r1,r2)
      | _ => false

local
  (* true iff every y in t is less or equal to  x *)
  fun lteq_all(x,t) =
      case t
       of Empty => true
        | Node(l,y,r) => x <= y andalso lteq_all(x,l) andalso lteq_all (x,r)

  (* true iff every y in t is greater than x *)
  fun grt_all(x,t) =
      case t
       of Empty => true
        | Node(l,y,r) => x > y andalso grt_all(x,l) andalso grt_all (x,r)
in
  fun issorted (t : tree) : bool =
      case t
       of Empty => true
        | Node(l,x,r) => lteq_all(x,r) andalso
                         grt_all(x,l) andalso
                         issorted(l) andalso
                         issorted(r)
end

local 
    (*
    If l is non-empty, then there exist l1,x,l2 such that
    split l == (l1,x,l2) and
    l == l1 @ x::l2 and
    length(l1) and length(l2) differ by no more than 1
    *)
    fun split (l : int list) : (int list * int * int list) =
        case l of
            [] => raise Fail "split should never be called on an empty list"
          | _ => let
                     val midlen = (length l) div 2
                     val front = (List.take (l,midlen))
                         
                     (* because we round down, if the list is non-empty, this
                        * has at least one thing in it
                        *)
                     val x :: back = (List.drop (l,midlen))
                 in
                     (front, x, back)
                 end
in
    (* Purpose: transforms an int list into a balanced tree *)
    fun fromlist (l : int list) : tree =
        case l of
            [] => Empty
          | _ =>
                let val (l1, x,  l2) = split l
                in
                    Node (fromlist l1, x , fromlist l2)
                end
            
    val Empty = fromlist nil
    val Node (Empty , 3 , Empty) = fromlist [3]
    val Node(Node(Empty,5,Empty),8,Node(Empty,2,Empty)) = fromlist [5,8,2]
end



(* uses int.compare to compare ints and produce the correct rel *)
fun intrelcmp (a : int, b : int) : rel =
    case Int.compare(a,b)
     of LESS => LT
      | GREATER => GEQ
      | EQUAL => GEQ

