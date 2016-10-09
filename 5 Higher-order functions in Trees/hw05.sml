use "lib.sml";

(* ---------------------------------------------------------------------- *)
(* map *)

fun pluralize_rec (t : string tree) : string tree =
    case t of
        Empty => Empty
      | Leaf x => Leaf (x ^ "s")
      | Node(l,r) => Node(pluralize_rec l , pluralize_rec r)

fun mult_rec (c : int, t : int tree) : int tree =
    case t of
        Empty => Empty
      | Leaf x => Leaf (c * x)
      | Node(l,r) => Node(mult_rec (c,l) , mult_rec (c,r))

(* TASK *)
(* spec: compute a new tree by applying some function f to each element of the given tree *)
fun map (f : 'a -> 'b, t : 'a tree) : 'b tree =
    case t of
        Empty => Empty
      | Leaf x => Leaf (f x)
      | Node(l,r) => Node(map(f,l), map(f,r))

(* TASK *)
(* spec: pluralize t evaluates to a tree t’, where
  t’ has the same structure as t, and the string at each leaf of t’
  is the string at the corresponding position in t, with an ’s’
  affixed to the end *)
fun pluralize (t : string tree) : string tree =
  map (fn x => x ^ "s", t)

val (Node(Node(Leaf "as", Leaf "bs"), Node(Leaf "cs", Leaf "ds"))) =
  pluralize(Node(Node(Leaf "a", Leaf "b"), Node(Leaf "c", Leaf "d")))

(* spec: mult t evaluates to a tree t’, where
   t’ has the same structure as t, and the int at each leaf of t’
   is the int at the corresponding position in t, multiplied by c *)
fun mult (c : int, t : int tree) : int tree =
  map (fn x => c * x, t)

val (Node(Node(Leaf 2, Leaf 4), Node(Leaf 6, Leaf 8))) =
  mult(2,Node(Node(Leaf 1, Leaf 2), Node(Leaf 3, Leaf 4)))



(* ---------------------------------------------------------------------- *)
(* reduce *)

fun sum_rec (t : int tree) : int =
    case t of
        Empty => 0
      | Leaf x => x
      | Node(t1,t2) => (sum_rec t1) + (sum_rec t2)

fun join_rec (t : string tree) : string =
    case t of
        Empty => ""
      | Leaf x => x
      | Node(t1,t2) => (join_rec t1) ^ (join_rec t2)

(* TASK *)
(* spec: takes a binary function of type ’a * ’a -> ’a to apply at each node, and a value of type ’a for the empty tree, and computes an ’a from an ’a tree *)
fun reduce (n : 'a * 'a -> 'a, b : 'a, t : 'a tree) : 'a =
    case t of
        Empty => b
      | Leaf x => x
      | Node(t1,t2) => n((reduce(n,b,t1)),(reduce(n,b,t2)))

(* TASK *)     
(* spec: sum t evaluates to a number n, where n is
   the sum of all of the numbers at the leaves of t *)   
fun sum (t : int tree) : int =
    reduce (fn (x,y) => x+y, 0, t)


val 10 = sum(Node(Node(Leaf 1, Leaf 2), Node(Leaf 3, Leaf 4)))

(* spec: join t evaluates to a string s, where s is
   the concatenation of all of the strings at the leaves of t,
   in order from left to right *)
fun join (t : string tree) : string =
    reduce (fn (x,y) => x^y, "", t)

val "abcd" = join(Node(Node(Leaf "a", Leaf "b"), Node(Leaf "c", Leaf "d")))

(* ---------------------------------------------------------------------- *)
(* programming with map and reduce *)

(* TASK *)
(* spec: flatten t contains all of the elements of all of the trees in t. The elements of each tree t1 in t should occur in flatten t in the same order in which they occur in t1; if a tree t1 is to the left of a tree t2 in t, the elements of t1 should occur to the left of the elements of t2 in flatten t. *)
fun flatten (t : ('a tree) tree) : 'a tree =
  reduce (fn (x,y) => Node(x,y), Empty, t)

val Node (Node (Leaf 1,Leaf 2),Node (Leaf 3,Empty)) =
  flatten (Node (Leaf (Node (Leaf 1, Leaf 2)), Node (Leaf (Leaf 3), Empty)))

(* TASK *)
(* spec: filter (p, t) contains all and only the elements x : ’a of t for which p x returns true. The elements that are kept should be in the same order as in the original tree. *)
fun filter (p : 'a -> bool, t : 'a tree) : 'a tree =
  flatten(map ((fn x => case (p x) of true => Leaf(x) | false => Empty), t))

val Node (Node (Empty,Empty),Node (Leaf 3,Empty)) =
   filter (fn x => x > 2, Node (Node (Leaf 1,Leaf 2),Node (Leaf 3,Empty)))

(* TASK *)
(* spec: allpairs(t1, t2) contains the pair (x,y) for every element x of t1 and y of t2. The order of the pairs is unspecified. *)
fun allpairs (tree1 : 'a tree, tree2 : 'b tree) : ('a * 'b) tree =
  flatten(map(fn x => map(fn y => (x, y), tree2), tree1))

val Node (Node (Leaf (1,"a"),Leaf (1,"b")),Node (Leaf (2,"a"),Leaf (2,"b"))) =
   allpairs (Node(Leaf 1, Leaf 2), Node(Leaf "a", Leaf "b"));

type answers = int * int * int * int

fun same(x : int, y : int) : real = 
    case x = y of
        true => 1.0
      | false => 0.0

fun count_same ((a1,a2,a3,a4) : answers , (a1',a2',a3',a4') : answers) : real = 
    same (a1,a1') + same (a2,a2') + same (a3,a3') + same (a4,a4')

(* TASK *)
(* if a students answers 5 than he or she is compatible with all the other students *)
fun my_scoring ((a1,a2,a3,a4) : answers , (a1',a2',a3',a4') : answers) : real =
    let val y = (case a1 = 5 of
        true => 1.0
      | _ => (case a1' = 5 of
              true => 1.0
            | _ => same (a1,a1')))
    in y + same (a2,a2') + same (a3,a3') + same (a4,a4') end

val EQUAL = Real.compare(3.0, my_scoring((1,2,3,4),(1,3,3,4)))
val EQUAL = Real.compare(4.0, my_scoring((1,2,3,4),(5,2,3,4)))

(* TASK *)
(* spec: similarity is a scoring function, cutoff is a real number, people is the input data for all of the users. matches (similarity, cutoff, people) should compute a tree of pairs (person1,person2,score) where
• each score is the similarity score of person1 and person2
• the tree is sorted from highest scores to lowest scores
• only pairs of people whose score is bigger than cutoff are included
• the tree never contains a pair of people of the form (person1,person1, ) or both the pair (person1,person2, ) and the pair (person2,person1, ). *)
fun matches (similarity : answers * answers -> real,
             cutoff : real,
             people : (string * answers) tree)
            : (string * string * real) tree =
    
    sort (fn (x,y) => let val (a:string,b:string,c:real) = x in let val (d:string,e:string,f:real) = y in Real.compare(f,c) end end, filter (fn z => let val (a,b,c) = z in c>cutoff andalso a<b end,
    map(fn y => let val ((a:string,b:answers),(c:string,d:answers)) = y in (a,c,similarity(b,d)) end, allpairs(people,people))))


(* code for testing *)

val test_data : (string * answers) tree = fromlist [ ("A",(1,1,1,1)), ("B",(2,2,2,2)), ("C",(1,2,2,2)) ]

fun show_matches (similarity : answers * answers -> real, cutoff : real, people : (string * answers) tree) : unit =
    List.app (fn (n1,n2,score) => print (n1 ^ " and " ^ n2 
                                         ^ " have compatibility " ^ (Real.toString score ^ "\n"))) 
             (tolist (matches (similarity, cutoff, people)))

val true = 
let val Node (Leaf ("B","C",x),Leaf ("A","C",y)) = matches (count_same, 0.0, test_data)
in Real.==(x,3.0) andalso Real.==(y,1.0)
end