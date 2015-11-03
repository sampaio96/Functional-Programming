signature LABDICT =
sig

  type ('k, 'v) dict

  (* the empty mapping *)
  val empty : ('k, 'v) dict

  (* insert cmp (k1 ~ v1, ..., kn ~ vn) (k,v) 
     == (k1 ~ v1, ..., ki ~v,...) if cmp(k,ki) ==> EQUAL for some ki
     == (k1 ~ v1, ..., kn ~ vn, k ~ v) otherwise
     *)
  val insert : ('k * 'k -> order) -> ('k, 'v) dict -> ('k * 'v) -> ('k, 'v) dict

  (* lookup cmp (k1 ~ v1,...,kn ~ vn) k 
     == SOME vi if cmp(k,ki) == EQUAL for some ki
     == NONE otherwise
     *)
  val lookup : ('k * 'k -> order) -> ('k, 'v) dict -> 'k -> 'v option

end




structure TreeDict : LABDICT=
struct
  (* Invariant: BST *)
  datatype ('k, 'v) tree =
      Empty
    | Node of ('k, 'v) tree * ('k * 'v) * ('k, 'v) tree

  type ('k, 'v) dict = ('k, 'v) tree

  val empty = Empty

  fun lookup cmp d k =
    case d of
      Empty => NONE
    | Node (L, (k', v'), R) =>
          case cmp (k,k') of
              EQUAL => SOME v'
            | LESS => lookup cmp L k
            | GREATER => lookup cmp R k

                  
  fun insert cmp d (k, v) =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
          EQUAL => Node (L, (k, v), R)
        | LESS => Node (insert cmp L (k, v), (k', v'), R)
        | GREATER => Node (L, (k', v'), insert cmp R (k, v))
end






















fun compareMod (i,j) = Int.compare (i mod 1024, j mod 1024) 
            
fun isins d p = TreeDict.insert Int.compare d p
val t1 = isins (isins (isins TreeDict.empty (1023,"c")) (111,"a")) (1025,"b")

val NONE = TreeDict.lookup compareMod t1 1 
(* wrong behavior if you search using a different comparison function than the one it is sorted by:
   violates the behavioral spec
   compareMod (1025,1025) ==> EQUAL and (1025 ~ "b") is in the dictionary
   *) 

signature ORDERED = 
sig
    type t
    val compare : t * t -> order
end

structure IntLt : ORDERED =
struct
   type t = int
   val compare = Int.compare
end

structure IntMod : ORDERED =
struct
   type t = int
   val compare = compareMod
end

structure StringLt : ORDERED =
struct
   type t = string
   val compare = String.compare
end

signature DICT =
sig
  structure Key : ORDERED
  type 'v dict

  val empty  : 'v dict
  val insert : 'v dict -> (Key.t * 'v) -> 'v dict
  val lookup : 'v dict -> Key.t -> 'v option
end



structure IntLtTreeDict : DICT =
struct

  structure Key : ORDERED = IntLt

  datatype 'v tree =
      Empty
    | Node of 'v tree * (int * 'v) * 'v tree

  type 'v dict = 'v tree

  val empty = Empty

  fun lookup d k =
    case d of
      Empty => NONE
    | Node (L, (k', v'), R) =>
          case Key.compare (k,k') of
              EQUAL => SOME v'
            | LESS => lookup L k
            | GREATER => lookup R k
                  
  fun insert d (k, v) =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case Key.compare (k,k') of
          EQUAL => Node (L, (k, v), R)
        | LESS => Node (insert L (k, v), (k', v'), R)
        | GREATER => Node (L, (k', v'), insert R (k, v))
end

val test = IntLtTreeDict.insert (IntLtTreeDict.insert IntLtTreeDict.empty (2,"b")) (1,"a")




structure IntModTreeDict : DICT =
struct

  structure Key : ORDERED = IntMod

  datatype 'v tree =
      Empty
    | Node of 'v tree * (int * 'v) * 'v tree

  type 'v dict = 'v tree

  val empty = Empty

  fun lookup d k =
    case d of
      Empty => NONE
    | Node (L, (k', v'), R) =>
          case Key.compare (k,k') of
              EQUAL => SOME v'
            | LESS => lookup L k
            | GREATER => lookup R k
                  
  fun insert d (k, v) =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case Key.compare (k,k') of
          EQUAL => Node (L, (k, v), R)
        | LESS => Node (insert L (k, v), (k', v'), R)
        | GREATER => Node (L, (k', v'), insert R (k, v))
end

val test = IntModTreeDict.insert (IntModTreeDict.insert IntModTreeDict.empty (2,"b")) (1,"a")




structure StringDict : DICT =
struct

  structure Key : ORDERED = StringLt

  datatype 'v tree =
      Empty
    | Node of 'v tree * (string * 'v) * 'v tree

  type 'v dict = 'v tree

  val empty = Empty

  fun lookup d k =
    case d of
      Empty => NONE
    | Node (L, (k', v'), R) =>
          case Key.compare (k,k') of
              EQUAL => SOME v'
            | LESS => lookup L k
            | GREATER => lookup R k
                  
  fun insert d (k, v) =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case Key.compare (k,k') of
          EQUAL => Node (L, (k, v), R)
        | LESS => Node (insert L (k, v), (k', v'), R)
        | GREATER => Node (L, (k', v'), insert R (k, v))
end
val test = StringDict.insert (StringDict.insert StringDict.empty ("b",2)) ("a",1)

(* type error
val test = StringDict.insert IntLtDict.empty ("a",1)
*)

functor TreeDict(K : ORDERED) : DICT =
struct

  structure Key : ORDERED = K

  datatype 'v tree =
      Empty
    | Node of 'v tree * (Key.t * 'v) * 'v tree

  type 'v dict = 'v tree

  val empty = Empty

  fun lookup d k =
    case d of
      Empty => NONE
    | Node (L, (k', v'), R) =>
          case Key.compare (k,k') of
              EQUAL => SOME v'
            | LESS => lookup L k
            | GREATER => lookup R k
                  
  fun insert d (k, v) =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case Key.compare (k,k') of
          EQUAL => Node (L, (k, v), R)
        | LESS => Node (insert L (k, v), (k', v'), R)
        | GREATER => Node (L, (k', v'), insert R (k, v))
end

structure IntOrdered : ORDERED =
struct
    type t = int
    val compare = Int.compare
end









structure IntLtDict = TreeDict (IntLt)
structure IntModDict = TreeDict(IntMod)
structure StringDict = TreeDict (StringLt)











val test = IntLtDict.insert (IntLtDict.insert IntLtDict.empty (2,"b")) (1,"a")
val test = StringDict.insert (StringDict.insert StringDict.empty ("b",2)) ("a",1)

val t1 = IntLtDict.insert 
    (IntLtDict.insert 
     (IntLtDict.insert 
      IntLtDict.empty (1023,"c")) 
     (111,"a")) (1025,"b")
(* if you try to run this, you will get a type error
val typeerror = IntModDict.lookup t1 1025 
*)

