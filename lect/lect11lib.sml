(* linear time getmin; use heaps instead *)
functor PQ(E : sig
                   type t
                   val cmp : t * t -> order
               end) : sig
                          type elem
                          type pq
                          val empty : pq
                          val insert : pq -> elem -> pq
                              
                          (* 0 <= size q *)
                          val size : pq -> int 

                          (* assumes size is > 0 *)
                          val delmin : pq -> (elem * pq) 

                          val askmin : pq -> (elem * pq) option
                      end =
struct

    type elem = E.t

    type pq = elem list 

    val empty : pq = []

    fun insert (p : pq) (x : elem) = x :: p

    fun askmin (p : pq) : (elem * pq) option = 
        case p of 
            [] => NONE
          | (x :: xs) => 
                (case askmin xs of
                     NONE => SOME (x,empty)
                   | SOME (min_xs,leftovers_xs) => 
                         case E.cmp (x, min_xs) of
                             LESS => SOME (x, insert leftovers_xs min_xs)
                           | _ => SOME (min_xs, insert leftovers_xs x))

    fun size (q : pq) : int = List.length q
        
    fun delmin (q : pq) : elem * pq = Option.valOf (askmin q) (* BAD STYLE; for illustration only *)

end

(* simple O(n) implementation *)
functor Dict(K : sig 
                     type t 
                     val cmp : t * t -> order 
                 end) : sig 
                            type key
                            type 'v dict
                            val empty : 'v dict
                            val insert : 'v dict -> key * 'v -> 'v dict
                            val merge : 'v dict * 'v dict -> 'v dict
                                
                            (* assumes key is in the dictionary *)
                            val lookup : 'v dict -> key -> 'v 
                        end 
=
struct

    type key = K.t

    type 'v dict = (K.t * 'v) list 

    val empty : ('v) dict = []

    (* newer inserts take precedence *)
    fun insert (d : 'v dict) (x : key * 'v) = x :: d

    (* RHS takes precedence *)
    fun merge (d1 : 'v dict, d2 : 'v dict) : 'v dict = d2 @ d1

    (* assumes k is in d *)
    fun lookup (d : 'v dict) (k : key) : 'v = 
        case d of 
            [] => raise Fail "impossible"
          | ((k',v') :: xs) => 
                case K.cmp (k, k') of
                    EQUAL => v'
                  | _ => lookup xs k 

end


fun reduce (c : 'a * 'a -> 'a) (n : 'a) (l : 'a list) : 'a = 
    case l of 
        [] => n
      | x :: xs => c(x,reduce c n xs)

fun flatten (l : ('a list) list) : 'a list = reduce (op@) [] l
