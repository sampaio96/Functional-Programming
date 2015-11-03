
signature COUNTER =
sig

    (* invariant: counter always represents a natural number *)
    type counter 

    val zero : counter
    val increment : counter -> counter
    val value : counter -> int

end

structure TMICounter : COUNTER =
struct

    (* representation invariant: C x always satisfies x >= 0 *)
    datatype counter = Nat of int
        
    val zero = Nat 0
    fun increment counter = case counter of Nat(x) => Nat(x + 1)
    fun value counter = case counter of Nat(x) => x

end
(*val ~1 = TMICounter.value (TMICounter.increment ~2)*)

signature BETTER_COUNTER =
sig

    structure C : COUNTER

    (* assuming n >= 0, increment c that many times *)
    val increment_many_times : C.counter * int -> C.counter

end

functor BetterCounter(C : COUNTER) : BETTER_COUNTER =
struct

    structure C = C

    (* assuming n >= 0, increment c that many times *)
    fun increment_many_times (aa: C.counter, bb : int) : C.counter = 
        case bb of 0 => aa
                | _ => increment_many_times (C.increment(aa), bb-1)

end


(* ---------------------------------------------------------------------- *)

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

structure YMDOrder : ORDERED =
struct

    type t = int * (int * int)

    fun compare ((y,(m,d)),(y',(m',d')))  = 
        case Int.compare (y,y') of 
            LESS => LESS 
          | GREATER => GREATER
          | EQUAL => (case Int.compare (m,m') of
                          LESS => LESS
                        | GREATER => GREATER
                        | EQUAL => Int.compare (d,d'))
                          

end

val LESS = YMDOrder.compare ((1999,(1,1)), (1999,(1,2)))
val LESS = YMDOrder.compare ((1999,(1,3)), (1999,(2,2)))
val GREATER = YMDOrder.compare ((2000,(1,1)), (1999,(2,2)))

signature TWO_ORDERS =
sig
    structure O1 : ORDERED
    structure O2 : ORDERED

end

functor PairOrder(T : TWO_ORDERS) : ORDERED =
struct
    
    type t
    fun compare (aa:t, bb:t) : order = case T.O1(aa,bb) of EQUAL => T.O2(aa,bb)
                                                        | _ => T.O1(aa,bb)

end






