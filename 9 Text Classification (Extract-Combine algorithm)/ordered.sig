signature ORDERED = 
sig
    type t
    val compare : t * t -> order 
end
(*
structure StringLt : ORDERED 

structure IntLt : ORDERED 

functor PairOrder (A : sig
                           structure O1 : ORDERED
                           structure O2 : ORDERED
                       end) : ORDERED 
*)


