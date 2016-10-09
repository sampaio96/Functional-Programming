structure StringLt : ORDERED = 
struct
    type t = string
    val compare = String.compare 
end


structure IntLt : ORDERED = 
struct
    type t = int
    val compare = Int.compare 
end

functor PairOrder (A : sig
                           structure O1 : ORDERED
                           structure O2 : ORDERED
                       end) : ORDERED =
struct

    type t = A.O1.t * A.O2.t

    fun compare ((x,y), (x',y')) =
        case A.O1.compare(x,x') of
            LESS => LESS
          | GREATER => GREATER
          | EQUAL => A.O2.compare(y,y')

end
