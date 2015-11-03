
fun evenP (n : int) : bool = (n mod 2) = 0

fun hasEven (l : int list) : bool = 
    case l of
        [] => false
      | x :: xs => evenP x orelse hasEven xs

fun firstEven (l : int list) : int = 
    case l of 
        [] => raise Fail "no even"
      | x :: xs => 
            (case evenP x of
                 true => x
               | false => firstEven xs)

fun example (l : int list)  = 
    case hasEven l of 
        true => firstEven l
      | false => firstEven l  (* oops! *)

fun politeFirstEven (l : int list) : int option = 
    case l of
        [] => NONE
      | x :: xs => 
            (case evenP x of 
                 true => SOME x
               | false => politeFirstEven xs)

fun example' (l : int list) = 
    case politeFirstEven(l) of
        SOME e => e
      | NONE => 0 (* no mistake to make *)
