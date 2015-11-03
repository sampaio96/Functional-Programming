
(* Purpose: returns m + n. Assumes m and n are natural numbers. *)
fun add (m : int, n : int) =
  case m of
    0 => n
  | _ => 1 + (add (m - 1, n))

(* Task: Implement and document this function. *)
(* DOCUMENTATION GOES HERE *)
fun mult (m : int, n : int) : int =
    (* Implementation goes here. *)
    case m of
      0 => 0
    | _ => add (n, mult(m-1,n))


(* Task: Implement and document this function. *)
(*
H0 is 0
H1 is 1
H2 is 3/2
H3 is 11/6
  *)
fun harmonic (n : int) : real =
    case n of
      0 => 0.0
    | _ => (1.0 / (real n)) + harmonic (n-1)


(* Example of how to test to a function that returns a real:
   because comparing equality of floating point numbers is fragile,
   SML doesn't let you pattern-match against them,
   so you need to use an explicit equality test.  *)
val true = Real.==(harmonic 1, 1.0)
val true = Real.==(harmonic 0, 0.0)
val true = Real.==(harmonic 2, 1.5)

(* Task: Implement this function. *)
(*  DOCUMENTATION GOES HERE *)



fun divmod (n : int, d : int) : int * int =
    let val q : int = d in
    let val d : int = 0 in

      let fun helper (n : int, d : int) : int * int =
          
            if n<q then (d,n)
            else helper (n-q, d+1)
          
      in helper(n,d)
      end
    end
    end





(* Task: Implement this function. *)
(* DOCUMENTATION GOES HERE *)
fun sum_digits (n : int, b : int) : int =
    let val (q,r) = divmod(n,b) in
    case n of
      0 => 0
    | _ => r + sum_digits(q,b)

end















