(* ---------------------------------------------------------------------- *)
(* map *)

fun double (x : int) : int = x * 2
fun doubAll (l : int list) : int list = 
    case l of 
        [] => []
      | x :: xs => double x :: doubAll xs

fun raiseBy (l : int list , c : int) : int list = 
    case l of 
        [] => []
      | x :: xs => (x + c) :: raiseBy (xs,c)

fun map (f : int -> int , l : int list ) : int list =
    case l of
        [] => []
      | x :: xs => f x :: map (f , xs)

(* map gets instantiated like this: *)
fun doubAll l = map (double , l)

(* or you can use an anonymous function *)
fun doubAll l = map (fn x => 2 * x , l)
(* doubAll can be defined anonymously too *)
val doubAll : int list -> int list = fn l => map (fn x => 2 * x , l)

fun raiseBy (l , c) = map (fn x => x + c , l)

(* what if we want to write something at a different type? *)
fun showAll (l : int list) : string list = 
    case l of 
        [] => []
      | x :: xs => Int.toString x :: showAll xs

(* map can be given a polymorphic type; 
   all instances of map have instance of this pattern
*)
fun map (f : 'a -> 'b , l : 'a list ) : 'b list =
    case l of
        [] => []
      | x :: xs => f x :: map (f , xs)

fun doubAll l = map (fn x => x + 1 , l)
fun showAll l = map (Int.toString , l)

(* ---------------------------------------------------------------------- *)
(* exists *)

fun evenP (x : int) : bool = (x mod 2) = 0

fun hasEven (l : int list) : bool = 
    case l of 
        [] => false
      | x :: xs => evenP x orelse hasEven xs

fun doctor (l : string list) : bool = 
    case l of 
        [] => false
      | x :: xs => (x = "doctor") orelse doctor xs

fun exists (p : 'a -> bool, l : 'a list) : bool =
    case l of
        [] => false
      | x :: xs => p x orelse exists (p, xs)

fun hasEven (l : int list) : bool = exists(evenP, l)
fun doctor (l : string list) : bool = exists((fn x => x = "doctor"), l)