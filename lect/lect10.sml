fun map (f : 'a -> 'b) (l : 'a list) : 'b list =
    case l of
        [] => []
      | x :: xs => f x :: map f xs

fun reduce (c : 'a * 'a -> 'a) (n : 'a) (l : 'a list) : 'a =
    case l of
        [] => n
      | x :: xs => c (x , reduce c n xs)

fun sum (l : int list) : int = reduce (fn (x,y) => x + y) 0 l



(* ---------------------------------------------------------------------- *)
(* programming with function composition: wordcount and longestline *)

fun words (s : string) : string list = (String.tokens (fn #" " => true | #"\n" => true | _ => false) s)
fun lines (s : string) : string list = (String.tokens (fn #"\n" => true | _ => false) s)

val wordcount : string -> int = sum o map (fn _ => 1) o words
val longestlinelength : string -> int = reduce Int.max 0 o map wordcount o lines

(* ---------------------------------------------------------------------- *)
(* programming wth function composition: stock market best gain *)

val SOME minint = Int.minInt

(* assumes both trees have the same size *)
fun zip (l1 : 'a list, l2 : 'b list) : ('a * 'b) list = ListPair.zip (l1,l2)

fun suffixes (l : 'a list) : ('a list) list = 
    List.tabulate ((List.length l), (fn x => List.drop (l , x + 1)))

val maxL : int list -> int = reduce Int.max minint
val maxAll : (int list) list -> int = maxL o map maxL

fun withSuffixes (l : int list) : (int * int list) list = 
    zip (l, suffixes l)

val bestGain : int list -> int = 
      maxAll                                                        (* step 3 *)
    o (map (fn (buy,sells) => (map (fn sell => sell - buy) sells))) (* step 2 *)
    o withSuffixes                                                  (* step 1 *)

(* if it did drops instead, this would be 40
   if it didn't only look at the future, this would be 40 *)
val 21 = bestGain [40, 20, 0, 0, 0, 1, 3, 3, 0, 0, 9, 21]

(* ---------------------------------------------------------------------- *)
(* polynomials *)

(* represent 
   c0 + c1 x + c2 x^2 + ... cn x^n

   by
   fn 0 => c0 
    | 1 => c1
    | 2 => c2
    | ... 
    | n => cn
    | _ => 0 
*)

type poly = int -> int
(* x^2 + 2x + 1 *)
val example : poly =
     fn 0 => 1 
      | 1 => 2
      | 2 => 1
      | _ => 0

fun add (p1 : poly, p2 : poly) : poly = fn k => p1 k + p2 k

(* recursive version of mult *)
fun mult (c , d) = 
    fn k => let 
                fun convolution i =
                    case i of 
                        ~1 => 0
                      | _ => (c i) * (d (k - i)) + convolution (i - 1)
            in
                convolution k
            end

(* higher-order function version of mult *)
val sum = reduce (fn (x,y) => x + y) 0
fun upto n = List.tabulate(n+1, fn x => x)
fun mult (c : poly, d : poly) : poly = 
    fn e => sum (map (fn i => c i * d (e - i)) (upto e))

val test = fn 0 => 1 
            | 1 => 1
            | _ => 0
val test' = mult (test ,test)
val 1 = test' 0
val 2 = test' 1
val 1 = test' 2
val 0 = test' 3
