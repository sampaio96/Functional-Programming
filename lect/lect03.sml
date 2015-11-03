(* Purpose: double the number n 
   Examples: 
   double 0 ==> 0
   double 3 ==> 6
*)
fun double (n : int) : int = 
    case n of 
        0 => 0
      | _ => 2 + (double (n - 1))

(* Tests *)
val 0 = double 0
val 6 = double 3

(* wrong *)
(* val 7 = double 3 *)

(* Good mistakes to make: forget to subtract (non-termination) *)

(* ---------------------------------------------------------------------- *)

(* Purpose: compute n! = n * (n-1) * (n-2) * ... * 1

   Examples: 
   fact 0 ==> 1
   fact 5 ==> 120
*)

fun fact (n : int) : int = 
    case n of 
        0 => 1
      | _ => n * (fact (n - 1))

(* Tests *)
val 1 = fact 0
val 120 = fact 5

(* ---------------------------------------------------------------------- *)

(* Purpose: compute the string "aa....a" with n letters

   Examples:
   doctor 0 ==> ""
   doctor 3 ==> "aaa"
*)
fun doctor (n : int) : string =
    case n of 
        0 => ""
      | _ => "a" ^ (doctor (n - 1))

(* Tests *)
val "" = doctor 0
val "aaa" = doctor 3

(* ---------------------------------------------------------------------- *)

(* Purpose: determine whether the number is even

   Examples:
   even 0 ==> true
   even 3 ==> false
   even 12 ==> true
   even 27 ==> false
*)
fun evenP (n : int) : bool = 
    case n of 
        0 => true
      | 1 => false 
      | _ => evenP (n - 2)

val true = evenP 0 
val false = evenP 1
val true = evenP 12
val false = evenP 27

(* Purpose: compute the string "hahaha..." with n total characters

   Examples:
   has 0 ==> ""
   has 1 ==> "a"
   has 2 ==> "ha"
   has 3 ==> "aha"
   has 4 ==> "haha"
   has 6 ==> "hahaha"
*)
fun ha (n : int) : string = 
    case n of 
        0 => ""
      | _ => (case evenP n of 
                   true => "h" ^ (ha (n - 1))
                 | false => "a" ^ (ha (n - 1)))
(* Tests *)    
val "" = ha 0 
val "a" = ha 1 
val "ha" =  ha 2
val "aha" = ha 3
val "haha" = ha 4
val "hahaha" = ha 6

(* case is an *expression* *)
fun ha' (n : int) : string = 
    case n of 
        0 => ""
      | _ => (case evenP n of 
                   true => "h" 
                 | false => "a") 
             ^ (ha' (n - 1)) 

(* Purpose: compute 2^n 
   Examples: exp 0 ==> 1
             exp 4 ==> 16
*)
fun exp (n : int) : int =
    case n of
	0 => 1
      | n => 2 * exp (n-1)

(* ----------------------------------------------------------------------  
   some examples of let and pairs that we will cover soon
*)

(* let *)

val 5 = let val x = 5 in x end
val 11 = let val x = 5 val y = 6 in x + y end

fun f (x : int) = 
    let 
        val twox = x * x 
    in 
        twox - 1 
    end

(* pairs *)

fun geom (x : int , y : int) = (x * y , 2 * (x + y))

fun geom (p : int * int) = 
    let val (x : int , y : int) = p in
        (x * y , 2 * (x + y))
    end

fun geom (p : int * int) = 
    case p of 
        (x : int , y : int) => (x * y , 2 * (x + y))



