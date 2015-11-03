(* Purpose: double the number n
 * Examples:
 * double 0 ==> 0
 * double 2 ==> 4
 *)
fun double (n : int) : int =
    case n of
      0 => 0
    | _ => 2 + double (n - 1)

(* Tests for double *)
val 0 = double 0
val 4 = double 2

(* Purpose: determine whether the number is even
 * Examples:
 * evenP 0 ==> true
 * evenP 3 ==> false
 * evenP 12 ==> true
 * evenP 27 ==> false
*)
fun evenP (n : int) : bool =
    case n of
      0 => true
    | 1 => false
    | _ => evenP (n - 2)

(* Tests for evenP *)
val true = evenP 0
val false = evenP 1
val true = evenP 12
val false = evenP 27

(* Purpose: sum all numbers from 0 to n *)
fun summ (n : int) : int =
	case n of
	  0 => 0
	| _ => n + summ (n - 1)

(* Tests for summ *)
val 0 = summ 0
val 6 = summ 3
val 55 = summ 10
val 45 = summ 9

(* Purpose: write n "ha"*)
fun ha (n : int) : string =
	case n of
	  0 => ""
	| _ => "ha" ^ ha (n - 1)

(* Tests for ha *)
val "hahaha" = ha(3)
val "hahahahaha" = ha(5)
val "" = ha(0)



fun oddP (n : int) : bool =
	case n of
	  0 => false
	| 1 => true
	| _ => oddP (n-2)

val false = oddP 0
val true = oddP 1
val false = oddP 12
val true = oddP 27


fun divisibleByThree (n : int) : bool =
	case n of
	  0 => true
	| 1 => false
	| 2 => false
	| _ => divisibleByThree (n - 3)

val false = divisibleByThree 1
val true = divisibleByThree 12
val false = divisibleByThree 100 
val true = divisibleByThree 27



fun add (x : int, y : int) : int =
	case x of 
	   0 => y
	 | _ => 1 + add (x-1, y)

val 12 = add(3,9)
val 2 = add(1,1) 
val 0 = add(0,0)












