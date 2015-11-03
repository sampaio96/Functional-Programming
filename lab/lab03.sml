(* you can remove this defintion when you're done to make sure you didn't
 * miss any functions
 *)
exception Unimplemented

fun evenP(n : int) : bool =
    case n
     of 0 => true
      | 1 => false
      | _ => evenP(n-2)

fun fib (n : int) : int =
    case n
     of ~1 => 0
      | 0 => 1
      | 1 => 1
      | _ => fib(n-1) + fib(n-2)


(* Task 4.1 *)
fun merge _ = raise Unimplemented

fun evens(l:int list):int list=
	case l of
		[] => []
	  | x::xs => case evenP(x) of
	  			 		true => x::evens(xs)
	  			 		| _ => evens(xs)

fun fastfib (n : int) : int*int =
	let fun fibhelper (a, b, n) = case n of
						0 => (a,b)
					   | _ => fibhelper(b, a+b, n-1)
					in fibhelper (0, 1, n) end
(*

Wfastfib = 




*)