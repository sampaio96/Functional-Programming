

(* map *)

fun map (f : 'a -> 'b, l : 'a list) : 'b list =
    case l of 
        [] => []
      | x :: xs => f x :: map (f , xs)

(* ---------------------------------------------------------------------- *)
(* filter *)

fun evenP (n : int) : bool = (n mod 2) = 0

fun evens (l : int list) : int list =
    case l of
	 [] => []
	| x :: xs => case evenP x of
			  true => x :: evens xs
			 |false => evens xs

fun keepUpper (l : char list) : char list =
    case l of 
        [] => []
      | x :: xs => (case Char.isUpper x of 
                        true => x :: keepUpper xs
                      | false => keepUpper xs)

(* TASK *)
fun filter (p : 'a -> bool, l : 'a list) : 'a list =
    case l of
        [] => []
      | x :: xs => (case p x of
                      true => x:: filter(p,xs)
                    | false => filter(p,xs) )

(* TASK *)
fun evens (l : int list) : int list = filter(evenP,l)
fun keepUpper (l : char list) : char list = filter(Char.isUpper,l)

(* val [#"B"] = keepUpper [#"a", #"B"] *)

(* TASK *)
fun quicksort_l (l : int list) : int list =
  case l of
      [] => []
      | [x] => [x]
      | x::xs => quicksort_l(filter(fn y => y < x,xs))@[x]@quicksort_l(filter(fn y => y >= x,xs))


(* ---------------------------------------------------------------------- *)
(* map and filter *)

(* TASK *)
fun ages_over_18 (l : (string * int) list) : (string * int) list =
  case l of
      [] => []
    | x::xs => let val (name, birth) = x in (case (birth < 1997) of
        true => (name,2014-birth)::ages_over_18(xs)
      | _ => ages_over_18(xs)) end

(*
val [("Sri",22),("Dan",32)] = ages_over_18 [("Sri",1992),("Dan",1982),("Cassie",2004)]
*)


(* ---------------------------------------------------------------------- *)
(* all *)
(* all positive *)
fun allPos (l : int list) : bool =
    case l of
         [] => true
       | x :: xs => (x > 0) andalso allPos xs

fun allOfLength (len : int, l : 'a list list) : bool =
     case l of
          [] => true
         | x :: xs => ( (List.length x = len) andalso allOfLength(len, xs))

(* TASK: define a function named all *)
fun all (f, l: 'a list) : bool =
      case l of
          [] => true
        | x::xs => (f x) andalso all(f, xs)

(* TASK *)
fun allPos (l : int list) : bool = all(fn x => x < 0, l)
fun allOfLength (len : int, l : 'a list list) : bool = all(fn x => List.length x = len, l)

fun square(l : 'a list list) : bool =
    let val q = List.length l in 
      all (fn x => List.length x = q, l)
    end

  


(* ---------------------------------------------------------------------- *)
(* reduce *)

fun sum (l : int list) : int = 
   case l of 
        [] => 0
      | x :: xs => x + (sum xs)

fun join (l : string list) : string = 
    case l of 
        [] => ""
      | x :: xs => x ^ join xs

(* TASK *)

fun reduce(c : 'a * 'a -> 'a, n : 'a, l : 'a list) : 'a =
    case l of
        [] => n
      | x::xs => c (x,reduce(c, n, xs))

(* TASK *)
fun sum (l : int list) : int = reduce (fn(a,b) => a+ b, 0, l)
fun join (l : string list) : string = reduce (fn(a,b) => a ^ b, "", l)


(* ---------------------------------------------------------------------- *)
(* reduce *)

fun lines (s : string) : string list =
    (String.tokens (fn #"\n" => true | _ => false) s)

fun words (s : string) : string list =
    (String.tokens (fn #" " => true | #"\n" => true | _ => false) s)

(* TASK *)


fun wordcount (s : string) : int = reduce (fn(x,y) => x+y, 0,
                                map(fn x => 1, words(s)) )

fun longestline (s : string) : int = reduce (fn (x, y) => (case x<y of true => y | false => x), 0, map(wordcount, lines(s)) )





val 7 = longestline "for life’s not a paragraph\nAnd death i think is no parenthesis\n"

val 12 = wordcount "for life’s not a paragraph\nAnd death i think is no parenthesis\n"

