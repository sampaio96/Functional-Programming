
val true = CM.make("sources.cm");

exception Unimplemented
val map = Seq.map

fun inteq(x,y) = case Int.compare (x,y) of EQUAL => true | _ => false
fun stringeq(x,y) = case String.compare (x,y) of EQUAL => true | _ => false

(* USE THESE FOR TESTING ONLY ! *)
fun seqFromList (l : 'a list) : 'a Seq.seq = 
    List.foldr (fn (x,y) => Seq.cons x y) (Seq.empty ()) l
fun seqToList   (s : 'a Seq.seq) : 'a list = Seq.mapreduce (fn x => [x]) [] (op@) s

fun oddP (n : int) = inteq(n mod 2, 1)

fun seqFromList2 (l : 'a list list) : 'a Seq.seq Seq.seq = seqFromList (List.map seqFromList l)
fun seqToList2 (l : 'a Seq.seq Seq.seq) : 'a list list = seqToList (Seq.map seqToList l)

(* ---------------------------------------------------------------------- *)

(* Task *)

fun seqExists (f : 'a -> bool) : 'a Seq.seq -> bool =
	fn s => not(Seq.length(Seq.filter f s) = 0)

(* Tests 
val true  = seqExists oddP (seqFromList [1,2,3])
val false = seqExists oddP (seqFromList [2,4,6])
*)

(* ---------------------------------------------------------------------- *)

(* Task *)

fun myAppend (s1 : 'a Seq.seq) (s2 : 'a Seq.seq) : 'a Seq.seq =
	let val a = Seq.length(s1) in
		Seq.tabulate (fn x =>
				case (x<a) of
					true => Seq.nth x s1
				  | false => Seq.nth (x-a) s2)
		(a+Seq.length(s2))
	end

(* Tests 
val [1,2,3,4,5,6] = seqToList (myAppend (seqFromList [1,2,3], seqFromList [4,5,6]))
val [1,2,3] = seqToList (myAppend (seqFromList [1,2,3], seqFromList []))
*)

(* Task *)
fun reverse (s : 'a Seq.seq) : 'a Seq.seq =
    Seq.tabulate (fn i => Seq.nth ((Seq.length s) - (i + 1)) s) (Seq.length s)

(* Task *)

(* assumes s is valid: 
   rectangular n x m where n,m > 0
   *)
fun transpose (s : 'a Seq.seq Seq.seq) : 'a Seq.seq Seq.seq =
	Seq.length(s)
	Seq.nth 1 s

(* Tests 
val [[1,4],[2,5],[3,6]] = seqToList2 (transpose (seqFromList2 [[1,2,3],[4,5,6]]))
*)

(* ---------------------------------------------------------------------- *)

(* Stocks *)

val SOME minint = Int.minInt

