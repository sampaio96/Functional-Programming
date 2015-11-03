
(* ---------------------------------------------------------------------- *)
(* SECTION 2 *)

fun zip (l1 : int list, l2 : string list) : (int * string) list =
    case l1 of
        [] => []
      | x::xs => (case l2 of
                    [] => []
                  | y::ys => (x,y)::zip(xs,ys))

fun unzip (l : (int * string) list) : int list * string list =

    let fun first (l1 : (int * string) list) : int list =
        case l1 of [] => []
                | p::ps => (let val (a,b) = p in
                                    a::first(ps)
                            end)
    in
        let fun second (l2 : (int * string) list) : string list =
            case l2 of [] => []
                    | q::qs => (let val (x,y) = q in
                                        y::second(qs)
                                end)
        in
    (first(l),second(l))
        end
    end


(* ---------------------------------------------------------------------- *)
(* SECTION 3 *)

fun lasHelp (l : int list, x : int, acc : int) : int list * int =
    case l of   [] => ([], acc)
                |_ =>   let fun around (A: int list, y: int, bcc: int) : int list * int * int =
                            case A of [] => ([], y, bcc)
                                | q::qs => (case q=y of false => (q::qs, y, bcc)
                                                       | true => around(qs, y, bcc+1) )
                        in (let val (g: int list, h: int, i: int) = around(l, x, acc) in (g,i) end)
                        end

fun look_and_say (l : int list) : int list =
    case l of [] => []
        | x::xs => let val (a,b) = lasHelp(l, x, 0) in b::x::look_and_say(a) end

(* ---------------------------------------------------------------------- *)
(* SECTION 4 *)

(* Purpose: add n to each element of the list l
 * (the raiseBy function from lecture with a different name).
 * Examples:
 *
 * add_to_each ([], 7) == nil
 * add_to_each (1::2::3::[], 3) == 4::5::6::[]
 * add_to_each (6::5::4::[], ~3) == 3::2::1::[]
 *)
fun add_to_each (l : int list, n : int) : int list =
    case l of
        [] => []
      | x::xs => x + n :: add_to_each (xs, n)

val [] = add_to_each ([], 7)
val 4::5::6::nil = add_to_each (1::2::3::[], 3)
val 3::2::1::nil = add_to_each (6::5::4::[], ~3)

(* Purpose: computes the list of prefix sums for the argument list.  The
 *          i-th int in the result list is the sum of the first i int's
 *          in the argument list.
 * Examples:
 *  prefixSum [] ==> []
 *  prefixSum (1::2::3::[]) ==> 1::3::6::[]
 *  prefixSum (5::3::1::[]) ==> 5::8::9::[]
 *)
fun prefixSum (l : int list) : int list =
    case l of
      [] => []
    | x::xs => x :: add_to_each (prefixSum xs, x)

(* Tests for prefixSum *)
val [] = prefixSum []
val [1,3,6] = prefixSum [1,2,3]
val [5,8,9] = prefixSum [5,3,1]


(* uncomment this line and fill it in when you decide on the type *)
fun prefixSumHelp (l : int list, p : int) : int list = 
    case l of [] => []
            | x::xs => x+p::prefixSumHelp(xs, x+p)

fun prefixSumFast (l : int list) : int list =
    case l of [] => []
            | x::xs => x::prefixSumHelp(xs,x)


(* ---------------------------------------------------------------------- *)
(* SECTION 5 *)

fun subset_sum (l : int list, s : int) : bool =
    case l of [] => s = 0
            | x::xs => let val (a,b) = (subset_sum(xs,s),subset_sum(xs,s-x)) in
                case (a,b) of
                    (true, _) => true
                  | (_, true) => true
                  | (_, _) => false
              end
subset_sum([6,14,5,1,10], 0) = true







