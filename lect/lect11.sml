use "lect11lib.sml";

datatype hufftree =
    Leaf of char * int
  | Node of hufftree * int * hufftree

fun get_freq (h : hufftree) : int = 
    case h of
        Leaf (_,freq) => freq
      | Node (_,freq,_) => freq

structure Pq = PQ(struct type t = hufftree val cmp = (fn (h1,h2) => Int.compare (get_freq h1, get_freq h2)) end)

datatype direction = Left | Right

structure D = Dict(struct type t = char val cmp = Char.compare end)
type dict = (direction list) D.dict

type decode_result = (char * direction list) option 

(* assumes p has at least one element *)
fun build_from_pq (p : Pq.pq) : hufftree = 
    case Pq.size p > 1 of
        false => let val (mintree,_) = Pq.delmin p in mintree end
      | true => 
            let val (mintree1, p') = Pq.delmin p 
                val (mintree2, p'') = Pq.delmin p'
            in 
                build_from_pq (Pq.insert p'' 
                               (Node(mintree1, 
                                     get_freq mintree1 + get_freq mintree2, 
                                     mintree2)))
            end
            
fun build (chars : (char * int) list) : hufftree = 
    build_from_pq (foldr (fn (f, q) => Pq.insert q (Leaf f)) Pq.empty chars)

fun hufftree_to_dict (h : hufftree, directions : direction list) : dict = 
    case h of 
        Leaf (c,k) => D.insert D.empty (c,directions)
      | Node (l,_,r) => 
            D.merge (hufftree_to_dict(l,directions@[Left]),
                     hufftree_to_dict(r,directions@[Right]))
                
(* assumes characters are in m *)
fun encode (h : hufftree, s : string) : direction list =
    flatten (map (D.lookup (hufftree_to_dict (h,[]))) (String.explode s))

fun decode (h : hufftree, dirs : direction list) : decode_result =
    case h of
        Leaf (c,_) => SOME (c,dirs)
      | Node(l,_,r) => 
            case dirs of 
                (Left :: dirs') => decode (l,dirs')
              | (Right :: dirs') => decode (r,dirs')
              | [] => NONE

val data = 
[(#" ", 1965675),
(#"e", 748002),
(#"o", 665179),
(#"t", 606356),
(#"a", 584128),
(#"i", 517051),
(#"n", 450421),
(#"l", 386714),
(#"h", 378106),
(#"s", 365691),
(#"r", 329150),
(#"u", 258117),
(#"m", 250611),
(#"d", 238126),
(#"y", 230752),
(#"w", 206117),
(#".", 202744),
(#"\n",200000),
(#"g", 184247),
(#"c", 154495),
(#"b", 137825),
(#"f", 130662),
(#"p", 125076),
(#"!", 121603),
(#"k", 117667),
(#"v", 66590),
(#"'", 52259),
(#"?", 42642),
(#"#", 38608),
(#"j", 26214),
(#"-", 25265),
(#",", 24498),
(#":", 17280),
(#"z", 16587),
(#"x", 14982),
(#")", 14671),
(#"2", 13650),
(#"1", 9661),
(#"0", 8842),
(#"*", 8541),
(#"<", 8359),
(#"q", 7748),
(#"(", 7065),
(#"&", 6103),
(#"3", 5903),
(#"4", 5526),
(#"/", 5399),
(#"_", 4323),
(#";", 4070),
(#"=", 3982),
(#"5", 3931),
(#">", 3231),
(#"8", 2274),
(#"6", 2175),
(#"9", 2163),
(#"7", 1957),
(#"@", 1814),
(#"~", 1188),
(#"]", 930),
(#"$", 884),
(#"|", 751),
(#"^", 534),
(#"[", 487),
(#"+", 422),
(#"%", 246),
(#"`", 242),
(#"{", 225),
(#"}", 179)]
