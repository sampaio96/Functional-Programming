use "lib.sml";

(* ---------------------------------------------------------------------- *)
(* quicksort on lists *)

(* Task *)
fun filter_l (l : int list,p:int,r:rel) : int list =
  case r of
    LT => (case l of
            [] => []
            | x :: xs => case (x<p) of
                        true => x::filter_l(xs,p,r)
                        | false => filter_l(xs,p,r) )
    | GEQ => (case l of
            [] => []
            | x :: xs => case (x<p) of
                        true => filter_l(xs,p,r)
                        | false => x::filter_l(xs,p,r) )

(* Task *)
fun quicksort_l (l : int list) : int list =
  case l of
      [] => []
      | [x] => [x]
      | x::xs => quicksort_l(filter_l(xs,x,LT))@[x]@quicksort_l(filter_l(xs,x,GEQ))


(* ---------------------------------------------------------------------- *)
(* quicksort on trees *)

(* Task *)
fun combine (t1 : tree, t2 : tree) : tree =
  case t1 of Empty => t2
           | Node (l,x,r) => Node (combine(l,r), x, t2)



(* Task *)
fun filter (t : tree, i : int, r : rel) : tree = 
  case t of Empty => Empty
        |_ => let val Node(rx,x,lx) = t in
            case r of
              LT => (case (x < i) of
                true => Node(filter(lx,i,r),x,filter(rx,i,r))
                |_   => combine(filter(lx,i,r),filter(rx,i,r)) )
              |_  => (case (x < i) of
                true => combine(filter(lx,i,r),filter(rx,i,r))
              |_    => Node(filter(lx,i,r),x,filter(rx,i,r)) )

end

(* Task *)
fun quicksort_t (t : tree) : tree =
  case t of
      Empty => Empty
      | Node(Empty,x,Empty) => Node(Empty,x,Empty)
      | Node(l,x,r) => Node(quicksort_t(filter(l,x,LT)),x,quicksort_t(filter(r,x,GEQ)))

(* ---------------------------------------------------------------------- *)
(* rebalance *)

(* Task *)
fun takeanddrop (t : tree, i : int) : tree * tree =
    case (t,i) of 
        (_ , 0) => (Empty, t)
      | (Empty, _) => (Empty, Empty)
      | (Node(Empty,x,Empty), 1) => (t, Empty)
      | (Node (l, x, r), _) => 
            (case i <= size(l) of
                 true => let val (t,d) = takeanddrop (l,i) in (t, Node(d,x,r)) end
              | false => let val (t,d) = takeanddrop (r,(i - size l)) in (Node(l,x,t), d) end)

(* the rest of rebalance interms of your takeanddrop *)
fun halves (t : tree) : tree * int * tree =
    let
      val (l , vr) = takeanddrop (t , (size t) div 2)
      val (Node (Empty, v , Empty) , r) = takeanddrop (vr , 1)
    in
      (l , v , r)
    end

fun rebalance (t : tree) : tree =
    case t
     of Empty => Empty
      | _ =>
        let
          val (l , x , r) = halves t
        in
          Node (rebalance l , x , rebalance r)
        end