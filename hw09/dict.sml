
functor TreeDict(Key : ORDERED) : DICT =
struct

  fun log2 (n : int) : int = 
      case n of 
          0 => 0 (* hack *)
        | 1 => 1
        | _ => 1 + log2 (n div 2)

  structure Key : ORDERED = Key

  (* invariant: sorted according to Key.compare *)
  datatype 'v tree =
      Empty
    | Node of 'v tree * (Key.t * 'v) * 'v tree

  type 'v dict = 'v tree 

  val empty = Empty

  fun depth (t : 'v tree) : int =
      case t of 
          Empty => 0
        | Node(l,_,r) => 1 + Int.max(depth l, depth r)

  fun size (t : 'v tree) : int =
      case t of 
          Empty => 0
        | Node(l,_,r) => 1 + size l + size r

  fun print_stats (t : 'v tree) : unit =
      print ("depth : " ^ Int.toString (depth t) ^ 
             " log2 size : " ^ Int.toString (log2 (size t)) ^ "\n")

  fun lookup d k =
      let fun lk d = 
          case d of
              Empty => NONE
            | Node (L, (k', v'), R) =>
                  case Key.compare (k,k') of
                      EQUAL => SOME v'
                    | LESS => lk L 
                    | GREATER => lk R
      in 
          lk d
      end

  fun lookup' d k = valOf (lookup d k)
                  
  fun insert d (k, v) =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case Key.compare (k,k') of
          EQUAL => Node (L, (k, v), R)
        | LESS => Node (insert L (k, v), (k', v'), R)
        | GREATER => Node (L, (k', v'), insert R (k, v))

  fun insertWith (c : 'v * 'v -> 'v) (d : 'v dict) (k : Key.t, v : 'v) : 'v dict =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case Key.compare (k,k') of
          EQUAL => Node (L, (k, (c(v,v'))), R)
        | LESS => Node (insertWith c L (k, v), (k', v'), R)
        | GREATER => Node (L, (k', v'), insertWith c R (k, v))

  fun map f d = 
      case d of
          Empty => Empty
        | Node(l,(k,v),r) => Node (map f l , (k, f v) , map f r)

  fun split d k = 
      case d of 
          Empty => (Empty , NONE , Empty)
        | Node (l , (k',v') , r) => 
              (case Key.compare (k,k') of
                   EQUAL => (l , SOME v' , r)
                 | LESS => let val (ll , vo , lr) = split l k 
                           in (ll , vo , Node (lr , (k',v') , r)) end
                 | GREATER => let val (rl , vo , rr) = split r k 
                              in (Node (l , (k',v') , rl) , vo , rr) end)

  fun merge' c (d1, d2) = 
    case d1 of 
        Empty => d2
      | Node (l1 , (k,v1) , r1) =>
            let val (l2 , v2o , r2) = split d2 k 
            in
                Node (merge' c (l1, l2) , 
                      (k , case v2o of NONE => v1 | SOME v2 => c (v1,v2)), 
                      merge' c (r1, r2))
            end

  (* optimize inserts *)
  fun merge c (d1,d2) = 
      case d1 of
          Node(Empty, kv1, Empty) => insertWith c d2 kv1
        | _ => case d2 of
                 Node(Empty, kv2, Empty) => insertWith c d1 kv2
               | _ => merge' c (d1,d2)

  fun fromSeq s =
      Seq.mapreduce (fn (k,v) => insert empty (k,v)) empty (merge (fn(v1,v2) => v1)) s

  fun toSeq d =
      case d of
          Empty => Seq.empty()
        | Node (l , x , r) => Seq.append (toSeq l) (Seq.cons x (toSeq r))

  fun valueSeq d =
      case d of
          Empty => Seq.empty()
        | Node (l , (_,v) , r) => Seq.append (valueSeq l) (Seq.cons v (valueSeq r))
        
  fun max d = 
      case d of 
          Empty => NONE
        | Node (l, (k,v), r) => 
              case max r of 
                  NONE => SOME (k,v)
                | SOME m' => SOME m'
end

functor Dict(K : ORDERED) = TreeDict(K)
