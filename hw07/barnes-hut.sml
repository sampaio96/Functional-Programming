structure BarnesHut =
struct

  open Mechanics
  structure BB = BoundingBox
  open Plane
  open TestData

  infixr 3 ++
  infixr 4 **
  infixr 3 -->

  datatype bhtree =
      Empty
    | Single of body
    | Cell of (Scalar.scalar * Plane.point) * BB.bbox * bhtree * bhtree * bhtree * bhtree
      (* ((mass, center), box, top-left, top-right, bottom-left, bottom-right) *)

  (* Projects the mass and center from the root node of a bhtree *)
  fun center_of_mass (T : bhtree) : Scalar.scalar * Plane.point =
      case T of
          Empty => (Scalar.zero, Plane.origin)
        | Single (m, p, _) => (m, p)
        | Cell (com, _, _,_,_,_) => com

  (* Note: Doesn't compare velocities as these are unaffected by compute_tree *)
  fun bodyEq ((m1, p1, _) : body, (m2, p2, _) : body) : bool =
      (Scalar.eq (m1, m2)) andalso Plane.pointEqual (p1, p2)

  fun bhtreeEq (t1 : bhtree, t2 : bhtree) : bool =
      case (t1, t2) of
          (Empty, Empty) => true
        | (Single b1, Single b2) => bodyEq (b1, b2)
        | (Cell ((cm1, cp1), bb1, tl1,tr1,bl1,br1), Cell ((cm2, cp2), bb2, tl2,tr2,bl2,br2)) =>
              Scalar.eq (cm1, cm2) andalso
              Plane.pointEqual (cp1, cp2) andalso
              BB.equal (bb1, bb2) andalso 
              bhtreeEq (tl1,tl2) andalso bhtreeEq (tr1,tr2) andalso 
              bhtreeEq (bl1,bl2) andalso bhtreeEq (br1,br2)
        | (_, _) => false

  (* ---------------------------------------------------------------------- *)
  (* TASKS *)

  (* TASK *)
  (* Compute the barycenter of four points *)
  (* Assumes the total mass of the points is positive *)
  fun barycenter ((m1,p1) : (Scalar.scalar * Plane.point),
                  (m2,p2) : (Scalar.scalar * Plane.point),
                  (m3,p3) : (Scalar.scalar * Plane.point),
                  (m4,p4) : (Scalar.scalar * Plane.point)) : Scalar.scalar * Plane.point =
    let val p = origin in
      let val m = Scalar.plus((Scalar.plus(m1, m2)), (Scalar.plus(m3, m4))) in
        (m, head (
          ((((p --> p1) ** m1) ++ ((p --> p2) ** m2)) ++ (((p --> p3) ** m3) ++ ((p --> p4) ** m4)))**Scalar.invert(m)
        ))
      end
    end

  (* TASK *)
  (* Compute the four quadrants of the bounding box *)
  fun quarters (bb : BB.bbox) : BB.bbox * BB.bbox * BB.bbox * BB.bbox =
      let val c = BB.center(bb) in
        let val (tl,tr,bl,br) = BB.corners(bb) in
          (BB.from2Points(tl,c),BB.from2Points(c,tr),BB.from2Points(bl,c),BB.from2Points(c,br))
        end
      end

  (* Test for quarters:
  val true = let val (tl,tr,bl,br) = quarters(bb4) 
             in BB.equal(tl,bb0) andalso BB.equal(tr,bb1) andalso
                BB.equal(bl, bb2) andalso BB.equal(br,bb3)
             end
  *)

  (* TASK *)
  (* Computes the Barnes-Hut tree for the bodies in the given sequence.
   * Assumes all the bodies are contained in the given bounding box,
     and that no two bodies have collided (or are so close that dividing the 
     bounding box will not eventually separate them).
     *)
  fun compute_tree (s : body Seq.seq) (bb : BB.bbox) : bhtree =
    case Seq.length(s) of
        0 => Empty
      | 1 => Single (Seq.nth 0 s)
      | _ => let val (tl,tr,bl,br) = quarters(bb) in

let val one = (compute_tree (Seq.filter (fn (_,p,_) => BB.contained (false,false,false,false) (p,tl)) s) tl) in
let val two = (compute_tree (Seq.filter (fn (_,p,_) => BB.contained (true,false,false,false) (p,tr)) s) tr) in
let val three = (compute_tree (Seq.filter (fn (_,p,_) => BB.contained (false,false,true,false) (p,bl)) s) bl) in
let val four = (compute_tree (Seq.filter (fn (_,p,_) => BB.contained (true,false,true,false) (p,br)) s) br) in

              Cell (
                (barycenter(
                  center_of_mass(one),
                  center_of_mass(two),
                  center_of_mass(three),
                  center_of_mass(four))), 
                bb,
                one, two, three, four
                )

            end end end end
           end



  (* Test for compute_tree:
  val three_bodies = Seq.cons body1 (Seq.cons body2 (Seq.cons body3 (Seq.empty())))
  val three_bodies_tree = Cell ((Scalar.fromInt 3, p22), bb4,
                                Cell ((Scalar.fromInt 2, p13), bb0,
                                      Single body3, Empty, Empty, Single body2), 
                                Empty, 
                                Empty, 
                                Single body1)
  val true = bhtreeEq (compute_tree three_bodies bb4, three_bodies_tree)
  *)

  (* TASK *)
  (* too_far p1 p2 bb t determines if point p1 is "too far" from 
   * a region bb with barycenter p2, given a threshold parameter t,
   * for it to be worth recuring into the region
   *)
  fun too_far (p1 : Plane.point) (p2 : Plane.point) (bb : BB.bbox) (t : Scalar.scalar) : bool =
      Scalar.lte(Scalar.divide(BB.diameter(bb),(distance p1 p2)),t)

  (* TASK *)
  (* Computes the acceleration on b from the tree T using the Barnes-Hut
   * algorithm with threshold t
   *)
  fun bh_acceleration (T : bhtree) (t : Scalar.scalar) (b : body) : Plane.vec =
    let val (mas,pos,vel) = b in
      case T of
          Empty => zero
        | Single (a) => Mechanics.accOn (b, a)
        | Cell ((m,c),bb,one,two,three,four) => case (too_far pos c bb t) of
                        true => Mechanics.accOnPoint(pos,(m,c))
                      | false => ((bh_acceleration one t b)++(bh_acceleration two t b))++((bh_acceleration three t b)++(bh_acceleration four t b))
    end

  (* TASK
     Given a threshold and a sequence of bodies, compute the acceleration
     on each body using the Barnes-Hut algorithm.
   *)
  fun barnes_hut (threshold : Scalar.scalar) (s : body Seq.seq) : Plane.vec Seq.seq = 
      let val bb = BB.fromPoints(Seq.map (fn (_,pos,_) => pos) s) in
        let val wegotourtree = (compute_tree s bb) in
          Seq.map (fn bods => bh_acceleration wegotourtree threshold bods) s
        end
      end

  (* Default value of the threshold, theta = 0.5 *)
  val threshold = (Scalar.fromRatio (1,2))

  val accelerations : body Seq.seq -> Plane.vec Seq.seq = barnes_hut threshold

end









