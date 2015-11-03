(* Note: could export outerBox and fromPoint, but not needed in BH *)

structure BoundingBox : BBOX =
struct

  datatype bbox = BBox of Plane.point * Plane.point
           (* (bl, tr) where bl is the lower left corner of the box and
            *                tr is the upper right corner of the box
            *)

  fun toString (BBox (p1, p2) : bbox) : string =
      "(" ^ (Plane.pointToString p1) ^ ", " ^ (Plane.pointToString p2) ^ ")"

  fun equal (BBox (bl,tr) : bbox , BBox (bl',tr') : bbox) = Plane.pointEqual (bl,bl') andalso Plane.pointEqual (tr,tr')

  (* contained p bb evaluates to true if and only if the point p is in b *)
  fun contained (excludeLeft : bool, excludeRight : bool, excludeTop : bool, excludeBot : bool)
                (p : Plane.point, BBox (lowleft, upright) : bbox) : bool =
      let
        val (x, y) = Plane.cartcoord p
        val (xleft, ylow) = Plane.cartcoord lowleft
        val (xright, yup) = Plane.cartcoord upright
        fun lt_or_lte exclude = case exclude of true => Scalar.lt | false => Scalar.lte
      in
        lt_or_lte excludeLeft (xleft, x) andalso lt_or_lte excludeRight (x, xright) andalso
        lt_or_lte excludeBot (ylow, y) andalso lt_or_lte excludeTop (y, yup)
      end

  (* Returns the four corners of the bounding box in 
     top left, top right, bottom left, bottom right order *)
  fun corners (BBox (lowleft, upright) : bbox) : Plane.point * Plane.point * Plane.point * Plane.point =
      let
        val (xleft, ylow) = Plane.cartcoord lowleft
        val (xright, yup) = Plane.cartcoord upright
      in
        (Plane.fromcoord (xleft, yup),
         upright,
         lowleft,
         Plane.fromcoord (xright, ylow))
      end

  (* length of the diagonal *)
  fun diameter (BBox (ll,ur)) = Plane.distance ll ur

  (* fromPoint p returns the smallest bounding box containing p
   * Namely, it returns the box consisting of the single point p
   *)
  fun fromPoint (p : Plane.point) : bbox = BBox (p, p)

  (* outerBox (bb1, bb2) returns the smallest bounding box containing both
   * all the points in bb1 and all the points in bb2
   *)
  fun outerBox (BBox (lowleft1, upright1) : bbox,
                BBox (lowleft2, upright2) : bbox) : bbox =
      let
        val (xleft1, ylow1) = Plane.cartcoord lowleft1
        val (xright1, yup1) = Plane.cartcoord upright1
        val (xleft2, ylow2) = Plane.cartcoord lowleft2
        val (xright2, yup2) = Plane.cartcoord upright2
      in 
          BBox (Plane.fromcoord (Scalar.min (xleft1, xleft2),
                                 Scalar.min (ylow1, ylow2)),
                Plane.fromcoord (Scalar.max (xright1, xright2),
                                 Scalar.max (yup1, yup2)))
      end

  (* from2Points (p1, p2) returns the smallest bounding box containing both
   * p1 and p2
   *)
  fun from2Points (p1 : Plane.point, p2 : Plane.point) : bbox =
      outerBox (fromPoint p1, fromPoint p2)

  (* Computes the center point of the bounding box *)
  fun center (BBox (lowleft, upright) : bbox) : Plane.point =
      Plane.midpoint lowleft upright

  (* Computes the minimum bounding box of a sequence of points.
     or returns NONE if the sequence is empty
     *)
  fun fromPoints_o (s : Plane.point Seq.seq) : bbox option =
      let fun join (b, NONE) = b
            | join (NONE, b) = b
            | join (SOME b1, SOME b2) = SOME (outerBox (b1,b2))
      in Seq.mapreduce (SOME o fromPoint) NONE join s
      end

  fun fromPoints (s : Plane.point Seq.seq) : bbox = 
      case fromPoints_o s of 
          NONE => raise Fail "fromPoints: got an empty sequence"
        | SOME x => x


end
