signature BBOX = sig

  type bbox
  val toString : bbox -> string
  val equal : bbox * bbox -> bool

  (* contained bs p bb evaluates to true if and only if the point p is in b.
     The four booleans are 
        (exclude left side, exclude right side, exclude top side, exclude bottom side)
     where a corner is excluded if either of the sides containing it are excluded.  
     *)
  val contained : (bool * bool * bool * bool) -> Plane.point * bbox -> bool

  (* computes the diameter, i.e. the length of the diagonal *)
  val diameter : bbox -> Scalar.scalar 

  (* from2Points (p1, p2) returns the smallest bounding box containing both
     p1 and p2 *)
  val from2Points : Plane.point * Plane.point -> bbox

  (* Computes the minimum bounding box of a sequence of points,
     assuming the sequence is non-empty
     *)
  val fromPoints : Plane.point Seq.seq -> bbox 

  (* Returns the four corners of the bounding box in order
     top left, top right, bottom left, bottom right  *)
  val corners : bbox -> Plane.point * Plane.point * Plane.point * Plane.point

  (* Computes the center point of the bounding box *)
  val center : bbox -> Plane.point

end
