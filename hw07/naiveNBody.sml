structure NaiveNBody : NBODY =
struct
  open Mechanics

  (* This is the quadratic pairwise computation of the acceleration vectors
   * from lecture *)
  fun accelerations (bodies : body Seq.seq) : Plane.vec Seq.seq =
      Seq.map (fn b1 => Plane.sum (fn b2 => accOn (b1, b2)) bodies) bodies
end
