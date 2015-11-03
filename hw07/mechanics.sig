signature MECHANICS =
sig

  (* we represent bodies as (mass, location, velocity) *)
  type body = Scalar.scalar * Plane.point * Plane.vec

  val position : body -> Plane.point
  val bodyToString : body -> string

  (* universal gravitational constant for this universe *)
  val G : Scalar.scalar

  val accOnPoint : Plane.point * (Scalar.scalar * Plane.point) -> Plane.vec
  val accOn : body * body -> Plane.vec
  val stepBody : body * Plane.vec * Scalar.scalar -> body
end
