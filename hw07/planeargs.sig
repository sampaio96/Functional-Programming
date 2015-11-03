signature PLANEARGS =
sig
  structure ScalarCore : SCALARCORE
  val distance :  (ScalarCore.scalar * ScalarCore.scalar)
               -> (ScalarCore.scalar * ScalarCore.scalar)
               -> ScalarCore.scalar
end
