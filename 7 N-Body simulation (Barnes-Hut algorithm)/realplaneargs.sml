structure RealPlaneArgs : PLANEARGS =
struct

  structure ScalarCore =
  struct
    datatype scalar = S of real

    fun plus (S x, S y) = S (x + y)
    fun minus (S x, S y) = S (x - y)
    fun times (S x, S y) = S (x * y)
    fun divide (S x, S y) = S (x / y)

    fun compare (S x, S y)= Real.compare (x,y)

    fun fromRatio (x : IntInf.int, y : IntInf.int) =
        S (Real.fromLargeInt x / Real.fromLargeInt y)

    fun toString (S x) =  Real.fmt (StringCvt.SCI (SOME 4)) x

    val scalar_string = "real"

    val fromReal = S

  end

  fun distance (ScalarCore.S x1, ScalarCore.S y1) (ScalarCore.S x2, ScalarCore.S y2) =
      let
        val (dx,dy) = (x2 - x1, y2 - y1)
      in
          ScalarCore.S (Math.sqrt (dx * dx + dy * dy))
      end
end

structure PlaneArgs = RealPlaneArgs