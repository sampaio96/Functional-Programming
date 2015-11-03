structure RatPlaneArgs : PLANEARGS =
struct

  structure ScalarCore : SCALARCORE  =
  struct
    open IntInf

    datatype rat = Frac of int * int
    type scalar = rat

    fun gcd (m, 0) = m
      | gcd (0, n) = n
      | gcd (m, n) = gcd (if m > n then (m mod n, n) else (m, n mod m))

    fun lcm (m : int, n : int) : int =
        let
          val g = gcd (m, n)
        in
          m * n div g
        end

    fun fromRatio (n : int, d : int) : scalar =
        let
          val g =
              case (IntInf.compare (n, 0), IntInf.compare (d, 0)) of
                  (_, EQUAL) => raise Fail "denominator can't be zero"
                | (EQUAL, _) => d
                | (LESS, GREATER) => gcd (~n, d)
                | (GREATER, LESS) => ~(gcd (n, ~d))
                | (LESS, LESS) => ~(gcd (~n, ~d))
                | (GREATER, GREATER) => gcd (n, d)
        in
          Frac (n div g, d div g)
        end

    fun plus (Frac (n1, d1) : scalar, Frac (n2, d2) : scalar) : scalar =
        let
          val cdenom = lcm (d1, d2)
        in
          Frac (n1 * cdenom div d1 + n2 * cdenom div d2, cdenom)
        end

    fun times (Frac (n1, d1) : scalar, Frac (n2, d2) : scalar) : scalar =
        fromRatio (n1 * n2, d1 * d2)

    fun minus (r1 : scalar, r2 : scalar) : scalar =
        plus (r1, times(fromRatio(~1,1),r2))

    fun divide (r1 : scalar, r2 : scalar) : scalar =
        let
          fun inverse (Frac (n, d) : scalar) : scalar =
              case IntInf.compare (n, 0) of
                  LESS => Frac (~d, ~n)
                | GREATER => Frac (d, n)
                | EQUAL => raise Div
        in
          times (r1, inverse r2)
        end

    fun compare (Frac (n1, d1) : scalar, Frac (n2, d2) : scalar) : order =
        let
          val cdenom = lcm (d1, d2)
        in
          IntInf.compare (n1 * cdenom div d1, n2 * cdenom div d2)
        end

    fun toString (Frac (n, 1)) = IntInf.toString n
      | toString (Frac (n, d)) = IntInf.toString n ^ "/" ^ IntInf.toString d


    val scalar_string = "rat"

    fun fromReal x = 
        let val {whole,frac} = Real.split x
        in 
            plus (fromRatio(IntInf.fromInt (Real.round whole), 1),
                  raise Fail "rat fromReal not yet implemented: do something with the frac")
        end
        
  end

  (* taxi distance *)
  fun distance (x1,y1) (x2,y2) =
      let
        fun abs x =
            case ScalarCore.compare (x, ScalarCore.fromRatio(0,1))
             of LESS => ScalarCore.minus(ScalarCore.fromRatio(0,1), x)
              | _ => x

        val dx = ScalarCore.minus (x2,x1)
        val dy = ScalarCore.minus (y2,y1)
      in
        ScalarCore.plus (abs dx,  abs dy)
      end
end

structure PlaneArgs = RatPlaneArgs