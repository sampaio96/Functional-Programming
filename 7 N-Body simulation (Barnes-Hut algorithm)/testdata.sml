structure TestData : sig 
                         (* some points and bounding boxes and bodies to use for testing *)
                         val p00 : Plane.point 
                         val p44 : Plane.point 
                         val p02 : Plane.point 
                         val p24 : Plane.point 
                         val p22 : Plane.point 
                         val p20 : Plane.point 
                         val p42 : Plane.point 
                         val p01 : Plane.point 
                         val p11 : Plane.point 
                         val p40 : Plane.point 
                         val p04 : Plane.point 
                         val p13 : Plane.point 
                         val p33 : Plane.point 

                         val bb0 : BoundingBox.bbox
                         val bb1 : BoundingBox.bbox
                         val bb2 : BoundingBox.bbox
                         val bb3 : BoundingBox.bbox
                         val bb4 : BoundingBox.bbox

                         val body1 : Mechanics.body
                         val body2 : Mechanics.body
                         val body3 : Mechanics.body

                         val sun : Mechanics.body
                         val mercury : Mechanics.body
                         val venus : Mechanics.body
                         val earth : Mechanics.body
                         val mars : Mechanics.body
                         val jupiter : Mechanics.body
                         val saturn : Mechanics.body
                         val uranus : Mechanics.body
                         val neptune : Mechanics.body
                           
                         val one_body : Mechanics.body Seq.seq
                         val two_body : Mechanics.body Seq.seq
                         val solar_system : Mechanics.body Seq.seq
                   end
=
struct
  structure BB = BoundingBox

  val ++ = Plane.++
  val ** = Plane.**
  val --> = Plane.-->
  val // = Plane.//

  infixr 3 ++
  infixr 4 **
  infixr 3 -->
  infixr 3 //

  (* some points and bounding boxes and bodies to use for testing *)
  val p00 = Plane.origin
  val p44 = Plane.fromcoord (Scalar.fromInt 4, Scalar.fromInt 4)
  val p02 = Plane.fromcoord (Scalar.zero, Scalar.fromInt 2)
  val p24 = Plane.fromcoord (Scalar.fromInt 2, Scalar.fromInt 4)
  val p22 = Plane.fromcoord (Scalar.fromInt 2, Scalar.fromInt 2)
  val p20 = Plane.fromcoord (Scalar.fromInt 2, Scalar.zero)
  val p42 = Plane.fromcoord (Scalar.fromInt 4, Scalar.fromInt 2)
  val p01 = Plane.fromcoord (Scalar.fromInt 0, Scalar.fromInt 1)
  val p11 = Plane.fromcoord (Scalar.fromInt 1, Scalar.fromInt 1)
  val p40 = Plane.fromcoord (Scalar.fromInt 4, Scalar.zero)
  val p04 = Plane.fromcoord (Scalar.zero, Scalar.fromInt 4)
  val p13 = Plane.fromcoord (Scalar.one, Scalar.fromInt 3)
  val p33 = Plane.fromcoord (Scalar.fromInt 3, Scalar.fromInt 3)

  val bb0 : BB.bbox = BB.from2Points (p02,p24)
  val bb1 : BB.bbox = BB.from2Points (p22,p44)
  val bb2 : BB.bbox = BB.from2Points (p00,p22)
  val bb3 : BB.bbox = BB.from2Points (p20,p42)
  val bb4 : BB.bbox = BB.from2Points (p00,p44)

  val body1 : Mechanics.body = (Scalar.one, p40, Plane.zero)
  val body2 : Mechanics.body = (Scalar.one, p22, Plane.zero)
  val body3 : Mechanics.body = (Scalar.one, p04, Plane.zero)


  (* rather inefficient. *)
  structure II = IntInf
  fun pow (f : II.int, 0) = 1
    | pow (f, n) = f * pow (f, n-1)

  (* FIXME: uses floating point arithmetic *)
  fun digits x = Real.floor (Math.log10 (Real.fromLargeInt (II.abs x)))

  (* parses a pair in "scientific notation" into a Plane.scalar
   * eg sci (412, 13) == 4.12E13
   *)
  fun sci (0, _) = Scalar.zero
    | sci (f : II.int, exp : int) =
      let
        val digs = digits f
        val f = Scalar.fromRatio (f, pow (10, digs))
        val order = Scalar.pow (Scalar.fromInt 10, exp)
      in
        Scalar.times (f, order)
      end

  val zero : II.int * int = (0,0)

  fun I x = (x, digits x)

  fun b (mass : II.int * int) (dist : II.int * int) (vel : II.int * int) =
      (sci mass,
       Plane.fromcoord (Scalar.zero, sci dist),
       Plane.--> (Plane.origin, Plane.fromcoord (sci vel, Scalar.zero)))

  (* astronomical constants sourced from Wikipedia, the font of all knowledge *)

  val sun = b (198892, 30) zero zero              (* sun *)
  val mercury = b (33022, 23) (579091, 10) (I 47870)  (* mercury *)
  val venus = b (48685, 24) (~10820893, 11) (I ~35020)    (* venus *)
  val earth = b (59736, 24) (~14960, 11) (I ~29780)    (* earth *)
  val mars = b (64185, 23) (2279391, 11) (I 24077)     (* mars *)
  val jupiter = b (18986, 27) (7785472, 11)  (I 13070)  (* jupiter *)
  val saturn = b (56846, 26) (~143344937, 12) (~969, 3)   (* saturn *)
  val uranus = b (8681, 25)  (2876679082, 12)  (681, 3)    (* uranus *)
  val neptune = b (10243, 26) (~4503443661, 12) (~543, 3)  (* neptune *)

  val one_body = Seq.singleton sun
  val two_body = Seq.cons sun (Seq.cons earth (Seq.empty()))
  val solar_system = 
      List.foldr (fn (x,y) => Seq.cons x y) (Seq.empty())
      [sun, mercury, venus, earth, mars, jupiter, saturn, uranus, neptune]

end
