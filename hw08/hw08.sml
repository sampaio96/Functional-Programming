
signature PIXEL =
sig

    type pixel

    (* show_pixel p returns the (alpha,red,green,blue) 
       values of the pixel.
       
       If show_pixel p == (a,r,g,b) then 
       all four numbers are in the range the range [0,256) *)
    val show_pixel : pixel -> int * int * int * int

    (* Make a pixel from its (alpha,red,green,blue) values.
       Assumes the integers are in the range [0,256) *)
    val make_pixel : int * int * int * int -> pixel

end

structure PairPixel : PIXEL =
struct

    datatype pixel = P of int * int * int * int

    fun make_pixel x = P x

    fun show_pixel (P (a,r,g,b)) = (a,r,g,b)

end

structure Word32Utils : 
sig 
    val << : Word32.word * int -> Word32.word
    val >> : Word32.word * int -> Word32.word
end =
struct
    infixr 1 << 
    infixr 1 >>
    fun x << y = Word32.<<(x,Word31.fromInt y)
    fun x >> y = Word32.>>(x,Word31.fromInt y)
end
(* to use these infix in your code, do

   open Word32Utils
   infixr 1 << 
   infixr 1 >>

   in your module.

   or ignore this and just write Word32Utils.<<(a,b)
*)

structure WordPixel : PIXEL =
struct

    open Word32Utils
    infixr 1 << 
    infixr 1 >>

    datatype pixel = P of Word32.word

    fun make_pixel (a:int,r,g,b) = P ((Word32.fromInt(a) << 24) + (Word32.fromInt(r) << 16) + (Word32.fromInt(g) << 8) + Word32.fromInt(b))

    fun show_pixel (P x) = (Word32.toInt((x >> 24)), Word32.toInt((x >> 16) mod Word32.fromInt(256)), Word32.toInt((x >> 8) mod Word32.fromInt (256)), Word32.toInt(x mod Word32.fromInt (256)))

end

functor Test(P : PIXEL) =
struct

    fun test_equal (a,r,g,b) = 
        let 
            val (a',r',g',b') = P.show_pixel (P.make_pixel(a, r, g, b))
        in
            (a' = a) andalso (r' = r) andalso (g' = g) andalso (b' = b) 
        end

    val true = test_equal(255,255,255,255) 
    val true = test_equal(0,0,0,0) 
    val true = test_equal(1,2,3,4) 
    val true = test_equal(45,127,192,243) 

end

structure TestPair = Test(PairPixel)
structure TestWord = Test(WordPixel)

signature IMAGE_TRANSFORMATIONS =
sig
    structure P : PIXEL

    val remove_red : P.pixel Seq.seq -> P.pixel Seq.seq
end

functor Images(P : PIXEL) : IMAGE_TRANSFORMATIONS =
struct

    structure P = P

    fun remove_red s = Seq.map (fn x => let val (a,r,g,b) = P.show_pixel(x) in P.make_pixel(a,0,g,b) end) s

end

functor RemoveRedTest(I : IMAGE_TRANSFORMATIONS) = 
struct

    val pixels = Seq.cons (I.P.make_pixel (0xFF,0xAB,0xCD,0x24))
                   (Seq.cons (I.P.make_pixel (0x12,0x34,0x45,0x9A)) (Seq.empty()))

    val removed = I.remove_red pixels

    val (0xFF,0,0xCD,0x24) = I.P.show_pixel (Seq.nth 0 removed)
    val (0x12,0,0x45,0x9A) = I.P.show_pixel (Seq.nth 1 removed)

    val () = print "remove red tests passed"

end

structure TestImages = RemoveRedTest(Images(PairPixel))