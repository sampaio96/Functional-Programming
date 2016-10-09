(* ---------------------------------------------------------------------- *)
(* BMP writing code *)
local 
    (* http://www.soc.napier.ac.uk/~cs66/course-notes/sml/writebmp.txt *)

    fun getInt(s,i) = Word8.toInt(Word8Vector.sub(s,i));
    fun get2(s,i)=getInt(s,i)+256*getInt(s,i+1);
    fun get4(s,i)=get2(s,i)+256*256*get2(s,i+2);
    fun ttt 0 = 1|ttt 1= 2|ttt 2=  4|ttt 3=8|ttt 4=16|ttt 5=32|ttt 6=64|ttt 7=128|ttt 8=256|ttt _ = raise Fail "unimplemented"
    fun bits(b,s,n)=((b div ttt(s*n)) mod (ttt n));
    fun pad4 x = ~4*(~x div 4)

    fun readbmp s = let
                        val input = BinIO.inputN
                        val fh = BinIO.openIn s;
                        val header = input(fh,54)
                        val bitspp = get2(header,28)
                        val colTab = input(fh,get4(header,10) - 54)
                        val bitMap = input(fh,get4(header,34))
                        val w = get4(header,18)
                        val h = get4(header,22)
                        val dy = pad4((bitspp * w) div 8)
                        fun outrange(x,y)=x<0 orelse y<0 orelse x>=w orelse y>=h
                        val r = 8 div bitspp
                        fun col(x,y) = if outrange(x,y) then 0 else
                            bits(getInt(bitMap,dy*y+(x div r)),r-1-(x mod r),bitspp)
                    in (w,h,colTab,col) 
                    end

    fun mk2 v = Word8Vector.fromList[Word8.fromInt(v mod 256), Word8.fromInt(v div 256 mod 256)];
    fun mk4 v = Word8Vector.concat[mk2 (v mod 65536),mk2 (v div 65536)];
    fun upto n m = if n=m then [n] else n::(upto (n+1) m);
    fun header w h c = let
                           val size = Word8Vector.length in
                               Word8Vector.concat[
                                                  Word8Vector.fromList(map(Word8.fromInt o ord) [#"B",#"M"]),
                                                  mk4((pad4 w)*h+size c+54),
                                                  mk4 0, mk4 (size c+54), mk4 40,
                                                  mk4 w,	mk4 h,	mk2 1,	mk2 8,	mk4 0,	mk4 ((pad4 w)*h),
                                                  mk4 0,	mk4 0,	mk4 256,	mk4 256] end;    

    (* width, height, colour table, (x,y)->pixel value, filename *)
    fun writebmp (w,h,c,t) s = let
                                   val fh = BinIO.openOut s
                                   val waste=BinIO.output(fh,header w h c)
                                   val waste'=BinIO.output(fh,c)
                                   val padw = pad4 w
                                   fun f i = Word8.fromInt(t(i mod padw, i div padw))
                                   val waste'' = Word8Vector.tabulate(padw*h,f)
                               in BinIO.output(fh,waste'');BinIO.closeOut fh; s 
                               end

        (* FIXME: based on my reading of the bitmap format, I think there should be a way to just 
           have two entries, but the following doesn't display:
           Word8Vector.fromList [0wx0,0wx0,0wx0,0wx0,0wxFF,0wxFF,0wxFF,0wx00]; 
           so I grabbed a colormap with both black and white from some bmp *)
    val colormap = 
        Word8Vector.fromList
  [0wx0,0wx0,0wx0,0wx0,0wxFF,0wxFF,0wxFF,0wx0,0wx82,0wx82,0wxFE,0wx0,0wxC8,
   0wxC8,0wxFF,0wx0,0wx45,0wx45,0wx86,0wx0,0wxC5,0wxCA,0wx7A,0wx0,0wxE6,0wxE8,
   0wxC5,0wx0,0wx68,0wx6B,0wx40,0wx0,0wxD5,0wxA0,0wx78,0wx0,0wxE1,0wxE1,0wxE1,
   0wx0,0wxE2,0wxD5,0wxC6,0wx0,0wxB5,0wx99,0wx8B,0wx0,0wxB2,0wxB2,0wxB2,0wx0,
   0wxC0,0wxC0,0wxC0,0wx0,0wx65,0wx65,0wx65,0wx0,0wx97,0wx97,0wx97,0wx0,0wxEE,
   0wxEE,0wxEE,0wx0,0wxCC,0wxCC,0wxCC,0wx0,0wx99,0wx0,0wx33,0wx0,0wxAA,0wxAA,
   0wxAA,0wx0,0wx88,0wx88,0wx88,0wx0,0wx77,0wx77,0wx77,0wx0,0wxCC,0wx66,0wx66,
   0wx0,0wxDD,0wx0,0wx0,0wx0,0wx55,0wx55,0wx55,0wx0,0wxFF,0wx99,0wx99,0wx0,
   0wxBB,0wxBB,0wxBB,0wx0,0wxFF,0wxCC,0wxCC,0wx0,0wx11,0wx11,0wx11,0wx0,0wxE7,
   0wxE7,0wxE7,0wx0,0wx6F,0wx6F,0wx6F,0wx0,0wxFF,0wx99,0wx0,0wx0,0wx0,0wxFF,
   0wxFF,0wx0,0wxB5,0wxB5,0wx69,0wx0,0wx0,0wxFF,0wx0,0wx0,0wx0,0wx0,0wxFF,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,
   0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0,0wx0]
in 

    fun write_bitmap (width:int,height:int,isBlack : int * int -> bool, filename : string) : unit
        = 
        let val _ = writebmp (width,height,colormap, 
                              fn (x,y) => case isBlack(x,y) of true => 0 | false => 1) filename
        in ()
        end
end

