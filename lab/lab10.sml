fun helper (A) = case TextIO.inputLine(A) of SOME a => a^helper(A)
                                          | NONE => ""

(* copy tasks *)
fun copy (A : TextIO.instream) (B : TextIO.outstream) =
        TextIO.output(B,helper(A))



fun copy_files (A: string) (B: string) =
  (copy (TextIO.openIn(A)) (TextIO.openOut(B)); TextIO.flushOut)



(* ---------------------------------------------------------------------- *)

(* map reduce tasks *)

signature MAP_REDUCE = 
sig
    type 'a mapreducable 
    val mapreduce : 
           ('a -> 'b)      (* handle single element *)
        -> 'b              (* result for empty *) 
        -> ('b * 'b -> 'b) (* merge results: assumed to be associative and commutative, with unit above *)
        -> 'a mapreducable -> 'b    
end

structure FileMR : MAP_REDUCE =
struct

    type 'a mapreducable = TextIO.instream
                           * (string -> 'a) (* parse a line as an 'a *)
        
    (* note: combines in reverse order, which is OK since n is commutative *)
    fun mapreduce (l : 'a -> 'b) (cur : 'b) (n : 'b * 'b -> 'b) (stream,parse) = 
          case TextIO.inputLine(stream) of
        SOME x => n ((l (parse x)),(mapreduce l cur n (stream,parse)))
      | NONE => cur

end

(*4.2*)
val numbersFromStdIn = case (TextIO.inputLine(TextIO.stdIn)) of SOME a => (case Int.fromString(a) of SOME b => b | NONE => 0) | NONE => 0


(*4.3*)
fun add x = FileMR.mapreduce 0 Int.+ x

