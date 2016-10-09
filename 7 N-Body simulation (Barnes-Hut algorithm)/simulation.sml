structure Simulation :
sig 
    val run : 
        (Mechanics.body Seq.seq -> Plane.vec Seq.seq) (* accel *)
        -> string        (* header *)
        -> Mechanics.body Seq.seq (* bodies *)
        -> Scalar.scalar (* timestep *)
        -> int           (* number of iterations *)
        -> string        (* output file; WARNING: overwritten *)
        -> unit
end =
struct
  open Mechanics

  (* t is the timestep *)
  fun stepGen (accs : body Seq.seq -> Plane.vec Seq.seq)
      (bodies : body Seq.seq , t : Scalar.scalar) : body Seq.seq =
      Seq.map (fn (b,a) => stepBody (b,a,t))
              (Seq.zip (bodies, accs bodies))

  val bodySeqToString =
      String.concatWith "," o
      Seq.mapreduce (fn x => [Plane.pointToString (position x)]) [] op@

  fun output_coordinates (s : body Seq.seq) (t : Scalar.scalar) (niters : int)
                         (accs : body Seq.seq -> Plane.vec Seq.seq)
                         (outfile : TextIO.outstream) : unit =
      case niters of 
          0 => ()
        | _ => (TextIO.output (outfile, bodySeqToString s);
                TextIO.output (outfile, "\n");
                output_coordinates (stepGen accs (s, t)) t (niters - 1) accs (outfile))

  fun seqFromList l = foldr (fn (x,y) => Seq.cons x y) (Seq.empty ()) l

  (* FIXME: toString for metrics may not be real-formatted.
   * Need to standardize on formatting.
   *)
  fun run (accs : body Seq.seq -> Plane.vec Seq.seq)
          (header : string)
          (s : body Seq.seq)
          (t : Scalar.scalar)
          (niters : int)
          (file : string) : unit =
      let
          val outfile = TextIO.openOut file
      in
          (TextIO.output (outfile, header);
           output_coordinates s t niters accs outfile;
           TextIO.flushOut outfile;
           TextIO.closeOut outfile)
      end

end
