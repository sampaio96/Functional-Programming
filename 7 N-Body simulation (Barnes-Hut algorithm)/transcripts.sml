structure Transcripts : 
sig
    (* run_solar days outfilename 
       generates a transcript for running the solar system for that many days
       in tests/outfilename. 
       *)
    val run_solar : int -> string -> unit

    (* run_solar_inner days outfilename 
       generates a transcript for running the solar system for that many days
       in tests/outfilename. the visualization radius shows only planets up to mars. *)
    val run_solar_inner : int -> string -> unit

    (* produces transcripts for a variety of small examples with one and two bodies and the solar system.
       examples are small enough to be used with exact rational numbers for testing *)
    val run_tests : unit -> unit

        
    (* run_file filename num_iters timestep 
       runs the simulation on an input file specified by filename, for 
       num_iters steps, with time given by Scalar.fromRatio timestep.
       output is written to filename.sim.
       
       For example:
          run_file "data/galaxy2.txt" 2000 (1,10); 
       Note: this will take a couple of minutes.  
          
       See data/datafiles.txt and the definition of run_files below
       for suggestions of what to run.
       *)
    val run_file : string -> int -> IntInf.int * IntInf.int -> unit

    (* runs some simulations on all of the files.
       Warning: may take a few hours. *)
    val run_files : unit -> unit

    val time : (unit -> 'a) -> {gcsys : IntInf.int, gcusr : IntInf.int, ngcsys : IntInf.int, ngcusr : IntInf.int} * 'a 

    (* transcript file format:
       
       float (planet size)
       float (viewing box radius)
       color1,color2,color3,...,colorn (colors of each body)
       (x11,y11),(x21,y21),(x31,y31),... (positions at begining)
       (x11,y11),(x21,y21),(x31,y31),... (positions at next timestep)
       ...

       *)
end
=
struct

    fun fmtcolor (x : int) : string = 
        (case x <= 15 of true => "0" | false => "") ^ Int.fmt StringCvt.HEX x
             
    fun rgb (r : int, g : int, b : int) : string = 
        "#" ^ fmtcolor r ^ fmtcolor g ^ fmtcolor b

    structure Test =
    struct

        val solar_colors = ["#FFFF00","#00FF00","#FF00FF","#0032FF","#FF0000",
                            rgb(123,109,99),rgb(189,159,133),rgb(194,234,237),rgb(91,142,230)]
            
        val solar_radius = "5E12\n" (* radius *)
        val small_radius = "2.5E11\n" (* up to mars *)

        val planet_size = "10\n"

        val timestep_1day = (Scalar.fromInt 86400) 
        val timestep_10days = (Scalar.fromInt 864000) 

        (* assumes length of bod <= 9 *)
        fun run_test(bod : Mechanics.body Seq.seq) (num_iters : int) (timestep : Scalar.scalar) (view_radius : string) (outfilename : string) : unit =
            let
                val header = planet_size
                           ^ view_radius
                           ^ String.concatWith "," (List.take (solar_colors, Seq.length bod)) ^ "\n"

                val () = print ("made " ^ ("tests/" ^ outfilename) ^ " ...")
                val t1 = Time.now ();
                val () = Simulation.run BarnesHut.accelerations header bod timestep num_iters ("tests/" ^ outfilename)
                val t2 = Time.now ();
                val delta = Time.-(t2,t1)
                val () = print (" in " ^ Time.toString delta ^ " seconds\n")
            in
                ()
            end

        (* run_tests runs barneshut with various subsets of the solar system and
           produces transcript files in the "tests" subdirectory of the current working directory.
           these canned examples are small enough that an arbitrary precision
           rational implementation of the universe should allow BH to
           terminate on them with in a few minutes *)
        fun run_tests () =
            let
                
                (* helpers to make input *)
                fun prep solars bef (x,y) =
                    (x,
                     solars,
                     String.concatWith "." [Scalar.scalar_string, bef, y, "auto", "txt"])
                    
                fun days (x : int) = (x+1, (Int.toString(x+1)) ^ "day")
                    
                val onebody_list = map (prep TestData.one_body "onebody") [(1,"1day"), (14,"2weeks"), (365,"1yr")]
                    
                val twobody_list = map (prep TestData.two_body "twobody") (List.tabulate(6,days))

                val system_list = map (prep TestData.solar_system "system") (List.tabulate(2,days))

                val all = onebody_list @ twobody_list @ system_list
            in
                (* actually run BH on the input and make transcripts *)
                List.app (fn (numdays, bods, outfilename) => run_test bods numdays timestep_10days solar_radius outfilename) all
            end
    end

    structure File = 
    struct
        fun parse_file (s : string) : string * (Mechanics.body * (int * int * int)) Seq.seq = 
            let val f = TextIO.openIn s
                val _ = TextIO.inputLine f (* skip number of bodies *)
                val radius = (case TextIO.inputLine f of
                                  SOME line => line
                                | NONE => raise Fail "expected radius line") 
                    
                fun parseLine (l : string) = 
                    case String.tokens Char.isSpace l of
                        [pxs , pys, vxs, vys, ms, rs, gs, bs] => 
                            (case (Real.fromString pxs, Real.fromString pys,
                                   Real.fromString vxs, Real.fromString vys,
                                   Real.fromString ms,
                                   Int.fromString rs, Int.fromString gs, Int.fromString bs) of
                                 (SOME px, SOME py, SOME vx, SOME vy, SOME m, SOME r, SOME g, SOME b) =>
                                     ((Scalar.fromReal m,
                                       Plane.fromcoord(Scalar.fromReal px, Scalar.fromReal py),
                                       Plane.-->(Plane.origin, Plane.fromcoord(Scalar.fromReal vx, Scalar.fromReal vy))),
                                      (r,g,b))
                               | _ => raise Fail ("wrong kind of thing on line: " ^ l))
                      | _ => raise Fail ("wrong number of things on line: " ^ l)
                                 
                fun loop (bodies : (Mechanics.body * (int * int * int)) Seq.seq) : (Mechanics.body * (int * int * int)) Seq.seq = 
                    case TextIO.inputLine f of
                        NONE => bodies
                      | SOME line => loop (Seq.cons (parseLine line) bodies)
            in
                (radius, loop (Seq.empty()))
            end
      
        fun run_file (input : string) (num_iters : int) (timestep_ratio : IntInf.int * IntInf.int): unit = 
            let 
                val () = print ("running " ^ input ^ "\n")
                val startt = Time.now()
      
                val (view_radius, color_bodies) = parse_file input 
                val color_string = String.concatWith "," (Seq.mapreduce (fn (_,c) => [rgb c]) [] (op@) color_bodies)
                    
                val header = "0.25\n" (* planet display size *)
                            ^ view_radius
                            ^ color_string
      
                val bodies = Seq.map (fn (b,_) => b) color_bodies
                    
                val timestep = Scalar.fromRatio timestep_ratio
      
                val () = Simulation.run BarnesHut.accelerations header bodies timestep num_iters (input ^ ".sim")
      
                val endt = Time.now ();
                val () = print (" in " ^ Time.toString (Time.-(endt,startt)) ^ " seconds\n")
            in 
                ()
            end
      
        (* run all the data files with some suggested parameters.
           each call is labeled with how long it took on my laptop, 
           so you have an idea for what to expect.  you can always 
           run for less time, but you won't see as much of the movie :)

           the total size of all generated data is about 2GB. 
           *)
        fun run_files () : unit =
            (run_file "data/test3.txt" 1000 (1,10);
             run_file "data/test1.txt" 1000 (1,10);
             run_file "data/test5.txt" 1000 (1,10);
             run_file "data/test4.txt" 1000 (1,10);
             run_file "data/test2.txt" 1000 (1,10);
             run_file "data/planets.txt" 1000 (1,10);
             run_file "data/asteroids1000.txt" 1000 (1,10); (* 62 seconds*)
             run_file "data/cluster2582.txt" 2000 (1,10); (* 555 seconds *)
             run_file "data/galaxy1.txt" 2000 (1,10); (* 130 seconds *)
             run_file "data/galaxy2.txt" 2000 (1,10); (* 83 seconds *)
             run_file "data/galaxy3.txt" 1500 (1,10); (* 304 seconds *)
             run_file "data/galaxy4.txt" 2000 (1,10); (* 41 seconds *)
             run_file "data/spiralgalaxy.txt" 2000 (1,10); (* 94 seconds *)
             run_file "data/galaxymerge1.txt" 5000 (1,5); (* 820 seconds *)
             run_file "data/galaxymerge2.txt" 2500 (1,10); (* 626 seconds *)
             run_file "data/galaxymerge3.txt" 2500 (1,10); (* 655 seconds *)
             run_file "data/galaxyform2500.txt" 2000 (1,10); (* 294 seconds *)
             run_file "data/collision2.txt" 2500 (1,10); (* 330 seconds *)
             run_file "data/collision1.txt" 1500 (1,10); (* 299 seconds *)
             run_file "data/saturnrings.txt" 100 (1,100); (*  112 seconds *)
             run_file "data/galaxy10k.txt" 100 (1,10); (* 93 seconds *)
             run_file "data/galaxy20k.txt" 50 (1,10); (* 188 seconds *)
             run_file "data/galaxy30k.txt" 800 (1,10) (* 1736 seconds *)
             )
      end

    fun run_solar (days : int) (outfilename : string) : unit =
        Test.run_test TestData.solar_system days Test.timestep_10days Test.solar_radius outfilename;

    fun run_solar_inner (days : int) (outfilename : string) : unit =
        Test.run_test TestData.solar_system days Test.timestep_1day Test.small_radius  outfilename;

    val run_tests = Test.run_tests

    val run_file = File.run_file
    val run_files = File.run_files

    fun time (f : unit -> 'a) : {gcsys : IntInf.int, gcusr : IntInf.int, ngcsys : IntInf.int, ngcusr : IntInf.int} * 'a = 
        let val t = Timer.startCPUTimer ()
            val a = f ()
            val {gc={sys=gcsys,usr=gcusr},
                 nongc={sys=ngcsys,usr=ngcusr}} = Timer.checkCPUTimes t
        in
            ({gcsys = Time.toSeconds gcsys, 
              gcusr = Time.toSeconds gcusr,
              ngcsys = Time.toSeconds ngcsys,
              ngcusr = Time.toSeconds ngcusr}, a)
        end

end
