
structure SeqMR : MAP_REDUCE =
struct
    type 'a mapreducable = 'a Seq.seq
    fun mapreduce l e n = Seq.mapreduce l e n
end

structure FileMR : MAP_REDUCE =
struct

    type 'a mapreducable = TextIO.instream
                           * (string -> 'a) (* parse a line as an 'a *)
        
    (* note: combines in reverse order, which is OK since n is commutative *)
    fun mapreduce (l : 'a -> 'b) (e : 'b) (n : 'b * 'b -> 'b) (stream,parse) = 
        let val c = ref 0

            fun loop (cur : 'b) : 'b =
                case TextIO.inputLine stream of
                    NONE => cur
                  | SOME line => (c := !c + 1; 
                                  (case !c mod 1000 = 0 of 
                                      true => print ("Progress: reading document " ^ Int.toString (!c) ^ "\n")
                                    | false => ());
                                  loop (n (l (parse line), cur)))
        in
            loop e
        end

end
