
structure WordFreq =
struct
    structure EC = ExtractCombine(struct structure Key = StringLt
                                         structure MR = SeqMR
                                  end)
    
    fun wordCounts (d : string Seq.seq) : int EC.D.dict =
        EC.extractcombine (fn s => Seq.map (fn w => (w, 1)) (SeqUtils.words s))
                          (op+) 
                          d

    (* convert the output to a key-value list for easy printing *)
    fun wordCounts_list (d : string Seq.seq) : (string * int) list = 
        SeqUtils.s2l (EC.D.toSeq (wordCounts d))

(*
    uncomment this to test your extract_combine

    val [("1",1),("2",1),("document",2),("is",3),("this",2)] = 
        wordCounts_list (SeqUtils.seq ["this is is document 1",
                                       "this is document 2"])
*)
end
