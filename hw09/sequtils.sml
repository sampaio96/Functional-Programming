
structure SeqUtils : SEQUTILS =
struct

    fun s2l s = Seq.mapreduce (fn x => [x]) [] op@ s
    fun seq l = List.foldr (fn (x,y) => Seq.cons x y) (Seq.empty()) l

    fun words (s : string) : string Seq.seq = 
        (* String.tokens shoudl return a sequence *)
        seq (String.tokens (fn s => s = #" ") s)

    fun words_punc (s : string) : string Seq.seq = 
        (* String.tokens shoudl return a sequence *)
        seq (String.tokens (fn s => Char.isSpace s orelse (Char.isPunct s andalso not (s = #"'"))) s)

    val explode = seq o String.explode 
    val implode = String.implode o s2l

    fun contains p = Seq.mapreduce p false (fn (x,y) => x orelse y) 

    fun strictSuffixes s = Seq.tabulate (fn i => Seq.drop (i + 1) s) ((Seq.length s) - 1)

end

functor Sort(E : ORDERED) : SORT =
struct

    structure El = E

    (* relies on the fact that Dict.toSeq is in order *)
    structure D = Dict(E)
    fun sort s = 
        let val d = Seq.mapreduce (fn x => D.insert D.empty (x, 1))
                                  D.empty
                                  (D.merge (op+)) s
        in 
            Seq.flatten (Seq.map (fn (item,multiplicity) => Seq.repeat multiplicity item)
                         (D.toSeq d))
        end
end
