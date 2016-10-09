
functor ExtractCombine (A : sig 
                                structure Key : ORDERED
                                structure MR : MAP_REDUCE
                            end) : EXTRACT_COMBINE =
struct

	structure MR = A.MR
	structure D = Dict(A.Key)

	fun extractcombine f g a =
		MR.mapreduce (fn x => D.fromSeq(f(x))) D.empty (fn (y1,y2) => D.merge g (y1,y2)) a
end