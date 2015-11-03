
signature MAP_REDUCE = 
sig
    type 'a mapreducable 
    val mapreduce : 
           ('a -> 'b)      (* handle single element *)
        -> 'b              (* result for empty *) 
        -> ('b * 'b -> 'b) (* merge results: assumed to be associative and commutative, with unit above *)
        -> 'a mapreducable -> 'b    
end
