
signature EXTRACT_COMBINE =
sig
    structure MR : MAP_REDUCE
    
    structure D : DICT 

    val extractcombine : ('a -> (D.Key.t * 'v) Seq.seq) (* keys are not nec unique *)
                        -> ('v * 'v -> 'v) 
                        -> 'a MR.mapreducable 
                        -> 'v D.dict

end
