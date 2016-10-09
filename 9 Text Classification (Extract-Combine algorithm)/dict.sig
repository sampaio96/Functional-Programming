
(* there is a 

   functor Dict(K : ORDERED) : DICT 

*)

signature DICT =
sig
  structure Key : ORDERED
  type 'v dict 

  val empty   : 'v dict
  val insert  : 'v dict -> (Key.t * 'v) -> 'v dict

  val lookup  : 'v dict -> Key.t -> 'v option
  (* assumes key is in the dictionary *)
  val lookup' : 'v dict -> Key.t -> 'v 

  val map     : ('a -> 'b) -> 'a dict -> 'b dict

  (* merge combine (d1,d2) == d where
     - k in d if and only if k is in d1 or k is in d2
     - If k~v in d1 and k is not in d2, then k ~ v in d
     - If k~v in d2 and k is not in d1, then k ~ v in d
     - If k~v1 in d1 and k~v2 in d2, then k ~ combine (v1, v2) in d
     *)
  val merge  : ('v * 'v -> 'v) -> 'v dict * 'v dict -> 'v dict
      
  val fromSeq :  (Key.t * 'v) Seq.seq -> 'v dict (* for duplicates, earlier keys win *)

  (* computes the sequence of all (key,value) pairs in the dictionary,
     ordered from smallest key to largest key
     *)
  val toSeq : 'v dict -> (Key.t * 'v) Seq.seq  

  (* computes the sequence of all values in the dictionary *)
  val valueSeq : 'v dict -> 'v Seq.seq  

  (* extract the maximum key/value pair in the dictionary,
     or returns NONE if dictionary is empty *)
  val max : 'v dict -> (Key.t * 'v) option

end



