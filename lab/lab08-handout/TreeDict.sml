structure TreeDict:LABDICT = 
 struct
 	
 	datatype ('k, 'v) tree =
		Leaf
	  | Node of ('k, 'v) tree * ('k * 'v) * ('k, 'v) tree

	type ('k, 'v) dict = ('k, 'v) tree

	val empty = Leaf

  	fun insert (f : 'k * 'k -> order) (d : ('k, 'v) dict) (a:'k, b: 'v) : ('k, 'v) dict =
  		case d of
  			Leaf => Node(Leaf,(a,b),Leaf)
  		  | Node (l,(x,y),r) => case f(a,x) of
  		  							LESS => Node ((insert f l (a,b)),(x,y),r)
  		  						  | EQUAL => Node (l,(x,b),r)
  		  						  | GREATER => Node (l,(x,y),(insert f r (a,b)))


	fun lookup (f : 'k * 'k -> order) (d: ('k, 'v) dict) (a: 'k) : 'v option =
		case d of
			Leaf => NONE
		  | Node (l, (x,y), r) => case f(a,x) of
		  							LESS => lookup f l a
  		  						  | EQUAL => SOME y
  		  						  | GREATER => lookup f r a
 end