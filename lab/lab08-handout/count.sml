structure Count =
struct

    (* break a string into a list of words*)
    fun words (s : string) = (String.tokens Char.isSpace s)

    (* derived form: update a key if it is already there *)
    fun increment (d : (string,int) TreeDict.dict) (s : string) : (string,int) TreeDict.dict = 
        case (lookup (fn (x,y) => Int.compare (x, y)) d  s) of
            NONE => insert (fn (x,y) => Int.compare (x, y)) d (s, 1)
            SOME of valueyouwant => insert (fn (x,y) => Int.compare (x, y)) d (s, valueyouwant)


    fun count (tweets : string list) (d : (string,int) TreeDict.dict) : (string,int) TreeDict.dict = 
        raise Fail "unimplemented"

    (* uncomment to test
    val test_dict = count ["hi there", "how are you today", "I'm fine how are you"] TreeDict.empty
    val SOME 1 = TreeDict.lookup String.compare test_dict "hi";
    val SOME 1 = TreeDict.lookup String.compare test_dict "there";
    val SOME 2 = TreeDict.lookup String.compare test_dict "how";
    val SOME 2 = TreeDict.lookup String.compare test_dict "are";
    val SOME 2 = TreeDict.lookup String.compare test_dict "you";
    val SOME 1 = TreeDict.lookup String.compare test_dict "today";
    val SOME 1 = TreeDict.lookup String.compare test_dict "I'm";
    val SOME 1 = TreeDict.lookup String.compare test_dict "fine";
    *)

end