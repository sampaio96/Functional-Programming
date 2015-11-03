functor NaiveBayes (ClassSpec : sig
                                  structure Category : ORDERED
                                  val default_category : Category.t
                                      
                                  structure Dataset : MAP_REDUCE
                                end) : NAIVE_BAYES_CLASSIFIER =
struct

    type category = ClassSpec.Category.t

    type labeled_document = category Seq.seq * string Seq.seq
    type document = string Seq.seq

    structure Dataset = ClassSpec.Dataset
        
    (* TASK 
       make the CatEC and CatDict and WordDict modules here
       *)

    
    structure WordEC = ExtractCombine (struct structure Key = StringLt structure MR = SeqMR end)
    structure WordDict = WordEC.D

    structure CatEC = ExtractCombine (struct structure Key = ClassSpec.Category structure MR = Dataset end)
    structure CatDict = CatEC.D
    
    type counts = 
        int (* number of labeled_documents with that category *)
      * int WordDict.dict (* frequencies of words in labeled_documents with that category *)

    fun counts_documents ((n,_) : counts) : int = n
    fun counts_words     ((_,w) : counts) : int WordDict.dict = w


    (* TASK *)
    fun count_by_category (docs : labeled_document Dataset.mapreducable) : counts CatDict.dict =
        
        CatEC.extractcombine
          (fn (x,y) =>
            Seq.map (fn z =>
              (z, (1, WordEC.extractcombine (fn x => Seq.singleton((x,1))) Int.+ y))
            )
          x)

          (fn ((aa,ab),(ba,bb)) => (aa+ba, WordDict.merge Int.+ (ab,bb)))
          
          docs


    type postprocess_data =
          category Seq.seq (* list of categories (no duplicates) *)
        * int              (* total number of categorized training labeled_documents (count doc once for each label) *)
        * int              (* total number of words *)
        * int CatDict.dict (* how many words in each category? *) 

    (* TASK *) (*I did not look at the solutions*)
    fun postprocess (counts_by_category : counts CatDict.dict) : postprocess_data = 

      let val this = CatDict.toSeq(counts_by_category)
          val alsothis = CatDict.valueSeq(counts_by_category) in

        (Seq.map (fn (x,y) => x) this,
         Seq.mapreduce (fn (y,z) => y) 0 Int.+ alsothis,
         Seq.length (WordDict.toSeq (Seq.mapreduce (fn (x,y) => y)  WordDict.empty (WordDict.merge (fn (a,b) => 0)) alsothis)),
         CatDict.map (fn (e,f) => (Seq.reduce Int.+ 0 (WordDict.valueSeq(f)))) counts_by_category)

      end
        
    (* TASK *)
    fun possible_classifications 
        (counts_by_category : counts CatDict.dict)
        ((all_categories, total_num_docs, total_num_words, num_words_by_cat) : postprocess_data)
        (test_doc : document) : (category * real) Seq.seq =

        Seq.map

        (fn x => (x,
            (Math.ln (Real.fromInt(counts_documents(CatDict.lookup' counts_by_category x)) /
                      Real.fromInt(total_num_words))
            +
            Math.ln (Real.fromInt(Seq.mapreduce (


              fn a => ( let val herewegoagain = case ( WordDict.lookup (counts_words(CatDict.lookup' counts_by_category x)) a )
                  of SOME (b) => b
                   | NONE => 0
                    in herewegoagain end )


              ) 0 Int.+ test_doc) /
                     (Real.fromInt(CatDict.lookup' num_words_by_cat x)) ) )) )
        all_categories



    (* TASK *) (*I did not look at the solutions*)
    fun classify (counts_by_category : counts CatDict.dict)
                 (pp : postprocess_data)
                 (test_doc : document) : (category * real) =

                let val yeah_almostdone = (possible_classifications counts_by_category pp test_doc) in

                Seq.reduce (fn ((a,x),(b,y)) => case (x>y) of true => (a,x) | _ => (b,y)) (ClassSpec.default_category, Real.negInf) yeah_almostdone

                end
                 

    (* TASK *) (*I did not look at the solutions*)
    fun train_classifier (train : labeled_document Dataset.mapreducable) : document -> (category * real) = 
      let val yay = count_by_category(train) in
        fn x => classify yay (postprocess yay) x
      end

    (* TASK report on the percentage correct in a comment here *)
      (*small: 75%
        medium: 47% *)
end













