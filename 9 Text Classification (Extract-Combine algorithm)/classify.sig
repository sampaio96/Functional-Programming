signature CLASSIFIER = 
sig
    structure Dataset : MAP_REDUCE

    type category

    type document = string Seq.seq
    type labeled_document = category Seq.seq * document
        
    val train_classifier : labeled_document Dataset.mapreducable 
                         -> document -> (category * real)
end

signature NAIVE_BAYES_CLASSIFIER = 
sig
    structure Dataset : MAP_REDUCE

    type category

    type labeled_document = category Seq.seq * string Seq.seq
    type document = string Seq.seq
        
    val train_classifier : labeled_document Dataset.mapreducable -> document -> (category * real)


    (* ---------------------------------------------------------------------- *)
    (* internal components that are exported only for testing *)

    structure WordDict : DICT
    structure CatDict : DICT


    type counts =   int (* number of labeled_documents with that category *)
                  * int WordDict.dict (* frequencies of words in labeled_documents with that category *)

    val count_by_category : labeled_document Dataset.mapreducable -> counts CatDict.dict 

    type postprocess_data =
          category Seq.seq (* list of categories (no duplicates) *)
        * int              (* total number of categorized training labeled_documents (count doc once for each label) *)
        * int              (* total number of words *)
        * int CatDict.dict (* how many words in each category? *) 

    val postprocess : counts CatDict.dict -> postprocess_data 


    val possible_classifications : counts CatDict.dict -> postprocess_data -> document -> (category * real) Seq.seq
    val classify : counts CatDict.dict -> postprocess_data -> document -> category * real

end 

