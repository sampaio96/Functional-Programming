
functor TestClassify(Dataset : MAP_REDUCE) 
  : 
  sig
      type labeled_document
      type document 
      val print_counts_by_category : labeled_document Dataset.mapreducable -> unit
      val print_postprocess        : labeled_document Dataset.mapreducable -> unit
      val print_possibles          : labeled_document Dataset.mapreducable -> labeled_document -> unit
      val number_correct           : labeled_document Dataset.mapreducable -> labeled_document Dataset.mapreducable -> int * int
      val print_predictions        : labeled_document Dataset.mapreducable -> labeled_document Dataset.mapreducable -> unit
  end
  =
struct
    structure C = NaiveBayes(struct structure Category = StringLt
                                    val default_category = "unknown"
                                    structure Dataset = Dataset
                             end)

    type labeled_document = C.labeled_document
    type document = C.document

    fun print_counts_by_category (train : C.labeled_document Dataset.mapreducable) : unit =
        let 
            val cc = C.count_by_category train
            val ccseq = C.CatDict.toSeq cc
                
            val _ = 
              print (Seq.mapreduce (fn (cat, (num_docs, word_dict)) =>
                                     (("Category: " ^ cat ^ "\n") ^
                                      ("  Num docs:" ^ Int.toString num_docs ^ "\n") ^ 
                                      Seq.mapreduce (fn (word,count) => 
                                                     ("    " ^ word ^ " " ^ Int.toString count ^ "\n"))
                                                    "" (op^) 
                                                    (C.WordDict.toSeq word_dict)))
                                   "" (op^)
                                   ccseq)
        in 
            ()
        end

    fun print_postprocess (train : C.labeled_document Dataset.mapreducable) : unit =
        let 
            val (all_categories, total_num_docs, total_num_words, num_words_by_cat) = C.postprocess (C.count_by_category train)

            val () = print ("All categories: " ^ Seq.reduce (fn (c1,c2) => c1 ^ " " ^ c2) "" all_categories ^ "\n")
            val () = print ("Total number of documents:" ^ Int.toString total_num_docs ^ "\n")
            val () = print ("Total number of distinct words:" ^ Int.toString total_num_words ^ "\n")
            val () = print ("Number of words by category:\n" ^ 
                            (Seq.mapreduce (fn (cat,count) => "  " ^ cat ^ " " ^ Int.toString count ^ "\n")
                                          "" (op^)
                                          (C.CatDict.toSeq num_words_by_cat))
                            ^ "\n"
                            )
        in 
            ()
        end

    fun print_possibles (train : C.labeled_document Dataset.mapreducable) ((cats, words) : C.labeled_document) : unit = 
        let val cc = C.count_by_category train
            val pp = C.postprocess cc
            val probs = C.possible_classifications cc pp words
        in 
            print (("Given Categories: " ^ Seq.mapreduce (fn c => c) "" (fn (c1,c2) => c1 ^ " " ^ c2) cats ^ "\n" ^
                    "Scores:\n" ^ Seq.mapreduce (fn (c,r) => "  " ^ c ^ " " ^ Real.toString r) "" (fn (s1,s2) => s1 ^ "\n" ^ s2) probs))
        end

    fun number_correct (train : labeled_document Dataset.mapreducable) (test : labeled_document Dataset.mapreducable) : int * int = 
        let 
            val cl = C.train_classifier train
        in 
            Dataset.mapreduce (fn (correct_answers, words) =>
                          let val (predicted, _) = cl words
                          in 
                              case SeqUtils.contains (fn x => x = predicted) correct_answers of
                                  true => (1,1)
                                | false => (0,1)
                          end) 
                          (0,0)
                          (fn ((cor1,tot1),(cor2,tot2)) => (cor1 + cor2 , tot1 + tot2))
                          test
        end

    fun print_predictions (train : labeled_document Dataset.mapreducable) (test : labeled_document Dataset.mapreducable) : unit =
        let 
            val cl = C.train_classifier train
        in 
            print (Dataset.mapreduce (fn (correct_answers, words) =>
                                 let val (predicted, _) = cl words
                                     val correctstring = "Given Categories: " ^ Seq.reduce (fn (c1,c2) => c1 ^ " " ^ c2) "" correct_answers ^ "\n"
                                     val predstring    = "Predicted: " ^ predicted ^ "\n"
                                     val doc           = (Seq.reduce (fn (s1,s2) => s1 ^ " " ^ s2) "" words) ^ "\n"
                                     val report        = correctstring ^ predstring ^ doc ^ "\n"
                                 in 
                                     (case SeqUtils.contains (fn x => x = predicted) correct_answers of
                                          true => "CORRECT\n" ^ report
                                        | false => "INCORRECT\n" ^ report)
                                 end) 
                                ""
                                (op^)
                                test)
        end
end

structure TestSeq =
struct

    structure TC = TestClassify(SeqMR)
    open TC

    open SeqUtils

    val simplest_train = seq [ (seq ["ECAT"], seq ["stock"]), (seq ["GCAT"], seq ["congress"]) ]
    val simple_train   = seq [ (seq ["ECAT"], seq ["stock","price"]), (seq ["GCAT"], seq ["congress","court"]) ]
    val cross_train    = seq [ (seq ["ECAT"], seq ["stock","price","fell"]), (seq ["GCAT"], seq ["congress","court","fell"]) ]
    val dups_train     = seq [ (seq ["ECAT"], seq ["stock","price","stock","price"]), (seq ["GCAT"], seq ["congress","court","court","congress"]) ]

    val doc1 = (seq ["ECAT"], seq ["stock"])
    val doc2 = (seq ["GCAT"], seq ["congress"])
    val doc3 = (seq ["GCAT"], seq ["court","fell"])
    val doc4 = (seq ["ECAT"], seq ["stock","ticker"])

    val docs14 = seq[doc1,doc2,doc3,doc4]

end

structure TestFile =
struct

    structure TC = TestClassify(FileMR)
    open TC

    fun parse_line (s : string) : labeled_document = 
        let val [cats , noncats] = String.tokens (fn #"\t" => true | _ => false) s
        in 
            (Seq.filter (fn s => String.isSuffix "CAT" s) (SeqUtils.seq (String.tokens (fn #"," => true | _ => false) cats)),
             SeqUtils.words noncats)
        end

    fun open_file (s : string) : TextIO.instream * (string -> labeled_document) =
        (TextIO.openIn s, parse_line)

    fun time (f : unit -> 'a) : {gcsys : IntInf.int, gcusr : IntInf.int, ngcsys : IntInf.int, ngcusr : IntInf.int} * 'a = 
        let val t = Timer.startCPUTimer ()
            val a = f ()
            val {gc={sys=gcsys,usr=gcusr},
                 nongc={sys=ngcsys,usr=ngcusr}} = Timer.checkCPUTimes t
        in
            ({gcsys = Time.toSeconds gcsys, 
              gcusr = Time.toSeconds gcusr,
              ngcsys = Time.toSeconds ngcsys,
              ngcusr = Time.toSeconds ngcusr}, a)
        end

end
