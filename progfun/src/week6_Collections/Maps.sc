package week6



object Maps {

val nmemonics = Map(
"2"->"ABC","3"->"DEF","4"->"GHI","5"->"JKL",
"6"->"MNO","7"->"PQRS","8"->"TUV","9"->"WXYZ")    //> nmemonics  : scala.collection.immutable.Map[String,String] = Map(8 -> TUV, 4
                                                  //|  -> GHI, 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)

type Word = String
type Sentence = List[Word]

type Occurrences = List[(Char, Int)]

var pepe: Word = "tee"                            //> pepe  : week6.Maps.Word = tee
var sentenceList : Sentence = List("tee","tee")   //> sentenceList  : week6.Maps.Sentence = List(tee, tee)




def wordOccurrences(w: Word): Occurrences = (w.toLowerCase().toList groupBy(x=>x) map (x => (x._1, x._2.length)) toList) sorted
                                                  //> wordOccurrences: (w: week6.Maps.Word)week6.Maps.Occurrences
                                                  
pepe.toLowerCase().toList groupBy(x=>x)           //> res0: scala.collection.immutable.Map[Char,List[Char]] = Map(e -> List(e, e),
                                                  //|  t -> List(t))

pepe.toLowerCase().toList groupBy(x=>x) map (x => (x._1, x._2.length))
                                                  //> res1: scala.collection.immutable.Map[Char,Int] = Map(e -> 2, t -> 1)

pepe.toLowerCase().toList groupBy(x=>x) map (x => (x._1, x._2.length)) toList
                                                  //> res2: List[(Char, Int)] = List((e,2), (t,1))

wordOccurrences("tee")                            //> res3: week6.Maps.Occurrences = List((e,2), (t,1))














def sentenceOccurrences(s: Sentence): Occurrences = (s flatMap (wordOccurrences) groupBy(x=>x._1) map (x => (x._1, x._2.length)) toList) sorted
                                                  //> sentenceOccurrences: (s: week6.Maps.Sentence)week6.Maps.Occurrences
  
sentenceList.flatMap(wordOccurrences)             //> res4: List[(Char, Int)] = List((e,2), (t,1), (e,2), (t,1))

sentenceList.flatMap(wordOccurrences).groupBy(x=>x._1)
                                                  //> res5: scala.collection.immutable.Map[Char,List[(Char, Int)]] = Map(e -> List
                                                  //| ((e,2), (e,2)), t -> List((t,1), (t,1)))


sentenceList.flatMap(wordOccurrences).groupBy(x=>x._1).map (x => (x._1, x._2.foldLeft(0)((y,k)=>y+k._2)))
                                                  //> res6: scala.collection.immutable.Map[Char,Int] = Map(e -> 4, t -> 2)

sentenceOccurrences(sentenceList)                 //> res7: week6.Maps.Occurrences = List((e,2), (t,2))













wordOccurrences("abcdea") map {x => for {i <- x._2 to 1 by -1 } yield (x._1,i)}
                                                  //> res8: List[scala.collection.immutable.IndexedSeq[(Char, Int)]] = List(Vecto
                                                  //| r((a,2), (a,1)), Vector((b,1)), Vector((c,1)), Vector((d,1)), Vector((e,1))
                                                  //| )

val ocurrenceList : List[(Char,Int)] = List(('a', 3), ('b', 1))
                                                  //> ocurrenceList  : List[(Char, Int)] = List((a,3), (b,1))

def combinations(occurrences: Occurrences): List[Occurrences] =  {
	 //the problem is that the return type is a indexedSeq instead of a Ocurrence because the type yielded on the for is tied to the <-object
    //occurrences map { x=> for {i <- x._2 to 1 by -1 } yield (x._1,i)}
   
    //with the iteration over  the list we solve the problem of the map operator
    //here we generate all the items for a char with more than one occurrence (a,2) to (a,1),(a,2)
    def generateCharOcurrences(occurrences : (Char,Int)):Occurrences={
      (for {i <- 0 to occurrences._2
      } yield (occurrences._1,i)).toList
    }
    
    //here we mix the current accumulated occurrence lists with the occurrencesToAdd param
    def mixCharOcurrences(occurrencesAcc : List[Occurrences], occurrencesToAdd : Occurrences):List[Occurrences]={
      for {i <- occurrencesAcc
           k <-occurrencesToAdd
      } yield i:+k //add at the end of the list (IMP be careful heavier operation that adding a head)
    }
    //looping the Occurrences recursively with pat match
    def combinationsAcc(occurrences : Occurrences,ocurrencesAcc : List[Occurrences]): List[Occurrences] = occurrences match{
      
    	case List()=> ocurrencesAcc
	    case List(x) => mixCharOcurrences(ocurrencesAcc,generateCharOcurrences(x))
	    case x::xs => combinationsAcc(xs,mixCharOcurrences(ocurrencesAcc,generateCharOcurrences(x)))
	}
    
    //call and filter the results that contains 0
    //TODO: check why is working with "List(List())" and not with "List[Occurrences]"
    combinationsAcc(occurrences,List(List())) map {x => x filter (x=>(x._2!=0))}
    
    //Alternative wrong thoughts
    //Wrong and longer way :(generateCharOcurrences(ocurrenceList).combinations(occurrences.length).toList map {x => for {i<-x;if (i match{ case (_,0) => false;case _ => true })}yield i}).distict
    //(generateCharOcurrences(occurrences).combinations(occurrences.length).toList map {x => x filterNot (x => x._2==0 )}).distinct)
  }                                               //> combinations: (occurrences: week6.Maps.Occurrences)List[week6.Maps.Occurren
                                                  //| ces]
   
   combinations(List(('a', 1),('b', 2),('c', 1))) //> res9: List[week6.Maps.Occurrences] = List(List(), List((c,1)), List((b,1)),
                                                  //|  List((b,1), (c,1)), List((b,2)), List((b,2), (c,1)), List((a,1)), List((a,
                                                  //| 1), (c,1)), List((a,1), (b,1)), List((a,1), (b,1), (c,1)), List((a,1), (b,2
                                                  //| )), List((a,1), (b,2), (c,1)))
   
   
   
   sentenceOccurrences(List("Yes"))               //> res10: week6.Maps.Occurrences = List((e,1), (s,1), (y,1))
   combinations(sentenceOccurrences(List("Yes"))) //> res11: List[week6.Maps.Occurrences] = List(List(), List((y,1)), List((s,1))
                                                  //| , List((s,1), (y,1)), List((e,1)), List((e,1), (y,1)), List((e,1), (s,1)), 
                                                  //| List((e,1), (s,1), (y,1)))
   
   
    
    def subtractFromMap(acc:Map[Char,List[(Char, Int)]],pair:(Char,Int)):Map[Char,List[(Char, Int)]]= {
      //In the assignment is specify that y is always a x subset thus we don't have to check if the key is contained in the map with a get and a match
      //and we use apply directly
      //We can get the 0 directly because we know that we only have one element per Char key in the map
      val mapValue = acc.apply(pair._1)(0)
      //We subtract the Int in the value of the key in the map or directly remove the key (depending on the Int of x and y )
      if(mapValue._2 > pair._2) acc.updated(pair._1, List((mapValue._1,mapValue._2 - pair._2 )))
      else acc - (pair._1)
    }                                             //> subtractFromMap: (acc: Map[Char,List[(Char, Int)]], pair: (Char, Int))Map[C
                                                  //| har,List[(Char, Int)]]
    
     
     List(('b',1), ('y',1)).foldLeft(List(('a',1), ('b',1), ('l',2), ('y',1)).groupBy(_._1))(subtractFromMap).map (j => (j._1, j._2(0)._2)).toList.sorted
                                                  //> res12: List[(Char, Int)] = List((a,1), (l,2))
 
    List(('a',1), ('b',1), ('l',2), ('y',1)).groupBy(_._1)
                                                  //> res13: scala.collection.immutable.Map[Char,List[(Char, Int)]] = Map(b -> Li
                                                  //| st((b,1)), y -> List((y,1)), a -> List((a,1)), l -> List((l,2)))

		List(('b',1), ('y',1)).foldLeft(List(('a',1), ('b',1), ('l',2), ('y',1)).groupBy(_._1))(subtractFromMap)
                                                  //> res14: scala.collection.immutable.Map[Char,List[(Char, Int)]] = Map(a -> Li
                                                  //| st((a,1)), l -> List((l,2)))
  
 }
 
 
 