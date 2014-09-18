package forcomp

import common._
import language.postfixOps
//http://www.scala-lang.org/api/2.11.0/index.html#scala.language$
//if we don't add this we have warnings while compiling
//postfix operator toList should be enabled
//[warn] by making the implicit value scala.language.postfixOps visible.
//[warn] (s flatMap (wordOccurrences) groupBy(x=>x._1) map (x => (x._1, x._2.length)) toList) sorted

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  /*Parse the String to list , remove white spaces, group by an element (char) and get a Map (char->List[Char]()) , execute the function (x => (x._1, x._2.length) for every Map element
   so we end up with a Map of (char ->Int) that was (x._1, x._2.length) in the map function and finally parse to list and sort */
  def wordOccurrences(w: Word): Occurrences = 
    (w.toLowerCase().toList groupBy(x=>x) map (x => (x._1, x._2.length)) toList) sorted
    //IMP when applying map over a map x._1 is the key and x._2 is the value
    // with filtering
    //(w.toLowerCase().toList.filter(x => x != ' ') groupBy(x=>x) map (x => (x._1, x._2.length)) toList) sorted 

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = 
    //flatmap the list of lists to a list and do something similar to wordOccurrences
    (s flatMap (wordOccurrences) groupBy(x=>x._1) map (x => (x._1, x._2.foldLeft (0) ((y,k)=>y+k._2 ))) toList) sorted
    //IMP when applying map over a map x._1 is the key and x._2 is the value

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  //lazy val, only executed when required as is a heavy  operation
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
     //IMP with Map() ++ we return a Map as by default the yielding is over the SAME type as dictionary that is a LIST 
	 //Map() ++ (for {word <- dictionary } yield (wordOccurrences(word) -> List(word)))

    //we loop over the list , make the operations in the map with patter matching (add to a Map an new key or modify an existing one (being inmutable both ops create a new map)) 
    //and call the function again (recursion to loop through the list)
     def dictionaryByOccurrencesAcc(dic:List[Word],dicOcc:Map[Occurrences, List[Word]]):Map[Occurrences, List[Word]]=      
      if(!dic.isEmpty) dicOcc.get(wordOccurrences(dic.head)) match {      
	       case Some(listWord) => dictionaryByOccurrencesAcc (dic.tail, dicOcc updated (wordOccurrences(dic.head) , (dic.head :: listWord))) 
	       case None => dictionaryByOccurrencesAcc (dic.tail, dicOcc updated (wordOccurrences(dic.head) , List(dic.head))) 
	       	  //dirty way adding and therefore smashing value dicOcc + (wordOccurrences(dic.head) -> List(dic.head)
      	}
      else dicOcc
    
 	 dictionaryByOccurrencesAcc(dictionary,Map[Occurrences, List[Word]]())	
  }
  /** Returns all the anagrams of a given word. */
  //patter matching with a map previously calculated
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)) match {
    case Some(listWord) => listWord
    case None => List()    
  }
  
  def wordsForAnOcurrence(ocurrence: Occurrences): List[Word] = dictionaryByOccurrences.get(ocurrence) match {
    case Some(listWord) => listWord
    case None => List()    
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
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
      for { i <- occurrencesAcc
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
  }
		  
		  

  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    
    def subtractFromMap(acc:Map[Char,List[(Char, Int)]],pair:(Char,Int)):Map[Char,List[(Char, Int)]]= {
      //In the assignment is specify that y is always a x subset thus we don't have to check if the key is contained in the map with a get and a match
      //and we use apply directly
      //We can get the 0 directly because we know that we only have one element per Char key in the map
      val mapValue = acc.apply(pair._1)(0) 
      //We subtract the Int in the value of the key in the map or directly remove the key (depending on the Int of x and y )
      if(mapValue._2 > pair._2) acc.updated(pair._1, List((mapValue._1,mapValue._2 - pair._2 )))  
      else acc - (pair._1)     
    }      
    //alternative way (acc,y)=>WhatEverTheFuntionDoes. The params of this function y.foldLeft--> the initial acumulator and the firts param of the subtractFromMap
    //are Map[Char,List[(Char, Int)]] (the base list x parsed to Map) then the second param of subtractFromMap is (Char,Int) that is the type of the list 
    // yy in which we are applying foldLeft and contains the pair that we will subtract from the original map x
    y.foldLeft(x.groupBy(_._1))(subtractFromMap).map (j => (j._1, j._2(0)._2)).toList.sorted
    //IMP USING ._1 and ._2 WITH MAPS j._2= value for map ,(0) first position of map value (a list), ._2= second part of tuple (an Int)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {   
    
    def sentenceAnagramsAcc(occurrences:Occurrences):List[Sentence]= {
      //println("start anagramAcc occurences:" + occurrences + "start anagramAcc comb:" + combinations(occurrences) )
      //we don't need this to stop recursion as eventually combinations(occurrences) will be empty and we will not call the function recursively again
      if (occurrences.isEmpty ) {/*println("empty occ : returning List(List())"  )*/;List(List())}
      else
	      for { occ <- {combinations(occurrences) filter (x => x!= List())} 	      		
	      		word <- {/*println("occurrence :" + occ +"wordList :" + wordsForAnOcurrence(occ) )*/ ;wordsForAnOcurrence(occ)}
	      		//making recursive call in the last place improves the performance a lot
	      		rest <- {/*println("occurrences Left :" + occurrences +"occurrence :" + occ +"call anagramAcc :" + subtract(occurrences, occ))*/ ; sentenceAnagramsAcc(subtract(occurrences, occ))}
	      		//the curly braces	are only to be able to have 2 statements and be able to println and debug    	  	
	    	  	//if we have any empty occ, word or sentence we will not yield 	    	  		            	            
	      	  } yield word::rest
    }    
    sentenceAnagramsAcc(sentenceOccurrences(sentence))
  }

}
