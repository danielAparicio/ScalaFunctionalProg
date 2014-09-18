package patmat

import common._
import scala.annotation.tailrec


/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  object EmptyTree extends CodeTree



  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match{
    case Leaf(c,w) => w
    case Fork(lct, rct, c, w) =>  w
  } 

  def chars(tree: CodeTree): List[Char] = tree match{
    case Leaf(c,w) => List(c)
    case Fork(lct, rct, c, w) => c    
  }

  def left(tree: CodeTree): CodeTree = tree match{
    case Fork(lct, rct, c, w) => lct 
    case _ => EmptyTree
  } 
  
  def right(tree: CodeTree): CodeTree = tree match{
    case Fork(lct, rct, c, w) => rct   
    case _ => EmptyTree
  } 

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Part 2: Generating Huffman trees
   /**
   Using makeCodeTree, code trees can be constructed manually in the following way
   
   val sampleTree = makeCodeTree(
  		makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
  		Leaf('t', 2)
	)
   */

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  /* we loop through the list and add 1 to the Int part of the pair(is a list of elements (Char, Int)) if the char part of the pair
   * is equals to the X (IMPORTANT we should use X that for pattern matching is a constant,
   * with x is a variable and only binds the value)
   * important in the recursive call join the head and tail of the list again ::
   * x++ is not working in Scala*/
  def  listAnalysis(X:Char,acc: List[(Char, Int)]): List[(Char, Int)] = acc match {
    //acc list is inmutable everytime we make some modification we create a new list that is why we need
    //to return a List , not get the acc modifications
    //if we reach the end of the list means that we didnt find X within the list and we need to add it
    case Nil => (X,1) :: List()
    //we return a new list
    case (X,_) :: tail => (X,acc.head._2 + 1)::tail 
    //we return a new list
    case _ => acc.head :: listAnalysis(X,acc.tail)      
  }
 
  def times(chars: List[Char]): List[(Char, Int)] = { 
    //acc list is inmutable everytime we make some modification we create a new list that is why we need
    //to return a new list int the listAnalysis method
    @tailrec
    def timesAcc(chars: List[Char],acc:List[(Char, Int)]): List[(Char, Int)] = {
      //We have been modifying acc and when list is empty we should return the acc with all the elements 
      if (chars.isEmpty) acc
      //we call recursively modifying acc with the listAnalysis method
      else timesAcc(chars.tail,listAnalysis(chars.head,acc))      
    }           
    timesAcc(chars,List())
    //
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    
     def makeOrderedLeafListAcc(freqs: List[(Char, Int)]): List[Leaf] = freqs match {       
       case List() => List()      
       case _ => Leaf(freqs.head._1,freqs.head._2) :: makeOrderedLeafListAcc(freqs.tail) 
       //this could be also done with if else statement      
     }   
     def sort(xs: List[(Char, Int)], xsSorted : List[(Char, Int)]): List[(Char, Int)] = xs match {
      case List() => xsSorted
      //WRONG :this is wrong as if the first 2 elements are like List(('t', 2), ('e', 5)... in the insert function we go for x::xs and we dont iterate
      //or sort anymore....so is only working in one case...BADDD
      //case y::ys => insert(y,ys)
      //GOOD :the right aproach is have a list as acc and loop through the original list to populate the accumulator calling insert
      case y::ys => sort(ys,insert(y,xsSorted))
      
	 }
	  
	 def insert(x: (Char, Int), xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
	      //IMPORTANT if the rest of the list is empty we should give back the list of the element (last element)
	      case List() => List(x)
	      case y::ys => if (x._2 <= y._2) x::xs else y::insert(x,ys)      
	 }
     makeOrderedLeafListAcc(sort(freqs,List()))   
  }  

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case x::List() => true
    //IMP: this is a pattert to match a list that contains one element , is more elegant than the above one
    //case List(x) => true
    case _ => false    
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match  { 
    //IMP: List() and Nil is the same so this first pattern matching is reiterative
    case Nil => List()
    case List() => List()
    //I think that both expressions above are the same , no????
    case x::List() => List(x) 
    //We need to reorder the list per tree weight once we create a fork with the ones of less weight
    case x::xs => insertTree(Fork(x,xs.head,chars(x):::chars(xs.head),weight(x) + weight(xs.head)),xs.tail)
  }

  def insertTree(x: CodeTree, xs: List[CodeTree]): List[CodeTree] = xs match {
	      case List() => x::List()
	      case y::ys => if (weight(x)<=weight(y)) x::xs else y::insertTree(x,ys)      
  }
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  @tailrec
  def until(singletonFunc: List[CodeTree]=>Boolean , combineFunct: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): CodeTree = {   
//   case List() => EmptyTree 
//   case x::List() => x
//   case x::xs => until(singletonFunc,combineFunct)(combineFunct(trees)) 
    
    //The method above was working perfectly but in the assignment specifies that singleton should be used...
    if(singletonFunc(trees)) trees.head
    else trees match {
      case Nil => EmptyTree
      case List() => EmptyTree 
      case x::xs => until(singletonFunc,combineFunct)(combineFunct(trees)) 
    }
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree =chars match {
    case List() => EmptyTree
    case _ => until(singleton,combine)(makeOrderedLeafList(times(chars)))   
  }

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {    
	 def decodeAcc(originalTree: CodeTree, tree: CodeTree, bits: List[Bit]): List[Char] = bits match {
	   //we need to call recursively based on the bits list not based on the tree, and nest two case	   
	    case List() => tree match {
	    			//we need to do this as if the list is empty and we have a Leaf (This means last char of the decoding) 
	    			//we should add the char to the list if we have a fork the decoding is finish
					case Leaf(_,_) => chars(tree):::decodeAcc(originalTree,originalTree,bits)
					case Fork(_,_,_,_) => List()
  				 }
  		case _ => tree match {
					//IMPORTANT match against a type, with the constructor and whatever params
					case Leaf(_,_) => chars(tree):::decodeAcc(originalTree,originalTree,bits)
					//IMPORTANT: same as case Fork(_,_,_,_)  ==== _:Fork or w:Fork, is whatever object _ that is a Fork type
					case _:Fork => if(bits.head==0) decodeAcc(originalTree,left(tree),bits.tail) else decodeAcc(originalTree,right(tree),bits.tail)
  				 }
	 }     
    decodeAcc(tree,tree,bits)    
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode,secret)



  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = text match {
    case List() => List()
    //IMP case List(x) => charEncoded(tree,x) is the same as below a list of one element, and is more elegant
    case x::List() => charEncoded(tree,x)
    //IMP here we can call recursively or to another function as we have cover previously with pattern matching
    //all the cases, empty list and list of one element
    case x::xs => charEncoded(tree,x) ::: encode(tree)(xs)
    //this cannot be done with the _
    //case x::_ => charEncoded(tree,x) ::: encode(tree)(_)
  }

  def charEncoded(tree: CodeTree,char: Char): List[Bit]=  {   
    def charEndodedHelper(tree: CodeTree,Char: Char,listAcc: List[Bit]): List[Bit]= tree match {
	    case Leaf(Char,_) => listAcc
	    case Leaf(_,_) => List()
		case Fork(_,_,_,_) => charEndodedHelper(left(tree),Char,listAcc:::List(0)) :::
							  charEndodedHelper(right(tree),Char,listAcc:::List(1))
    }  
	charEndodedHelper(tree,char,List())      
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(Char: Char): List[Bit] = table match {
    case List() => List()
    //if its the char that we are searching for we return the List of that element of the CodeTable
    case (Char,xy)::xs => xy
    //in any other case we keep on searching
    case x::xs => codeBits(xs)(Char)
  } 

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable =  {   
    def charCodetableHelper(tree: CodeTree,listAcc: List[Bit]): CodeTable= tree match {
        //important syntax List(  (char, listAcc)  ) List with a pair element (only one)
        //we dont need the weight for anything just the track of 0 and 1 that we have in listacc until we have reach the char
	    case Leaf(char,weight) => List((char, listAcc))
		case Fork(_,_,_,_) => charCodetableHelper(left(tree),listAcc:::List(0)) :::
							  charCodetableHelper(right(tree),listAcc:::List(1))
    }  
	charCodetableHelper(tree,List[Bit]())      
  }

  /*
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = ???

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] =  {  
    def quickEncodeHelper(table: CodeTable, text: List[Char] ): List[Bit] = text match {
      case List() => List()
      case x::xs => codeBits(table)(x) ::: quickEncodeHelper(table,xs)     
    }    
    quickEncodeHelper(convert(tree),text)
  }
}
