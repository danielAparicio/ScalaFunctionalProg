package week1

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object exercise {

	//@tailrec cannot be used directly in a worksheet that is why we have created a block for that
	//IMPORTANT this block we can also if we want specify the reurn type : def callgcd(a:Int, b:Int):Int={
	def callgcd(a:Int, b:Int)={
		@tailrec
  	def gcd(a:Int, b:Int):Int =
    	if (b==0) a else gcd(b, a % b)
    	
    gcd(a,b)
	}                                         //> callgcd: (a: Int, b: Int)Int
	callgcd(1,2)                              //> res0: Int = 1
	
	
	  
  def abs(x:Double) = if (x<0) -x else x          //> abs: (x: Double)Double

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double
  sqrt(2)                                         //> res1: Double = 1.4142156862745097
  
  
  
  
  
  //----------------------------------------------------------------------------------------------
  
  //IMPORTANT VAR and VAL remember the java one too
  
  //The params to the functions are alaways passed as VAL (like in java, primitive value or object reference),
  //so you cannot reassign inside (in java you can, if its a primitive
  //has no effect if its an object you would not be changing the object that you have outside anymore)
  
  //You can modify objects as long as you are using mutable types, if its inmutable (we create a new copy everytime)
  //no matter what you do inside the value of the var object outside doesn't change (in java for example if you pass an String param
  //this will always keep the value outside as String in java is inmutable)
  
  //In scala everything is an object (even the ones that corresponds to primitives in java):
  //All the AnyVal instances are INMUTABLE VALUE INSTANCES, and all the AnyVal types are ABSTRACT FINAL.
  //Hence, none of them can be instantiated with NEW. Rather, new instances are created with literal values (e.g., 3.14 for a Double)
  //or by calling methods on instances that return new values.
  
  val numv = 5                                    //> numv  : Int = 5
  val listv = List()                              //> listv  : List[Nothing] = List()
  val arrayv = Array[Int](1)                      //> arrayv  : Array[Int] = Array(1)
  
  
  var numvr = 7                                   //> numvr  : Int = 7
  var listvr = List()                             //> listvr  : List[Nothing] = List()
  var arrayvr = Array[Int](1)                     //> arrayvr  : Array[Int] = Array(1)
  
  
  def allval(numval:Int,listval:List[Int],arrayval:Array[Int]){
  	//numval = 4;   Error reasignment to val
    5 :: listval
    arrayval(0)=5
  }                                               //> allval: (numval: Int, listval: List[Int], arrayval: Array[Int])Unit
  
  def allvar(numvar:Int,listvar:List[Int],arrayvar:Array[Int]){
    //numvar = 4;   Error reasignment to val
    1 :: listvar
    arrayvar(0)=1
  }                                               //> allvar: (numvar: Int, listvar: List[Int], arrayvar: Array[Int])Unit
  
  allval(numv,listv,arrayv)
  allvar(numvr,listvr,arrayvr)
  
  //inmutable list value doesnt change in any case
  listv                                           //> res2: List[Nothing] = List()
  listvr                                          //> res3: List[Nothing] = List()
  
  //mutable the value changes in any case
  arrayv                                          //> res4: Array[Int] = Array(5)
  arrayvr                                         //> res5: Array[Int] = Array(1)
  
  ////////////////////////////////////////////////////////////////////////////////
  // Mutable value inside a var and a val, if the mutalbe object change
  //IMP depending of how are we using that mutable object
  //in this case we create the refernece of an objec to create another one, so DOESNT CHANGE
  
  //Mutable list buffer
  var listbufA = ListBuffer(1)                    //> listbufA  : scala.collection.mutable.ListBuffer[Int] = ListBuffer(1)
  
  val listbugB = ListBuffer(4) ++ listbufA        //> listbugB  : scala.collection.mutable.ListBuffer[Int] = ListBuffer(4, 1)
  //Same result being listbugB a var or a val
  //we are creating a new list with the elements of the mutable object , not using the reference of the mutalbe object
  //So doesn´t change
  
  listbufA += 2                                   //> res6: scala.collection.mutable.ListBuffer[Int] = ListBuffer(1, 2)
  
  listbugB                                        //> res7: scala.collection.mutable.ListBuffer[Int] = ListBuffer(4, 1)
  //doest change!!!!
  
  ///////////////////////////////////////////////////////
  //Now with a refernece to a var inside the val
  // in this case changes as we have an object in the val that has a reference to another object (the one hold in the var)
  
  var elem = ListBuffer(4)                        //> elem  : scala.collection.mutable.ListBuffer[Int] = ListBuffer(4)
  //var elem = 10
  //if var is an elem = 10 the final val of list Elem DOESNT CHANGE
  
  
  val listElem = List(elem)                       //> listElem  : List[scala.collection.mutable.ListBuffer[Int]] = List(ListBuffe
                                                  //| r(4))
  //Same result being listbugB a var or a val
  //we are creating a new list with the elements of the mutable object , not using the reference of the mutalbe object
  //So doesn´t change
  
  elem += 10                                      //> res8: scala.collection.mutable.ListBuffer[Int] = ListBuffer(4, 10)
  
  listElem                                        //> res9: List[scala.collection.mutable.ListBuffer[Int]] = List(ListBuffer(4, 1
                                                  //| 0))
  

  //Now with primitives values inide the list
  //or getting a value of a mutable object that is a inmutable value (ListBuffer.header of  ListBuffer[Int])
  //The VAL, VAR reulst value DOESN'T CHANGES!!!!!!!!!!
  
  var factor = 2                                  //> factor  : Int = 2
  
  val result = List(2 * factor)                   //> result  : List[Int] = List(4)
  //Same result being result a var or a val
  
  factor = 5
  
  result                                          //> res10: List[Int] = List(4)
  
  
  //////////////////////////////////////////////////////
  //With Closures
 
  var multiplier = 2                              //> multiplier  : Int = 2
  
  val applymultiplier = (n:Int) => 2 * multiplier //> applymultiplier  : Int => Int = <function1>
  
  applymultiplier(2)                              //> res11: Int = 4
  
  multiplier = 4
  
  applymultiplier(2)                              //> res12: Int = 8
  
  //the result of applying the closure Changes
  
   
}
//TAIL RECURSION
//IMP if you have a recursive function that calls herself as its last action (IMP not adding a value to the function or 2 functions)
//The fucntion stack can be resused, that is called tail recursion
// cgcd(b, a % b) tail recursive
// n * gcd(b, a % b) NOT tail recursive

//refactor your function to be tailrecursive if you can, but not if its very complex and you are going to lose too much clarity