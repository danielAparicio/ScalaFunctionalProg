package week1

import scala.annotation.tailrec

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
}
//TAIL RECURSION
//IMP if you have a recursive function that calls herself as its last action (IMP not adding a value to the function or 2 functions)
//The fucntion stack can be resused, that is called tail recursion
// cgcd(b, a % b) tail recursive
// n * gcd(b, a % b) NOT tail recursive

//refactor your function to be tailrecursive if you can, but not if its very complex and you are going to lose too much clarity