package week2

object exercise {

  def sum(f: Int => Int, a: Int, b: Int) = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, f(a) + acc)

    loop(a, 0)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int
  sum(x => x * x, 3, 5)                           //> res0: Int = 50
  /*IMP: Is an anonymus function x doesn't have a previous value when we use that anonymus function
  to for the def call , inside the method we call the anonymus function x=>x*x with specific params*/
  
  
  
  def mapReduce (f: Int=>Int , combine:(Int,Int)=>Int, zero:Int)(a:Int,b:Int):Int=
  	if(a>b) zero
  	else combine(f(a), mapReduce(f,combine,zero)(a+1,b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int
  	
  def product(f: Int => Int )(a: Int,b:Int):Int = mapReduce(f,(x,y) => x* y,1)(a,b)
                                                  //> product: (f: Int => Int)(a: Int, b: Int)Int
  product(x=>x*x)(3,4)                            //> res1: Int = 144
  
  
  def product2(f: Int => Int )(a: Int,b:Int):Int =
  	if(a>b) 1
  	else f(a)*product2(f)(a+1,b)              //> product2: (f: Int => Int)(a: Int, b: Int)Int
  
  product2(x=>x*x)(3,4)                           //> res2: Int = 144
  
  def fact(n:Int) = product2(x=>x)(1,n)           //> fact: (n: Int)Int
  fact(5)                                         //> res3: Int = 120
  
  //min 9 fixed points averagedamp ----> is a function that takes a function and return a function
   
  ///////////////////////////////////////////////////////////////
  //Currying example
  val summ: (Int, Int) => Int = _ + _             //> summ  : (Int, Int) => Int = <function2>
  val sumCurried = summ.curried(2)                //> sumCurried  : Int => Int = <function1>
  sumCurried(2)                                   //> res4: Int = 4
  
  
  ///////////////////////////////////////////////////////////////
  //A Closure function decared in different ways
  var multiplier = 2                              //> multiplier  : Int = 2
  val applymultiplier = (n:Int) => n * multiplier //> applymultiplier  : Int => Int = <function1>
  val applymultiplier2 : Int => Int = _ * multiplier
                                                  //> applymultiplier2  : Int => Int = <function1>
}