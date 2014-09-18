package week6

object Sequ {

  //Arrays and Strings can USE Sequence methods

  val f = Array(1, 2, 3)                          //> f  : Array[Int] = Array(1, 2, 3)
  //IMP this is the same but specifying the type
  val j: Array[Int] = Array(1, 2, 3)              //> j  : Array[Int] = Array(1, 2, 3)
  f map (x => 2 * x)                              //> res0: Array[Int] = Array(2, 4, 6)

  val s = "Hello World"                           //> s  : String = Hello World
  //IMP this is the same but specifying the type
  val p: String = "Hello World"                   //> p  : String = Hello World

  s filter (c => c.isUpper)                       //> res1: String = HW
 
 ///////
 
 def isPrime(n:Int): Boolean= (2 until n) forall (d => n % d !=0)
                                                  //> isPrime: (n: Int)Boolean
 //this solution is not taking in account the efficiency
 
 
 //this in Imperative Languaje would be 2 for nested loops
 //this is a less legible way with FOR is much more clear and understandable
 val n= 7                                         //> n  : Int = 7
 
 (1 until n) flatMap
 	( i => (1 until i) map ( j => (i,j))) filter
 		(pair => isPrime(pair._1 + pair._2))
                                                  //> res2: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5))
 //IMP 2 nested for in I.Languaje more easy and legible than above
 for{ i <- 1 until n
 			j <- 1 until i
 			if isPrime(i + j)
 		} yield (i,j)                     //> res3: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5))
 
}