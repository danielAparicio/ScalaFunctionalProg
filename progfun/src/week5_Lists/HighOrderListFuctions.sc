package week5

object HighOrderListFuctions {

  def squarelist(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => y * y :: squarelist(ys)

  }                                               //> squarelist: (xs: List[Int])List[Int]

/*applying HOFunctions

	abstract class List[T] {
    def map[U](f: T => U): List[U] = this match {
      case Nil => this
      case x :: xs => f(x) :: xs.map(f)
    }
  }

  def squarelistHOF(xs: List[Int]): List[Int] =

    xs map (x = x * x)
*/

 val nums = List(2,-4,5,7,1)                      //> nums  : List[Int] = List(2, -4, 5, 7, 1)
 
 nums filter (x => x > 0)                         //> res0: List[Int] = List(2, 5, 7, 1)
 nums filterNot (x => x > 0)                      //> res1: List[Int] = List(-4)
 nums partition (x => x > 0)                      //> res2: (List[Int], List[Int]) = (List(2, 5, 7, 1),List(-4))
 nums takeWhile (x => x > 0)                      //> res3: List[Int] = List(2)
 nums dropWhile (x => x > 0)                      //> res4: List[Int] = List(-4, 5, 7, 1)
 nums span (x => x > 0)                           //> res5: (List[Int], List[Int]) = (List(2),List(-4, 5, 7, 1))
 
 
 
 val data = List("a","a","a","b","c","c","a")     //> data  : List[String] = List(a, a, a, b, c, c, a)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      //we assign the return of span applied with an specific function to a val
      //after that we call recursively to pack again
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }                                               //> pack: [T](xs: List[T])List[List[T]]
  pack(data)                                      //> res6: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a))
                                                  //| 
  
  def encode[T](xs: List[T]): List[(T,Int)] =
      //IMP
      //we use the map method that applies the function to every member of the list
      //for every member in the list, map method returns a pair (not a list)
      //that contains the head and the legth of the element of the list (pack gives us a list of lists)
      pack(xs) map (ys => (ys.head , ys.length))  //> encode: [T](xs: List[T])List[(T, Int)]
      
   encode (data)                                  //> res7: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))
   


   //IMP for the difference between foldLeft and foldRight
   def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys) (_::_)                      //> concat: [T](xs: List[T], ys: List[T])List[T]
    
//   def concat[T](xs: List[T], ys: List[T]): List[T] =
//    (xs foldLeft ys) (_::_)
//   Concat is not working with foldLeft because of opetation type error:

//   With foldRight you concat :: always an elment with a list (right folded), but with foldLeft you concat ::
//   a list with an element and that is not allowed is concat (is always an element and a list)

//   foldRight ===> x1 op (rest) applied on a list xs (x1,x2,x3...,xn)
//   leftFold  ===> (rest) op Xn applied on a list xs (x1,x2,x3...,xn)
 
}