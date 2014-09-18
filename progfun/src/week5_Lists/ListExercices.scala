package week5_Lists
import math.Ordering

object ListExercices {
  
  def removeAt[T](xs : List[T], n : Int) = (xs take n) ::: (xs drop n + 1)
  
  
  //////////////////////////////////
  //efficient way of sorting a list with splitting it in two with recursion
  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (fst, snd) = xs splitAt n
      betterMerge(msort(fst), msort(snd))
    }
  }

  def merge(xs: List[Int], ys: List[Int]): List[Int] =
    xs match {
      case Nil => ys
      case x :: xs1 =>
        ys match {
          case Nil => xs
          case y :: ys1 =>
            if (x < y) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }
    }
  
  //better merge using tuples
  def betterMerge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    
    case (Nil,ys) => ys
    case (xs,Nil) => xs
    case (x::xs1,y::ys1) => if (x<y) x:: betterMerge(xs1,ys)
    						else y :: betterMerge(xs, ys1)
      
  }
  
  val nums = List(2,-4,5,7,1)
  msort(nums)
  
  //////////////////////////////////////////////
  //Implicit Parameters
  //Now we want to be able to have as parameter a list of wahtever type
  //we need to include (lt: (T, T) => Boolean) as a function in order to make the right comparison
  def msortT[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    
    val n = xs.length / 2
    if (n == 0) xs
    
    else {

      def betterMergeT(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {

        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (lt(x, y)) x :: betterMergeT(xs1, ys)
        else y :: betterMergeT(xs, ys1)

      }
      
      val (fst, snd) = xs splitAt n
      betterMergeT(msortT(fst)(lt), msortT(snd)(lt))
    }
  }
  
  val fruit = List("apple","banana","pear","melon")
  msortT(nums)((x: Int, y: Int) => x < y)
  //IMP: Parameter type can be inferred from the call msortT(nums)
  msortT(nums)((x, y) => x < y)
  
  //IMP: x.compareTo(y) is to compare Strings
  msortT(fruit)((x: String, y: String) => x.compareTo(y) < 0)
  //IMP: Parameter type can be inferred from the call msortT(fruit)
  msortT(fruit)((x, y) => x.compareTo(y) < 0)
  
  
  //Instead of with (lt: (T, T) => Boolean) we can do it with scala.math.Ordering[T] ---> (ord: Ordering[T])
  
  def msortTO[T](xs: List[T])(ord: Ordering[T]): List[T] = {
    
    val n = xs.length / 2
    if (n == 0) xs
    
    else {

      def betterMergeTO(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {

        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: betterMergeTO(xs1, ys)
        else y :: betterMergeTO(xs, ys1)

      }
      
      val (fst, snd) = xs splitAt n
      betterMergeTO(msortTO(fst)(ord), msortTO(snd)(ord))
    }
  }
  
  msortTO(nums)(Ordering.Int)
  msortTO(fruit)(Ordering.String)
  
  //IMP: f we call def msortTO[T](xs: List[T])(implicit ord: Ordering[T]) = we can remove the (ord) in the functions and the 
  //(Ordering.Int) in the calls as well
  
  
}

