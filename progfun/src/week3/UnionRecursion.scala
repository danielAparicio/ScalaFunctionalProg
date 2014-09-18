package week3
 
import util.Random;
 
object intsets {
  //util.Properties.versionString
  
  // Return the (approx) number of seconds elapsed after calling the given block
  def time[R](block: => R): Double = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    (t1 - t0) / 1000000.0
  }
 
  val small = 10
  val large = 100
  
  val singleTon = new NonEmpty(100, Empty, Empty)
  val smallSet = Seq.fill(small)(Random.nextInt(small * 1000)).foldLeft(Empty: IntSet)(_.incl(_))
  val largeSet = Seq.fill(large)(Random.nextInt(large * 1000)).foldLeft(Empty: IntSet)(_.incl(_))
  
  time { Empty U Empty }
  time { Empty U singleTon }
  time { Empty U smallSet }
  time { Empty U largeSet }
  time { singleTon U Empty }
  time { singleTon U singleTon }
  time { singleTon U smallSet }
  time { singleTon U largeSet }
  time { smallSet U Empty }
  time { smallSet U singleTon }
  time { smallSet U smallSet }
  time { smallSet U largeSet }
  time { largeSet U Empty }
  time { largeSet U singleTon }
  time { largeSet U smallSet }
  time { largeSet U largeSet }
}
 
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
  // Only possible if saved as UTF-8
  def U(other: IntSet) = union(other)
}
 
object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def toString = "."
  def union(other: IntSet): IntSet = other
}

/*IMP in the Cons class the val in the parameters of the class constructor means that they are
at the same time the parameters and the field value definition in the class*/
class NonEmpty(val elem: Int, val left: IntSet, val right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  override def toString = "{" + left + elem + right + "}"
  def union(other: IntSet): IntSet =
    // Changing the following expression to ((left U right) U other), union can take forever
    (left U (right U other)) incl elem
}