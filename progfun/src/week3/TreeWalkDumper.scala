//package week3;
//// TreeWalkDumper.scala is a downsized version of TweetSet.scala.
//// Lots of code and all comments removed to keep it short for posting.
//// Console dumping code added.
//
//class Tweet(val user: String, val text: String, val retweets: Int) {
//  override def toString: String =
//    "User: " + user + " " + 
//    "Text: " + text + " [" + retweets + "]"
//}
//
//abstract class TweetSet {
//  def contains(tweet: Tweet): Boolean
//  def incl(tweet: Tweet): TweetSet
//  def foreach(f: Tweet => Unit): Unit
//}
//
//class Empty extends TweetSet {
//  def contains(tweet: Tweet): Boolean = false
//  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)
//  def foreach(f: Tweet => Unit): Unit = ()
//}
//
//class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
//
//  def contains(x: Tweet): Boolean = {
//    println("\n", elem)
//    if (x.text < elem.text) { println("less than"); left.contains(x) }
//    else if (elem.text < x.text) { println("greater than"); right.contains(x) } 
//    else { println("Found it!"); true } 
//  }
//
//  def incl(x: Tweet): TweetSet = {
//    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
//    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
//    else this
//  }
//
//  def foreach(f: Tweet => Unit): Unit = {
//    f(elem)
//    left.foreach(f)
//    right.foreach(f)
//  }
//}
//
//object TreeWalkDumper {
//  def main(args: Array[String]) {
//    println(" ")
//    println("This program will dump a TweetSet on the console.");
//    println("It will also dump the recursive tree walk on the console.");
//
//    val set1 = new Empty
//    val set2 = set1.incl(new Tweet("a", "a body", 20))
//    val set3 = set2.incl(new Tweet("b", "b body", 20))
//    val c = new Tweet("c", "c body", 7)
//    val d = new Tweet("d", "d body", 9)
//    val set4c = set3.incl(c)
//    val set5 = set4c.incl(d)
//    val x4 = new Tweet("x4", "x4 body", 7)
//    val x1 = new Tweet("x1", "x1 body", 7)
//    val x3 = new Tweet("x3", "x3 body", 7)
//    val x2 = new Tweet("x2", "x2 body", 7)
//    val set6 : TweetSet = set5.incl(x1).incl(x2).incl(x3).incl(x4)
//    val x5 = new Tweet("x5", "x5 body", 7)
//  
//    println("\n")
//    println("Dump set6, a TweetSet on the console.")
//    println("There are 8 tweets in this TweetSet.")
//    println("You'll see 8 lines below, one for each tweet.")
//    set6 foreach println 
//    println("\n")
//
//    println("Dump recursive steps walking a TweetSet on the console.")
//    println("This will return true.")
//    println("This will not walk the entire tree.") 
//    println("It will only visit 4 nodes in the tree.") 
//    println("contains(d) returned " + set6.contains(d)) 
//    println("\n")
//
//    println("Dump recursive steps walking a TweetSet on the console.")
//    println("This will return false.")
//    println("This will walk the entire tree.") 
//    println("It will visit all 8 nodes in the tree.") 
//    println("contains(x5) returned " + set6.contains(x5)) // this should print false
//  }
//}