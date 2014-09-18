package objsets

import common._
import TweetReader._
import scala.util.control.Breaks
import scala.annotation.tailrec

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  /*yes we can as every subclass has his own filerAcc implementation and the call of
   the filter method is THE SAME for all the classes with an Empty TweetSet (we can instantiate a subclass object within the abstract superclass)*/
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p,new Empty)

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
   /*We cannot implement union here, should be abstract as this method has different implementations in the subclasses*/ 
   def union(that: TweetSet): TweetSet 

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
   /*is different to call mostRetweeted in an EMpty that in a nonEmpty , also the same with the helper that only needs to be implemented in the subclasses, same with teh Acc helper
    every subclass should implement it as the main def mostRetweeted is calling mostRetweetedAcc  */ 
   def mostRetweeted: Tweet
   
   /* GENERAL question ,IS BETTER ABSTRACT AND IMPLEMENT OR IMPLEMENT AND OVERRIDE?: depends on how many subclasses you have to override the method */ 
   def mostRetweetedAcc(mRetweeted: Tweet):Tweet
   
   def lessRetweeted: Tweet
   
   def lessRetweetedAcc(mRetweeted: Tweet):Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  
  def descendingByRetweet: TweetList
  
  def descendingByRetweetAcc(tweetsList: TweetList): TweetList


  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
  
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet): TweetSet = that
  
  /*this is when we call the most retweeted initially on a empty  TweetSet and is giving us back a Nothing type that is a subclass of all the objects
   * this stops the recursion without giving back any value so we cannot use it as the implementation of mostRetweetedAcc */
  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException()
  
  /*this is the method that we call when we are in an empty object but called initially on nonEmpty object, is our recursion stop condition*/
  def mostRetweetedAcc(mRetweeted: Tweet):Tweet = mRetweeted
  
  def lessRetweeted: Tweet = throw new java.util.NoSuchElementException()
  
  def lessRetweetedAcc(lRetweeted: Tweet):Tweet = lRetweeted
  
  def descendingByRetweet: TweetList= Nil
  
  def descendingByRetweetAcc(tweetsList: TweetList): TweetList= tweetsList

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  //function that returns void and....does nothing???
  def foreach(f: Tweet => Unit): Unit = ()
  
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  /*this is the key in Scala you can pass a function that you don't know how is implemented and you 
   only know it when you call the filterAcc method that you define it*/  
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    /*we call to the left side recursively with the acc of the return call of the right side recursively
     we start the recursion on the left side as in the union function as eventually the left side will end
     that is our end of recursion condition*/
    if(p(this.elem))  this.left.filterAcc(p, this.right.filterAcc(p, acc.incl(this.elem)))
    else this.left.filterAcc(p, this.right.filterAcc(p, acc))
   }
  
  /*we call the union of this set with every this tweetList (left and right) and after this the union with the 'that' tweetlist
    we know that the recursion will finish as we call the union with the this.left tweetList as the left side is a decremental value
    and eventually we will have a empty node
    here we are using union with the other representation not with union()*/
  def union(that: TweetSet): TweetSet = 
	 (this.left.union((this.right).union(that))) incl elem
	 /*IMP this is about ((this.left.union(this.right)).union(that))
	 performing worse than (this.left.union((this.right).union(that)))
	 the last one will always* call incl less times than the first one
	 IMP: in this union the new object is being built on the way back of the recursion
	 we need to go to the deeper part of the trees and build the new one
	 */

  //The initial ac is a tweet with any value as we know that if it's a NonEmpty we will have at least one tweet
  //We use this.elem.retweets as default retweets as we have to loop through the entire tweet set so that value as initial value is valid
  def mostRetweeted: Tweet = mostRetweetedAcc(new Tweet("","",this.elem.retweets))	 

  def mostRetweetedAcc(mRetweeted: Tweet): Tweet = {
    //This recursive call is similar to the filterAcc 
    if (this.elem.retweets >= mRetweeted.retweets) this.left.mostRetweetedAcc(this.right.mostRetweetedAcc(this.elem))
    else this.left.mostRetweetedAcc(this.right.mostRetweetedAcc(mRetweeted))
  }
  
  //The initial ac is a tweet with any value as we know that if it's a NonEmpty we will have at least one tweet
  //We use this.elem.retweets as default retweets as we have to loop through the entire tweet set so that value as initial value is valid
  def lessRetweeted: Tweet = lessRetweetedAcc(new Tweet("","",this.elem.retweets))	 

  def lessRetweetedAcc(lRetweeted: Tweet): Tweet = { 
    if (this.elem.retweets <= lRetweeted.retweets ) this.left.lessRetweetedAcc(this.right.lessRetweetedAcc(this.elem))
    else this.left.lessRetweetedAcc(this.right.lessRetweetedAcc(lRetweeted))
  }
  
  def descendingByRetweet: TweetList= this.descendingByRetweetAcc(Nil)
    
  def descendingByRetweetAcc(tweetsList: TweetList): TweetList= 
    // we use mostRetweeted recursively (every time we remove the mostRetweeted Tweet and we add it to the list) until there is no more elements in the list
    this.remove(this.lessRetweeted).descendingByRetweetAcc(new Cons(this.lessRetweeted,tweetsList))
   /**
   * The following methods are already implemented
   */
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  //this Unit is like a void in JAVA  
  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
 
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}
/*this is an object instead of a class because would be and object instance from class Nil that ALWAYS
   has the same data so...we define it as an object instead of a class but still extending from trait TweetList*/
object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

/*IMP in the Cons class the val in the parameters of the class constructor means that they are
at the same time the parameters and the field value definition in the class*/
class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  
  /*we use this breakable as we need to break the loop as soon the text contains the string*/
  @tailrec
  def containAnyKey(keyList:List[String],tweetText:String):Boolean={          
      /*Implemented with recursion , this way is more efficient
      (no break needed, no return explicit needed, no mutable var needed) 
      and get get full styleCheck Points*/
      if(keyList.isEmpty) false //if we loop the full list means that thre is nio match ...thus false..... is not contained
      else if (tweetText.contains(keyList.head)) true
      else containAnyKey(keyList.tail,tweetText)
      
      /*Implemented with for loop and break, less elegant and less efficient
      var contains:Boolean = false
      val loop = new Breaks;
      loop.breakable {
         for(text <- keyList){
            if (tweetText.contains(text)) {
               contains=true
               loop.break;
            }
         }
      }
      return contains*/   
    }
  
  /*IMP: val is executed when it is defined whereas a lazy val is executed when it is accessed the first time.
   lazy val is executed once and then never again (never change its value like val) , def every time that is accessed is calculated
   This feature helps not only delaying expensive calculations, but is also useful to construct mutual dependent or cyclic structures
   trying to access a lazy val from other object if it hasn't been initialized will throw a null pinter exception*/
  lazy val googleTweets: TweetSet =     
    TweetReader.allTweets.filter(tw=>containAnyKey(google,tw.text))
    /*IMP : vive scala and functions:
     filter has as param a funcion p: Tweet => Boolean, we have created a function containAnyKey that returns a Boolean
     and receives a List and a Tweet (the tweet of the param side of the function p: Tweet => Boolean ) ) 
     thus we can do this call with filter(tw=>containAnyKey(google,tw.text)) as
     containAnyKey(google,tw.text) returns a boolean, using tw as the Tweet param (tw is not defined when we call filter 
     as inside filterAcc we apply this function over a Tweet object p(Tweet) )
     
     IMP: Is an anonymus function tw is not defined and inside the method filter we really get the parameter that we are using to 
     evaluate that function, is like the exercises x=>x*x ,x doesn't have a previous value when we use that anonymus function
     to call the def, inside the method we call the anonymus function x=>x*x with specific params
     */
  
  lazy val appleTweets: TweetSet = 
     TweetReader.allTweets.filter(tw=>containAnyKey(apple,tw.text))
  //this version is more compact and uses exists as per the assignment hints
  //lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(p => google.exists(q => p.text.contains(q)))
   

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}
//another way for main extend from App
object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
 
}
