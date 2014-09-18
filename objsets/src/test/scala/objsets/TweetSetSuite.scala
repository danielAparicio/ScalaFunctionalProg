package objsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.hamcrest.core.IsInstanceOf

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set6 = (new Empty).incl(c).incl(d)
    def loop(set:TweetSet,count:Int):TweetSet = {
      if(count==321) set
      else loop(set.incl(new Tweet(count.toString,count.toString,count)),count+1)
  }
    val set7=loop(new Empty,0)
    
  }
  

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("filter: 20 on set7") {
    new TestSets {
      println(size(set7.filter(tw => tw.retweets == 20)))
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("union: set5 with set7") {
    new TestSets {
      println(size(set5.union(set7)))
    }
  }
  
  test("most and less retweeted") {
    new TestSets {
      
      assert(set3.mostRetweeted.retweets === 20)
      println(set3.mostRetweeted.retweets) 
      
      println(set5.mostRetweeted.retweets)
      println(set5.lessRetweeted.retweets)
      assert(set5.lessRetweeted.retweets === 7) 
      assert(set5.mostRetweeted.retweets === 20)      
      
      println(set6.mostRetweeted.retweets)
      println(set6.lessRetweeted.retweets)
      assert(set6.lessRetweeted.retweets === 7)
      assert(set6.mostRetweeted.retweets === 9)
      
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      trends foreach println
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
  test("descending: set7") {
    new TestSets {
      val trends = set7.descendingByRetweet
      assert(!trends.isEmpty)
    }
  }
}
