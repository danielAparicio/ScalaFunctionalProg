package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    
    val s0 = singletonSet(0)
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)    
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
    val s1000 = singletonSet(-1000)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
      
      val sInt = intersect(s1, s2)
      assert(!contains(sInt, 1), "Intersection 1")
      
    }    
  }
  test("forall and exists") {
    new TestSets {
            
      val s1234 = union(union(union(s1, s2), s3), s4)
      val s34 = union(s3,s4)
      val s45 = union(s4,s5)
      val s56 = union(s5,s6)
      val s01000 = union(s0,s1000)
      
      printSet(s1234)
      printSet(s34)
      printSet(s45)
      printSet(s56)
      printSet(s01000)
      
      assert(forall(s34, s1234), "foral 1")
      assert(!forall(s45, s1234), "foral 2")
      assert(!forall(s56, s1234), "foral 3")
      assert(forall(s1234, x => x<5), "foral less than 5")
      assert(!forall(s1234, x => x<4), "foral less than 4")
      assert(forall(s01000, x => x<1000), "foral less than 1000")
      
      assert(!exists(s1234, x => x>4), "exists more than 4")
      assert(exists(s1234, x => x>3), "exists more than 3")
      assert(exists(s1234, s34), "foral 1")
      assert(exists(s1234, s45), "foral 2")
      assert(!exists(s1234, s56), "foral 3")
      
    }
  }
  
  test("map") {
    new TestSets {
      
      val s1234 = union(union(union(s1, s2), s3), s4)
      //map2 is the version of map in function of exists
      val mapSet = map2(s1234, x=>x*x)
      printSet(s1234)
      printSet(mapSet)
      assert(contains(mapSet, 16), "map contains 16")
      assert(!contains(mapSet, 15), "map doesn't contains 15")
      
    } 
  }
}
