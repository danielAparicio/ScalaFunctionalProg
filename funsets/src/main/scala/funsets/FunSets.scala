package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  //Set is a type that receives an Int and returns a Boolean
  type Set = Int => Boolean
  //Here for example a val that is defined as a function
  //val power : ( Int, Int ) => Int  =  ( base: Int, exp :Int ) => if( exp <= 1 ) base else base * power( base, exp - 1 )

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  //IMP: this is a function that returns a Set that is another function is the parameter that the function receives
  //as you see we only use element  in the Boolean response. 
  //x is the Int param (we need the () as we are returning a function with format (param: Type) => Boolean expression using the params of de def) 
  //x is passed BUT NOT in this Singleton set.
  /*
   Uses of : =>
   
   1) Call-by-Name: => Type
   def f(x: => Int) = x * x
   
   2) 0-arity Functions: () => Type
   () => Unit  (Function with empty params)
   (x:Int) => Boolean (this is what this def singletonSet set is returning)
   
   3) as function param:
   def product(f: Int => Int )(a: Int,b:Int):Int
   def pepe(f: (Int,Int) => Int )(a: Int,b:Int):Int
   
   4) anonymus fucntion:
   this is a call to the previous def product
   product(x=>x*x)(3,4) 
   
   5) pattern matching 
   list() => XXXXX 
   */
  def singletonSet(elem: Int): Set = (x: Int) => elem==x

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s,a) && !contains(p,a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = forall(s,p) || !(forall(s,(x:Int) => !contains(p,x))) 

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  //The fastest way to do this is apply inverted function....MINDSET
  //Here we have using a block as function result (x:Int) => {boolean}  , in this case boolean
  def map(s: Set, f: Int => Int): Set = (x:Int) =>  {    
    def iter(a: Int): Boolean = {
      if (a > bound) false
      //if the original Set s contains a and the value x (that is the value that we want to check if belongs to the 
      //transformed Set) is result of the function of the new map applied to the a element then x BELONGS to the new Set
      else if (contains(s,a) && (x == f(a))) true
      else iter(a + 1)
    }
    iter(-bound)    
  }
  //Same map implemented in terms of exists
  def map2(s: Set, f: Int => Int): Set = x => exists(s,y => f(y)==x)

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
