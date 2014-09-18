package week3

object intsets {
	val t1 = new NonEmpty(3,Empty,Empty)
  val t2 = t1 incl 4
}
//If a class hasn't any superclass, the superclass will be Object like in Java
//As well as in JAVA, SCALA only can have one suerclass (single inheritance) but ...check TRAIT
abstract class IntSet {
		//IntSet class abstract that is why the def functs dont have body
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
 		def union(other: IntSet): IntSet
}

//object definition is the same as class but the diference is that is a singleton
//we only have one that is created at the begining
//IMP: SINGLETON OBJECT
object Empty extends IntSet {

  def contains(x: Int): Boolean = false
  
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  //and as is a singleton object we call it with Empty to refecence the object instead of
  //the previous new Empty that was creating another object previously
  
  def union(other: IntSet): IntSet = other
  
  override def toString = "."
  //It is also possible to redefine an existing, non-abstract definition in a subclass by using override
}

//IMP!!!! spot that the abstract class DOESN'T hava params and this sub class DOES
//Anyway the abtract class is not instanciable so I guess that the params dont make any sense
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
    
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x) else this
  
  def union(other: IntSet): IntSet =
		  /*IMP : This implementstion fo union is veri ineficient if we do the same in the week 3 objset exercise
		  it will give us a timeout on the test , in the objset project is the right implementation*/
   		((left union right)union other) incl elem
   		//how do we know that this recursion terminates???
   		//because the left side is always smaller
   		/*IMP: in this union the new object is being built on the way back of the recursion
	 		we need to go to the deeper part of the trees and build the new one
			 */
   
  override def toString = "{" + left + elem + right + "}"
   
}

//these trees are called persist data structures , as even when you make changes
//the old structure remains the same (see video 1 of week 3)


abstract class Base {
    def foo = 1
    def bar: Int
}
class Sub extends Base {
  //overriding a non abstrac method needs override modifier,
  //if is an abstract method we dont need override, but you can write it if you want(not good plactice)
  override def foo = 2
  def bar = 3
}


//like in java (and object oriented languajes) scala has DYNAMIC METHOD DISPATCH :
//This means that the code invoked by a method call
//depends on the runtime type of the object that contains the method.
//Dynamic dispatch of methods is analogous to calls to higher-order functions.