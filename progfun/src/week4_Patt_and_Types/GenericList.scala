package week4_Patt_and_Types

import java.util.NoSuchElementException

/*IMP [T] here we are using GENERICS
trait List[+T] { is for the covariance example making Nill an object*/
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  //IMP : this is not working because a covariant type parameter +T
  //cannot be used as a function parameter (should be a contravariant)
  //def prepend(elem:T)   : List[T]    = new Cons(elem,this)  
  
  //we can fix it with a lower bound           , IMPORTANT upper and lower bounds CAN BE USED without Generics, but of course compared with and SPECIFIC TYPE 
  //def prepend[U :> T] (elem:U)   : List[U]    = new Cons(elem,this)
  //covariant (used for function results) +T type params appears in lower bounds (like this example)
  //contravariant (used for function parameters) -T in upper bounds
}

/*Lower bound in class parameters with covariant type
 class Slot[+T, V :> T](var some: V) {
} 
*/

/*IMP in the Cons class the val in the parameters of the class constructor means that they are
at the same time the parameters and the field value definition in the class*/
class Cons[T](val head: T,val tail: List[T]) extends List[T]{
  def isEmpty = false   
}

/*IMP covariance example : Nil could be an object
 1) we remove the type to Nil as it's an object (single instance)
 2) T is not a bound parameter anymore , if we put for example String we are making a restriction to a
 list of String so we will make a list of Nothing that is the subtype of every other type
 3) we make List[T] type parameter covariant List[+T]
object Nil extends List[Nothing] { 
 */

class Nil[T] extends List[T] {
  //check that in the class above we did def isEmpty = false ,WITHOUT the Type, directly the VALUE
  def isEmpty :Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  //is OK to have Nothing as value as Nothing is a subtype of any other type
  //def tail= throw new NoSuchElementException("Nil.tail") is also valid,including the return TYPE
}

/* covariance example
 object test{
 val x: List[String] = Nil
 This example is having Nil as an object, if we don't do List[T] convariant (List[+T]) 
 as Nil (List[Nothing]) is not a subtype of List[String], adding a covariant the compiler checks that 
 List[Nothing] is a subtype of List[String]
 } 
 */

/*
 check the 3rd lecture of the 3rd week about
 -Type as parameters in functions
 -Type inference
 -Type erasure
 -Polymorphism: 1)subtyping (tradicionally OO first) 2)generics (tradicionally FP first) 
*/
