//we have removed the import week3 that is created by default

//if we import the pkg.class we can access the SCALA OBJECT(and all his classes inside) directly

import week3.Rational // imports just Rational NAMED import
import week3.{Rational, Hello} // imports both Rational and Hello NAMED import
import week3._ // imports everything in package week3 WILDCARD import

//IMP we only can import objects (.scala) no worksheets (.sc)

object scratch {

/*To access an specific class object in a .scala (SCALA OBJECT) or .sc (SCALA WORKSHEET) we only have to import the qualifier
progfun.examples.Hello , this is like (project).pkgName.Object
also in the command like with : scala progfun.examples.Hello
IMP should be in a scala class , a sc is not valid */

//IMP if the .scala or .sc is in the same pkf that the .scala that contains the class definition we can access the class object directly

new week3.Rational(1, 2)                          //> res0: week3.Rational = 1/2

//after the import import week3.Rational we can use
new Rational(1, 2)                                //> res1: week3.Rational = 1/2




def error(msg:String) = throw new Error(msg)      //> error: (msg: String)Nothing
//The type compuded by error is Nothing (is a subtype of every other type)
//Used for 1) abnormal termination (exceptions)   2) as an element type of empty collections
//error("test")

val x=null                                        //> x  : Null = null
val y: String = x                                 //> y  : String = null
//val z: Int = null
//null is incompatible with subtypes of AnyVal,null is a subtype of everytype that inherits from Object

if (true) 1 else false                            //> res2: AnyVal = 1
//we can return an Int or a Boolean and that is why we get AnyVal as both inherits from Scala.AnyVal

}

/*all this entities are imported automatically:"
All members of package scala
All members of package java.lang
All members of the singleton object scala.Predef.

Int                            scala.Int
Boolean                        scala.Boolean
Object                         java.lang.Object
require                        scala.Predef.require
assert                         scala.Predef.assert

Scala library
www.scala-lang.org/api/current

----------------------------------
TRAITS:(one class can inherit from several traits)

class Square extends Shape with Planar with Movable ...

Traits resemble interfaces in Java, but are more powerful because
they can contains fields and concrete methods.
On the other hand, traits cannot have (value) parameters, only classes can.
*/