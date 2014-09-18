package week3

object rationals {

	//the .scala Object that contains the Rational class is in the same pkg we can use it without imports

  val x = new Rational(1,3)                       //> x  : week3.Rational = 1/3
  val y = new Rational(5,7)                       //> y  : week3.Rational = 5/7
  val z = new Rational(3,2)                       //> z  : week3.Rational = 3/2
  
  
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 3
  x + y                                           //> res2: week3.Rational = 22/21
  x.add(y)                                        //> res3: week3.Rational = 22/21
  //IMP also valid x add y
  //               x less y
  x.sub(y).sub(z)                                 //> res4: week3.Rational = -79/42
  x.less(y)                                       //> res5: Boolean = true
  x < y                                           //> res6: Boolean = true
  x.max(y)                                        //> res7: week3.Rational = 5/7
  //val strange = new Rational(1,0)
  new Rational(2)                                 //> res8: week3.Rational = 2/1
}