object InitializationSequence {
  
  //Solution for null pointers in abstracts :
  //Override val/def abstract with a lazy val
  //use early initializer
  
  
  trait A {
    val x:String
    val z:String
    var j:String
    def y:String
    println(x + " ," + y + " , " +z +" , "+j)
  
  }
  trait B extends A {
  
    lazy val x = "override with Lazy"
    def y = "override with def"
    //IMP:Cannot override a var with a val
    var j = "override with var"
   
  }
  
  //This is an anonymous class
  new B {val z="override with val"}               //> override with Lazy ,override with def , null , null
                                                  //| res0: InitializationSequence.B{} = InitializationSequence$$anonfun$main$1$$a
                                                  //| non$1@50625c49
  

  //IMP early initializer are no valid in traits, they are not initilized before the parent
  //early init example
  abstract class C {
    val x1: String
    println(x1)
  }
  class D extends {
    val x1: String = "hello"
  } with C {
    println(x1)
  }
  new D                                           //> hello
                                                  //| hello
                                                  //| res1: InitializationSequence.D = InitializationSequence$$anonfun$main$1$D$1@
                                                  //| 69637577
  
}