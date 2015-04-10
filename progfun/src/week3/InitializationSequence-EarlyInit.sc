object InitializationSequence {
  
  //LAZY VALS GENERAL:
  /*is a good practice in seq prog to initialize the lazy val with an expression that does not
  depends on the current state, in concurrent prog is more importante as lazy vals are affected
  by non determinism
  a lazy val is initialized at most once, if it;s not required it will never be initizalized*/
  
   
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
  
  //This is an anonymous class using a trait
  new B {val z="override with val"}               //> override with Lazy ,override with def , null , null
                                                  //| res0: InitializationSequence.B{} = InitializationSequence$$anonfun$main$1$$a
                                                  //| non$1@1c47cf34
  
////////////////////////////////////////
  
  trait C {
    val x1: String
    println(x1)
  }
  //IMP early initializer are no valid in traits (when we trying to create an anonymous class),
  //they are not initilized before the parent
  
  //early init example in the class definition and mixing a trait
  class D extends {
    val x1: String = "hello"
  } with C {
    println(x1)
  }
  new D                                           //> hello
                                                  //| hello
                                                  //| res1: InitializationSequence.D = InitializationSequence$$anonfun$main$1$D$1
                                                  //| @76de43f3
  
  //new D overriding a value when created
  new D {override val x1: String = "hello 2"}     //> null
                                                  //| null
                                                  //| res2: InitializationSequence.D{} = InitializationSequence$$anonfun$main$1$$
                                                  //| anon$2@2495223b
  
  //early init example with ANONYMOUS mixing C
  new {
    val x1: String = "hello"
  } with C {
    println(x1)
  }                                               //> hello
                                                  //| hello
                                                  //| res3: InitializationSequence.C{} = InitializationSequence$$anonfun$main$1$$
                                                  //| anon$3@c487600
                                                  
  //IMP: Subclases don´t inherit private members (val,var,defs) !!!!
  //IMP: You can't/shouldn't override values and methods on a Object/Companion Object
  
}