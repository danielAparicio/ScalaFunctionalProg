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
  
  //This is an anonymous class using a trait
  new B {val z="override with val"}               //> override with Lazy ,override with def , null , null
                                                  //| res0: InitializationSequence.B{} = InitializationSequence$$anonfun$main$1$$a
                                                  //| non$1@2e746d6d
  
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
                                                  //| res1: InitializationSequence.D = InitializationSequence$$anonfun$main$1$D$1@
                                                  //| 7a3f437c
  
  //new D overriding a value when created
  new D {override val x1: String = "hello 2"}     //> null
                                                  //| null
                                                  //| res2: InitializationSequence.D{} = InitializationSequence$$anonfun$main$1$$
                                                  //| anon$2@3afa6240
  
  //early init example with ANONYMOUS mixing C
  new {
    val x1: String = "hello"
  } with C {
    println(x1)
  }                                               //> hello
                                                  //| hello
                                                  //| res3: InitializationSequence.C{} = InitializationSequence$$anonfun$main$1$$
                                                  //| anon$3@693b004c
  //IMP: Subclases don´t inherit private members (val,var,defs) !!!!
  
}