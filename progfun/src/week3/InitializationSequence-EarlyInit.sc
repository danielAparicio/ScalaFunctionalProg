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
    lazy val x:String = "x"
    //we cannot have an abstract lazy val, this value will never be null in the parent as its a lazy val
    val z:String = "z"
    val p:String
    var j:String
    def y:String
    println("trait A :" + x + " ," + z + " , " + p + " , " + j + " , " + y)
  
  }
  trait B extends A {
    
    override lazy val x = "override with Lazy"
    
    //if we override z in trait's A initialization is going to be null
    override val z = "overriden z value"
    
    //implementing an abstract val with a lazy val is going to avoid null
    lazy val p = "implementing p with a lazy val"
    
    //IMP:Cannot override/implement a var with a val
    var j = "implement with var"
    
    //def is evaluated when needed, so it's not going to be null in the trait A inititalization even if it's abstract in trait A.
    //If we had overriden def y with a val instead of just defining def y in the parent the value would be null!!
    //as in the parent initialization is not a def anymore is a val that hasn't been initialized in the subclass
    def y = "implementing with def"
      
  }
  
  //This is an anonymous class using a trait
  new B {override val z="override with val"}      //> trait A :override with Lazy ,null , implementing p with a lazy val , null ,
                                                  //|  implementing with def
                                                  //| res0: InitializationSequence.B{} = InitializationSequence$$anonfun$main$1$$
                                                  //| anon$1@7451b30d
  
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
                                                  //| @1ff32372
  
  //new D overriding a value when created (we need the override modifier is not as when we define an abstract one)
  new D {override val x1: String = "hello 2"}     //> null
                                                  //| null
                                                  //| res2: InitializationSequence.D{} = InitializationSequence$$anonfun$main$1$$
                                                  //| anon$2@1f8bd6f9
  
  //early init example with ANONYMOUS mixing C
  new {
    val x1: String = "hello"
  } with C {
    println(x1)
  }                                               //> hello
                                                  //| hello
                                                  //| res3: InitializationSequence.C{} = InitializationSequence$$anonfun$main$1$$
                                                  //| anon$3@29a6119a
                                                  
  //IMP: Subclases don´t inherit private members (val,var,defs) !!!!
  //IMP: You can't/shouldn't override values and methods on a Object/Companion Object
  
}