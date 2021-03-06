object InitializationSequence {
  //LAZY VALS GENERAL:
  
  /*-Is a good practice in seq prog to initialize the lazy val with an expression that does not
  depends on the current state, in concurrent prog is more importante as lazy vals are affected
  by non determinis*/
  //-A lazy val is initialized at most once, if it;s not required it will never be initizalized
  //-Everytime that we access a lazy val the initialized status needs to be checked
  //-If a lazy val is throwing an exception the first time is gonna be considered uninitialized and it´s ging to try next time in a later access
  //-Lazy val is not a silver bullet BE CAREFUL: https://blog.codecentric.de/en/2016/02/lazy-vals-scala-look-hood/
  //-If we have a lazy val (one or more) inside an object, when we initialize any of the lazy vals we lock (with a synch block) the whole instance, thus
  // the object access is blocked (other threads cannot access) and all the lazy vals inside are initilialized sequentially
  
  //Solution for null pointers in abstracts :
  //-Override val/def abstract with a lazy val
  //-use early initializer
   
  trait A {
    lazy val x:String = "x"
    //-we cannot have an abstract lazy val, this value will never be null in the parent as its a lazy val and will be evaluated when needed (just once)
    val z:String = "z"
    val p:String
    var j:String
    def y:String
    println("trait A :" + x + " ," + z + " , " + p + " , " + j + " , " + y)
  }
  
  trait B extends A {
    
    override lazy val x = "override with Lazy"
    
    //-If we override z in trait's A initialization is going to be null as that field is NOT going to be initialized in the superclass
    override val z = "overriden z value"
    
    //-Implementing an abstract val with a lazy val is going to avoid null, as the value of p will be evaluated when needed (just once)
    lazy val p = "implementing p with a lazy val"
    
    //-IMP!!!! Cannot override/implement a var with a val, we cannot go from a more stable version to a more unstable
    //-abstract/concrete def with : def,val, lazy val
    //-abstract val with : val, lazy val
    //-concerte val with a val
    //-abstract/concrete var with  : var
    var j = "implement with var"
    
    //-def is evaluated when needed (EVERYTIME), so it's not going to be null in the trait A inititalization even if it's abstract in trait A.
    //-We can also override/implement the def with a lazy val then it is going to be initialized in the superclass avoiding the null
    //-If we had overriden/implement def y with a val in the parent the value would be null!! as that val will not be initialized in the supeclass (same as val z)
    def y = "implementing with def"
  }
  
  //This is an anonymous class using a trait
  new B {override val z="override with val"}      //> trait A :override with Lazy ,null , implementing p with a lazy val , null ,
                                                  //|  implementing with def
                                                  //| res0: InitializationSequence.B{} = InitializationSequence$$anonfun$main$1$$
                                                  //| anon$1@2cb0730e
////////////////////////////////////////
  
  trait C {
    val x1: String
    println(x1)
  }
  //-IMP early initializer are no valid in traits (when we trying to create an anonymous class), they are not initilized before the parent
  
  //-Early init example in the class definition and mixing a trait
  class D extends {
    val x1: String = "hello"
  } with C {
    println(x1)
  }
  new D                                           //> hello
                                                  //| hello
                                                  //| res1: InitializationSequence.D = InitializationSequence$$anonfun$main$1$D$1
                                                  //| @48fa2a5c
  //-New D overriding a value when created (we need the override modifier is not as when we define an abstract one)
  new D {override val x1: String = "hello 2"}     //> null
                                                  //| null
                                                  //| res2: InitializationSequence.D{} = InitializationSequence$$anonfun$main$1$$
                                                  //| anon$2@6f6c0ff5
  //-Early init example with ANONYMOUS mixing C
  new {
    val x1: String = "hello"
  } with C {
    println(x1)
  }                                               //> hello
                                                  //| hello
                                                  //| res3: InitializationSequence.C{} = InitializationSequence$$anonfun$main$1$$
                                                  //| anon$3@15ce859d
  //-IMP Subclases don´t inherit private members (val,var,defs) !!!!
  //-IMP You can't/shouldn't override values and methods on a Object/Companion Object
}