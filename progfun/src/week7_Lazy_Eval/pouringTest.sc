package week7_Lazy_Eval

object test {
  //the capacity of every glass int he vector
  val problem = new Pouring(Vector(4,7))          //> problem  : week7_Lazy_Eval.Pouring = week7_Lazy_Eval.Pouring@fc54796
  problem.moves                                   //> res0: scala.collection.immutable.IndexedSeq[Product with Serializable with w
                                                  //| eek7_Lazy_Eval.test.problem.Move] = Vector(Empty(0), Empty(1), Fill(0), Fill
                                                  //| (1), Pour(0,1), Pour(1,0))
                                 
  problem.pathSets.take(3).toList                 //> res1: List[Set[week7_Lazy_Eval.test.problem.Path]] = List(Set(-->Vector(0, 0
                                                  //| )), Set(Fill(0)-->Vector(4, 0), Fill(1)-->Vector(0, 7)), Set(Fill(0) Fill(1)
                                                  //| -->Vector(4, 7), Fill(0) Pour(0,1)-->Vector(0, 4), Fill(1) Fill(0)-->Vector(
                                                  //| 4, 7), Fill(1) Pour(1,0)-->Vector(4, 3)))
  
  
  problem.solution(6)                             //> res2: Stream[week7_Lazy_Eval.test.problem.Path] = Stream(Fill(1) Pour(1,0) E
                                                  //| mpty(0) Pour(1,0) Fill(1) Pour(1,0)-->Vector(4, 6), ?)
 
  
}