package week7_Lazy_Eval


//The base logic of all this is in the  week 7.4 paper notes
class Pouring(capacity:Vector[Int]) {
  
  //States
  
  type State = Vector[Int]
  val initialState = capacity map (x=>0)
  
  //Moves
  
  trait Move{
    //how move change the states
    def change(state:State):State
    
  }
  case class Empty(glass:Int) extends Move {
    def change(state:State):State = state updated (glass,0)    
  }
  case class Fill(glass:Int) extends Move{
	  def change(state:State):State = state updated (glass,capacity(glass))    
  }
  case class Pour(from:Int,to:Int) extends Move{
    //more complicated as we have to check if a glass fully fit into the other
    def change(state:State):State = {
      val amount =state(from) min (capacity(to)-state(to))
      state updated (from, state(from)-amount) updated (to, state(to) + amount)      
    }
  }
  
  
  //generate all possible moves
  val glasses = 0 until capacity.length
  
  val moves =
     (for (g <- glasses ) yield Empty(g)) ++
     (for (g <- glasses ) yield Fill(g)) ++
     (for (from <- glasses; to <- glasses if from!=to ) yield Pour(from,to))

  //Paths
  class Path(history: List[Move]) {
    //this solution is more difficult to see for beginners , the recursive solution is also valir!!
    def endState: State = (history foldRight initialState)(_ change _)

    //  def endState : State = trackState(history)
    //  private def trackState(xs:List[Move]): State = xs match {
    //     case Nil => initialState
    //     case move :: xs1 =>move change trackState(xs1)      
    //  }

    def extend(move: Move) = new Path(move :: history)

    //we need to override the toString method to print it friendly
    override def toString = (history.reverse mkString " ") + "-->" + endState
  }
  val initialPath = new Path(Nil)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        //explored is where we store the explored states, in order to do not repeat useless states
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more ,explored ++ (more map (_.endState)))
    }
  val pathSets = from(Set(initialPath), Set(initialState))
  
  def solution(target:Int): Stream[Path] = 
    for{
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    }yield path
}