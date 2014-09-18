package week6

object Sets {

  //typical queens problem,in a N chess table with N queens
  //there can't be two queens in the same row, column, or diagonal
  def queens(n: Int) = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
         //we take the placeQueens(k-1) response for the for loop
         //IMP: llamada recursiva dentro del for, de manera que se empieza
         //colocando la primera reina
          queens <- placeQueens(k - 1)
          //for every queen we are doing col from o until n
          col <- 0 until n
          if isSafe(col, queens)
        }
        //add the safe col for n queen to the queens that we had from previous calls
        // all this with yield is added to the Set of the response
        yield col :: queens
    }
    placeQueens(n)
  }                                               //> queens: (n: Int)Set[List[Int]]
  
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    //the queen should be placed in the next available row, it means queens.lenght
  	val row = queens.length
  	val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {
    	case (r,c) => col !=c && math.abs(col - c) != row -r
    }
  }                                               //> isSafe: (col: Int, queens: List[Int])Boolean
  
  queens(4)                                       //> res0: Set[List[Int]] = Set(List(1, 3, 0, 2), List(2, 0, 3, 1))
  
  
  //in order to pretty print it
  def show(queens: List[Int]) = {
	 val lines =
	   for (col<- queens.reverse)
	   yield Vector.fill(queens.length)("* ").updated(col,"X ").mkString
	   //each line in lines will be separated by a line
	  "\n" + (lines mkString "\n")
  }                                               //> show: (queens: List[Int])String
  
  //we apply show to every member of queens call
  (queens(4) map show)                            //> res1: scala.collection.immutable.Set[String] = Set("
                                                  //| * * X * 
                                                  //| X * * * 
                                                  //| * * * X 
                                                  //| * X * * ", "
                                                  //| * X * * 
                                                  //| * * * X 
                                                  //| X * * * 
                                                  //| * * X * ")
  
  //make it pretty printed
}