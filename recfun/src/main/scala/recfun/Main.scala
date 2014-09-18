package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int) = {

    def isEdge(c: Int, r: Int): Boolean = r == 0 || (c == 0 || c == r)    
    
    def pascalCalc(c: Int, r: Int): Int =
      if (isEdge(c, r)) 1 else pascalCalc(c, r - 1) + pascalCalc(c - 1, r - 1)

    pascalCalc(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]) = {

    def balanceChars(chars: List[Char], count: Int): Int = 
      if (count < 0 || chars.isEmpty) count else 
        balanceChars(chars.tail, count + sumfunction(chars.head))

    def sumfunction(c: Char): Int = if (c == '(') 1 else  if (c == ')') -1 else 0 

    balanceChars(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]) = {

    def countChangeWays(m: Int, c: List[Int], count: Int): Int =
      if (m == 0) count + 1 else
        if (m<0 || c.isEmpty) count else countChangeWays(m-c.head, c, count) + countChangeWays(m, c.tail, count)
        
    if (money == 0 || coins.length == 0) 0 else (countChangeWays(money, coins, 0))
    
  }
}
