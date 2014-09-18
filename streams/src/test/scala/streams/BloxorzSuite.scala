package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    //IMP here we are not instantiating the trait, we are creating an anonymous class that implicitly extends the trait
    //the result is all that methods executed with the trait values
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      println(terrain(Pos(0,0)))
      println(terrain(Pos(4,0)))
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      println(startPos)
      assert(startPos == Pos(1,1))
    }
  }
  
  test("blocks, == equals and eq") {
    new Level1 {
      //comparing objects in scala, do not put the result in the println as is forcing to a toString thus the
      //eq evaluation iss not reliable as eq is comparing THE REFFERENCE
      val pos1 = Pos(1,2)
      val pos2 = Pos(1,2)
      println ("same objects content : == result ")
      println (pos1 == pos2)
      println ("same objects content : equals result ")
      println (pos1 equals pos2)
      println ("same objects content : eq result ")
      println (pos1 eq pos2)
      //
      
      println(startBlock)
      println(startBlock.isStanding)
      println(startBlock.neighbors)
      println(startBlock.legalNeighbors)
    }
  }
  
  test("neighborsWithHistory") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet == Set(
			  (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
			  (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
			))
    }
  }
  
  test("newNeighborsOnly") {
     new Level1 {      
              
      assert(newNeighborsOnly(
			  Set(
			    (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
			    (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
			  ).toStream,Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))) == Set(
			    (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
			  ).toStream)
    }
  }
  
  test("from method") {
    new Level1 {
      //println(List((Block(Pos(3,2),Pos(3,2)),List(Left, Down, Down, Right, Up, Right, Down)), (Block(Pos(3,5),Pos(3,5)),List(Right, Down, Down, Right, Up, Right, Down)), (Block(Pos(3,5),Pos(4,5)),List(Down, Right, Down, Right, Up, Right, Down)), (Block(Pos(2,6),Pos(2,7)),List(Right, Right, Down, Right, Up, Right, Down)), (Block(Pos(3,1),Pos(3,2)),List(Down, Down, Left, Up, Right, Right, Down)), (Block(Pos(2,3),Pos(2,3)),List(Right, Down, Left, Up, Right, Right, Down)), (Block(Pos(0,0),Pos(0,0)),List(Left, Up, Left, Up, Right, Right, Down)), (Block(Pos(3,4),Pos(3,5)),List(Down, Down, Right, Up, Right, Right, Down)), (Block(Pos(2,3),Pos(2,3)),List(Left, Down, Right, Up, Right, Right, Down)), (Block(Pos(2,6),Pos(2,6)),List(Right, Down, Right, Up, Right, Right, Down)), (Block(Pos(0,0),Pos(0,0)),List(Up, Left, Up, Left, Down, Down, Right)), (Block(Pos(3,2),Pos(3,2)),List(Down, Right, Up, Left, Down, Down, Right)), (Block(Pos(3,3),Pos(3,3)),List(Down, Left, Up, Right, Down, Down, Right)), (Block(Pos(3,5),Pos(3,5)),List(Down, Right, Up, Right, Down, Down, Right)), (Block(Pos(4,7),Pos(4,7)),List(Right, Down, Right, Right, Down, Down, Right)), (Block(Pos(4,7),Pos(5,7)),List(Down, Right, Right, Right, Down, Down, Right)), (Block(Pos(3,8),Pos(3,9)),List(Right, Right, Right, Right, Down, Down, Right)), (Block(Pos(4,7),Pos(4,7)),List(Right, Down, Down, Right, Right, Down, Right)), (Block(Pos(4,7),Pos(5,7)),List(Down, Right, Down, Right, Right, Down, Right)), (Block(Pos(3,8),Pos(3,9)),List(Right, Right, Down, Right, Right, Down, Right)), (Block(Pos(5,7),Pos(5,7)),List(Down, Down, Right, Right, Right, Down, Right)), (Block(Pos(3,6),Pos(4,6)),List(Left, Down, Right, Right, Right, Down, Right)), (Block(Pos(3,8),Pos(4,8)),List(Right, Down, Right, Right, Right, Down, Right)), (Block(Pos(5,6),Pos(5,7)),List(Down, Right, Down, Right, Down, Right, Right)), (Block(Pos(3,6),Pos(3,7)),List(Up, Right, Down, Right, Down, Right, Right)), (Block(Pos(4,8),Pos(4,8)),List(Right, Right, Down, Right, Down, Right, Right)), (Block(Pos(4,7),Pos(4,8)),List(Right, Down, Right, Right, Down, Right, Right)), (Block(Pos(4,7),Pos(4,7)),List(Down, Right, Right, Right, Down, Right, Right)), (Block(Pos(2,8),Pos(3,8)),List(Right, Right, Right, Right, Down, Right, Right))).partition(x => x._1.b1 == goal && x._1.isStanding))
      println(from(Set((Block(Pos(1,1),Pos(1,1)), List())).toStream,Set(Block(Pos(1,1),Pos(1,1)))).toList)
      println(pathsFromStart.toList)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
