package streams

import common._

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  //we only need to check that one of the squares (b1 in this case) is in the goal position as the second condition is 'isStanding'
  //that means that b1 and b2 are in the same position
  def done(b: Block): Boolean = b.b1 == goal && b.isStanding
  //IMP: object comparison equals and ==
  //== is a final method, and calls .equals, which is not final.
  //This is radically different than Java, where == is an operator rather than a method and strictly compares reference equality for objects.
  //One welcome change in Scala is the change of semantics of the == operator. 
  //In Scala it is the same as doing a .equals comparison in Java. If you want identity comparison (references) you can use the eq method (and ne for the not equal comparator).

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   * 
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   * 
   * The function returns a stream of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   * 
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
 
    // converting to Stream after applying the map , less efficient ???
    //(b.legalNeighbors map (elem => (elem._1,elem._2::history))).toStream
    b.legalNeighbors.toStream map (elem => (elem._1,elem._2::history)) 
    //we get the legal neighbors with he gameDef method transform to stream and we add the 
    //previous history or movements
  }
    

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = {
    
	  def newNeighborsOnlyAcc(neighbors: Stream[(Block, List[Move])], validNeighbors: Stream[(Block, List[Move])]): Stream[(Block, List[Move])] = {
	    //this also could have been done with patterMatching
	    if(neighbors.isEmpty) validNeighbors
	    else {
    		//here we need to check if the current block in has been explored(take in account that we only iterate over the new 4 positions of the neighbors)
    		//if it has been explored we do not have to add it to the new stream
    		if(!explored.contains(neighbors.head._1)) newNeighborsOnlyAcc(neighbors.tail,neighbors.head #:: validNeighbors)
    		else newNeighborsOnlyAcc(neighbors.tail,validNeighbors)
	    }	    	
	  }
	  
      newNeighborsOnlyAcc(neighbors, Stream[(Block, List[Move])]())
  }
       
  /**
   * The function `from` returns the stream of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * stream.
   * 
   * The blocks in the stream `initial` are sorted by ascending path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the stream.
   * 
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * stream `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid cycles.
   * 
   * The resulting stream should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the stream.
   * 
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted stream.
   */
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = { 
    //IMPORTANT: recursion stop condition!!!
    if(initial.isEmpty) Stream.Empty
    else{
		val iteration = for{
			initialstate <- initial
			next <- {/*println("newcombs" + neighborsWithHistory(initialstate._1,initialstate._2).toList);println("result" + newNeighborsOnly(neighborsWithHistory(initialstate._1,initialstate._2),explored).toList);println("explored" + explored.toList);*/
					newNeighborsOnly(neighborsWithHistory(initialstate._1,initialstate._2),explored)} 
			} yield next
		
		//Wrong way : iteration.filter(x => x._1 == goal) ++ iteration.filterNot(x => x._1 == goal) 
		//IMP : for this we have partition that splits the collection in two based on a predicate
	    //IMP: span doesn't work as span :Returns the longest prefix of the list whose elements all satisfy the given predicate, and the rest of the list!!!!
		val streamSpan= iteration.partition(x => x._1.b1 == goal && x._1.isStanding)
		//we split the stream in two the first stream is the stream that has completed paths (one of the squares is in the Goal position plus the block isStanding)
		//println("completed : " + streamSpan._1.toList )
		//println("nextcall : " + streamSpan._2.toList )
		//println("explored : " + (explored ++ streamSpan._2.map(x=>x._1)).toList )
		streamSpan._1 ++ from(streamSpan._2, explored ++ streamSpan._2.map(x=>x._1))
    }		
  }

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = from(Stream((Block(startPos,startPos), List[Move]())),Set[Block](Block(startPos,startPos)))
  //initial stream with one block in the start position , empty moves list and the initial position as explored
  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = ???

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = {
    val solution = from(Stream((Block(startPos,startPos), List[Move]())),Set[Block](Block(startPos,startPos)))
    //if we don't have solution we should return the empty list
    if(solution.isEmpty) List()
    else solution.head._2.reverse
  } 
  //we call from from the start position the we get the head (that is the optimal or one of the optimal solutions) get the moves and reverse it as originally the last moves are in the head
}
