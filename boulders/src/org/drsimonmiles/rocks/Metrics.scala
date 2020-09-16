package org.drsimonmiles.rocks

import org.drsimonmiles.rocks.Game.{getAvailableMoves, move}
import org.drsimonmiles.rocks.Solve.solve
import scala.annotation.tailrec

object Metrics extends App {
  /*def loadData (): List[(Int, Int, Int, Int)] =
    for ((puzzle, index) <- IO.load (Configuration.puzzlesFile).zipWithIndex) yield {
      print (".")
      solve (Game (puzzle))(() => false) match {
        case Some (solution) => (index, length (solution), 0 /*breadth (puzzle)*/, weaving (puzzle, solution))
        case None => (index, 0, 0, 0)
      }
    }*/

  /** Returns the set of possible game states found by breadth-first search until the solution is reached.
    * Game states are counted by the variety of different vertical positions of boulders.
    * The intuition for this is that the player moving or the player pushing a boulder along sideways may be
    * a recoverable change whereas pushing a boulder to fall is not. */
  def challenge (puzzle: Puzzle)(implicit definition: Definition): Int = {
    @tailrec
    def breadth (games: List[Game], tried: List[Game]): Int = games match {
      case Nil => tried.length
      case game :: rest =>
        if (games.head.hasWon)
          tried.map (_.boulders.toVector.map (_._2)).toSet.size
        else {
          // Get the next available states and try those for which no pushing was involved before others on the list.
          // This is because they are likely to be equivalent states to the starting state, i.e. can move back again.
          val moved = getAvailableMoves (game)(definition).map (move (game, _)).filterNot (tried.contains).toList
            .partition (_.boulders == game.boulders)
          breadth (moved._1 ::: rest ::: moved._2, game :: tried)
        }
    }

    breadth (List (Game (puzzle)), Nil)
  }

  /** Returns the number of non-fall moves in the given puzzle solution */
  def length (solution: List[Move]): Int =
    solution.count (_ != Fall)

  /** Returns the number of times a position is visited more than once in a solution, denoting the lack of
    *  linearity of the solution */
  def weaving (puzzle: Puzzle, solution: List[Move]): Int = {
    // The list of positions visited in the solution
    val pos = solution.foldLeft (List (Game (puzzle)))((gs, m) => move (gs.head, m) :: gs).map (g => (g.x, g.y))
    // Add up how many repeat visits to a position occur
    pos.map (p => pos.count (_ == p) - 1).sum
  }
}
