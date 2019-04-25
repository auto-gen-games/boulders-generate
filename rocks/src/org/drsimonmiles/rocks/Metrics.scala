package org.drsimonmiles.rocks

import org.drsimonmiles.rocks.Game.{getAvailableMoves, move}
import org.drsimonmiles.rocks.Solve.solve
import scala.annotation.tailrec

object Metrics extends App {
  def loadData (): List[(Int, Int, Int, Int)] =
    for ((puzzle, index) <- IO.load (Configuration.puzzlesFile).zipWithIndex) yield {
      print (".")
      solve (Game (puzzle))(() => false) match {
        case Some (solution) => (index, length (solution), 0 /*breadth (puzzle)*/, weaving (puzzle, solution))
        case None => (index, 0, 0, 0)
      }
    }

  IO.saveMetrics ("metrics.csv", loadData ())
  println (" done")

  def breadth (puzzle: Puzzle): Int = {
    @tailrec
    def breadth (games: List[Game], tried: List[Game]): Int = games match {
      case Nil => tried.length
      case game :: rest => breadth (getAvailableMoves (game).map (move (game, _)).filterNot (tried.contains).toList ::: rest, game :: tried)
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
