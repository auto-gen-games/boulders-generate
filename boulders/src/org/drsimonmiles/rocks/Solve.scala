package org.drsimonmiles.rocks

import org.drsimonmiles.agg.PathSearch._
import org.drsimonmiles.rocks.Game.{getAvailableMoves, perform}
import org.drsimonmiles.rocks.Metrics.length

object Solve {
  /** Returns the length of a path of moves */
  private def pathLength (moves: List[Move]): Double =
    length (moves).toDouble

  /** Calculates the heuristic distance from the current position to the next objective (star or exit) */
  private val distance = (game: Game) =>
    if (game.diamondRemains)
      ((game.x - game.diamond.get.x).abs + (game.y - game.diamond.get.y).abs).toDouble
    else
      ((game.x - game.exit.x).abs + (game.y - game.exit.y).abs).toDouble

  /** Perform the given move in the given game state and return the resulting state (if possible) */
  private val act: (Game, Move) => Option[Game] = (game: Game, move: Move) => perform (game, move)

  /** Solve the given game state, with a timeout function given */
  def solve (game: Game)(implicit definition: Definition): Option[List[Move]] =
    aStarSearch [Move, Game](game)(pathLength)(distance)(getAvailableMoves)(act)(_.hasWon).map (_.toList)

  def toString (solution: Option[List[Move]]): String =
    solution match {
      case None => "No solution found"
      case Some (moves) => moves.map (Move.toString).mkString
    }
}
