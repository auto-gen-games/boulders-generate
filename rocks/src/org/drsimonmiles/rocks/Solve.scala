package org.drsimonmiles.rocks

import org.drsimonmiles.agg.PathSearch._
import org.drsimonmiles.rocks.Game.{getAvailableMoves, perform}

object Solve {
  private val distance = (game: Game) =>
    if (game.starRemains)
      ((game.x - game.star.get.x).abs + (game.y - game.star.get.y).abs).toDouble
    else
      ((game.x - game.exit.x).abs + (game.y - game.exit.y).abs).toDouble

  private val act = (game: Game, move: Move) => perform (game, move)

  def solve (game: Game)(terminate: () => Boolean): Option[List[Move]] = {
    //println ("Try to solve:")
    //println (Game.toString (game))
    val solution = aStarSearch [Move, Game](game)(terminate)(distance)(getAvailableMoves)(act)(_.hasWon).map (_.toList)
    //println ("Solved? " + solution.isDefined)
    solution
  }
}
