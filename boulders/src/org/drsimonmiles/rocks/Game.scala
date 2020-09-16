package org.drsimonmiles.rocks

import org.drsimonmiles.rocks.Puzzle.flippedPuzzle
import scala.annotation.tailrec

final case class Game (puzzle: Puzzle, x: Int, y: Int, boulders: Set[(Int, Int)], diamondRemains: Boolean, flipRemains: Boolean = false) {
  val exit: Position = puzzle.exit
  def hasBoulder (bx: Int, by: Int): Boolean = boulders.contains (bx, by)
  def hasExit (ex: Int, ey: Int): Boolean = puzzle.hasExit (ex, ey)
  def hasFloor (fx: Int, fy: Int): Boolean = puzzle.hasFloor (fx, fy)
  def hasLeftWall (wx: Int, wy: Int): Boolean = puzzle.hasLeftWall (wx, wy)
  def hasPlayer (mx: Int, my: Int): Boolean = x == mx && y == my
  def hasDiamond (sx: Int, sy: Int): Boolean = diamondRemains && puzzle.hasDiamond (sx, sy)
  val hasWon: Boolean = !diamondRemains && hasExit (x, y)
  val height: Int = puzzle.height
  val diamond: Option[Position] = if (diamondRemains) Some (puzzle.diamond) else None
  val width: Int = puzzle.width
  override lazy val toString: String = s"($x,$y)" + (if (diamondRemains) "*" else "") + " / " + boulders.map (b => s"(${b._1},${b._2})").mkString (" ")
}

object Game {
  /** Create the initial game state from the given puzzle. */
  def apply (puzzle: Puzzle): Game = Game (puzzle, puzzle.player.x, puzzle.player.y,
    (for (bx <- 1 until puzzle.width - 1; by <- 0 until puzzle.height; if puzzle.hasBoulder (bx, by)) yield (bx, by)).toSet, true)

  /** Determines whether a boulder at the given position would, at that position or after falling, squash
    * the player, diamond or exit. */
  @tailrec
  def squashes (fx: Int, fy: Int, game: Game): Boolean =
    game.hasPlayer (fx, fy) || game.hasExit (fx, fy) || game.hasDiamond (fx, fy) ||
      (!game.hasFloor (fx, fy) && !game.hasBoulder (fx, fy + 1) && squashes (fx, fy + 1, game))

  /** Determines whether any boulder in the game starting from its current position would squash the player,
    * diamond or exit. */
  def anySquashes (game: Game): Boolean =
    game.boulders.exists (pos => squashes (pos._1, pos._2, game))

  /** Can the given move be taken in the given game state? */
  def canMove (game: Game, move: Move): Boolean = {
    import game._
    // Checks whether, if the move pushed a boulder, that boulder is not blocked from moving nor would squash diamond or exit
    def canPush: Boolean = move match {
      case WalkLeft => !hasLeftWall (x - 1, y) && !hasBoulder (x - 2, y) && !squashes (x - 2, y, game)
      case WalkRight => !hasLeftWall (x + 2, y) && !hasBoulder (x + 2, y) && !squashes (x + 2, y, game)
      case JumpLeft => !hasLeftWall (x - 1, y - 1) && !hasBoulder (x - 2, y - 1) && !squashes (x - 2, y - 1, game)
      case JumpRight => !hasLeftWall (x + 2, y - 1) && !hasBoulder (x + 2, y - 1) && !squashes (x + 2, y - 1, game)
      case _ => false
    }
    // Checks whether the move would cause a boulder to squash the player
    def wouldntBeSquashed: Boolean = move match {
      case WalkLeft  => !hasBoulder (x - 1, y - 1) || hasFloor (x - 1, y - 1)
      case WalkRight => !hasBoulder (x + 1, y - 1) || hasFloor (x + 1, y - 1)
      case JumpLeft  => !hasBoulder (x - 1, y - 2) || hasFloor (x - 1, y - 2)
      case JumpRight => !hasBoulder (x + 1, y - 2) || hasFloor (x + 1, y - 2)
      case Flip      => !anySquashes (flippedGame (game))
      case _ => true
    }
    // Check whether a wall or non-pushable boulder blocks the move
    def unblocked: Boolean = move match {
      case WalkLeft => !hasLeftWall (x, y) && (!hasBoulder (x - 1, y) || canPush)
      case WalkRight => !hasLeftWall (x + 1, y) && (!hasBoulder (x + 1, y) || canPush)
      case JumpLeft => !hasFloor (x, y - 1) && !hasLeftWall (x, y - 1) && (!hasBoulder (x - 1, y - 1) || canPush)
      case JumpRight => !hasFloor (x, y - 1) && !hasLeftWall (x + 1, y - 1) && (!hasBoulder (x + 1, y - 1) || canPush)
      case Jump => !hasFloor (x, y - 1)
      case Fall => !hasFloor (x, y) && !hasBoulder (x, y + 1)
      case _ => true
    }
    def stillFalling: Boolean = move != Fall && canMove (game, Fall)
    def alreadyFlipped: Boolean = move == Flip && !game.flipRemains
    // Possible to move if the game is not already won, and
    //  the player is not trying to do anything but fall when there is no support below, and
    //  the player, diamond and exit would not be squashed by the move, and
    //  the player is not trying to flip when that move is no longer available
    //  the player is not blocked
    !hasWon && !alreadyFlipped && wouldntBeSquashed && !stillFalling && unblocked
  }

  /** For a boulder at (bx, by), let it fall until blocked by a floor or other boulder, return the resultant game state */
  @tailrec
  def drop (game: Game, bx: Int, by: Int): Game =
    if (game.hasFloor (bx, by) || game.hasBoulder (bx, by + 1)) game
    else drop (game.copy (boulders = game.boulders - ((bx, by)) + ((bx, by + 1))), bx, by + 1)

  /** Drop all the boulders in the game until blocked by a floor or a lower boulder. */
  def dropAll (game: Game): Game =
    game.boulders.toVector.sortBy (_._2).reverse.foldLeft (game){
      case (state, position) => drop (state, position._1, position._2)
    }

  /** Let the man fall until blocked by a floor or other boulder, return the resultant game state */
  @tailrec
  def fallToGround (game: Game): Game =
    if (game.hasFloor (game.x, game.y) || game.hasBoulder (game.x, game.y + 1) || game.hasWon) game
    else fallToGround (move (game, Fall))

  /** Return all the moves available in the current game state. */
  def getAvailableMoves (game: Game)(implicit definition: Definition): Seq[Move] =
    definition.moves.filter (canMove (game, _))

  /** Attempt to perform a move in a game, returning the new game state if successful or the old state if not. */
  def move (game: Game, move: Move): Game = perform (game, move) match {
    case Some (newState) => newState
    case None => game
  }

  /** Checks whether the player has reached the diamond and marks it collected if so. */
  def collectDiamond (game: Game): Game =
    game.copy (diamondRemains = game.diamondRemains && !game.hasDiamond (game.x, game.y))

  /** Sets the new position and removes the diamond from an initial game state if the player has moved onto it, returning the game state */
  def after (initial: Game, newX: Int, newY: Int): Game =
    collectDiamond (initial.copy (x = newX, y = newY))

  /** Attempt to perform a move in a game state, and if successful return the new game state or None if not. */
  @tailrec
  def perform (game: Game, move: Move): Option[Game] = {
    import game._
    /** Move the boulder at position (bx, by) in the direction given (-1 left, +1 right), and return the new game state. */
    def push (bx: Int, by: Int, direction: Int): Game =
      drop (game.copy (boulders = boulders - ((bx, by)) + ((bx + direction, by))), bx + direction, by)

    // If we can't perform the move, return None
    if (!canMove (game, move)) None
    else
      // If we can move, first move any boulder pushed then move the man, or if no boulder then check whether the star is collected
      move match {
        case WalkLeft => if (hasBoulder (x - 1, y)) perform (push (x - 1, y, direction = -1), WalkLeft) else Some (after (game, x - 1, y))
        case WalkRight => if (hasBoulder (x + 1, y)) perform (push (x + 1, y, direction = 1), WalkRight) else Some (after (game, x + 1, y))
        case Jump => Some (after (game, x, y - 1))
        case Fall => Some (after (game, x, y + 1))
        case JumpLeft => if (hasBoulder (x - 1, y - 1)) perform (push (x - 1, y - 1, direction = -1), JumpLeft) else Some (after (after (game, x, y - 1), x - 1, y - 1))
        case JumpRight => if (hasBoulder (x + 1, y - 1)) perform (push (x + 1, y - 1, direction = 1), JumpRight) else Some (after (after (game, x, y - 1), x + 1, y - 1))
        case Flip => Some (dropAll (flippedGame (game)))
      }
  }

  def flippedGame (game: Game): Game =
    Game (flippedPuzzle (game.puzzle), game.x, game.puzzle.height - 1 - game.y,
      game.boulders.map (pos => (pos._1, game.puzzle.height - 1 - pos._2)), game.diamondRemains, flipRemains = false)

  def toString (game: Game): String = {
    import game._
    val outer = "+" + (for (_ <- 0 until width) yield "-+").mkString
    def within (y: Int) = "|" + (for (x <- 0 until width) yield {
      (if (hasBoulder (x, y)) "O" else if (hasPlayer (x, y)) "M" else if (hasExit (x, y)) "X" else if (hasDiamond (x, y)) "*" else " ") + (if (hasLeftWall (x + 1, y)) "|" else " ")
    }).mkString
    def under (y: Int) = "+" + (for (x <- 0 until width) yield {(if (hasFloor (x, y)) "-" else " ") + "+"}).mkString
    outer + "\n" + (for (y <- 0 until height) yield {within (y) + "\n" + under (y) + "\n"}).mkString
  }
}
