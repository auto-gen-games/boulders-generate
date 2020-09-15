package org.drsimonmiles.rocks

import org.drsimonmiles.rocks.PuzzleCreation.createSolver
import org.drsimonmiles.util.Matrix.updated
import org.drsimonmiles.util.TimeOut.timeOutFromNow
import scala.collection.mutable.ArrayBuffer

sealed trait Setting
case object Yes extends Setting
case object No extends Setting
case object Unspecified extends Setting

object Setting {
  def apply (boolean: Boolean): Setting = if (boolean) Yes else No
}

final case class Position (x: Int, y: Int) {
  def at (otherX: Int, otherY: Int): Boolean = x == otherX && y == otherY
}

/**
  * Represents a rocks puzzle. All cells outside the playing area are assumed to have walls, floor and ceiling but no boulder.
  *
  * @param man The starting position of the man.
  * @param exit The position of the exit.
  * @param star The position of the star, if any.
  * @param walls A 2D array (x then y indices) of whether each cell has a wall on its left.
  * @param floors A 2D array (x then y indices) of whether each cell has a floor.
  * @param boulders A 2D array (x then y indices) of whether each cell has a boulder in it at start (not possible for leftmost and rightmost x).
  */
final case class Puzzle (man: Position, exit: Position, star: Position,
                   walls: List[List[Setting]], floors: List[List[Setting]], boulders: List[List[Setting]]) {
  def hasBoulder (x: Int, y: Int): Boolean = x > 0 && x < width - 1 && boulders (x - 1) (y) == Yes
  def hasExit (x: Int, y: Int): Boolean = exit.at (x, y)
  def hasFloor (x: Int, y: Int): Boolean = y < 0 || y >= height - 1 || x < 0 || x >= width || floors (x)(y) == Yes
  def hasLeftWall (x: Int, y: Int): Boolean = x <= 0 || x >= width || y < 0 || y >= height || walls (x - 1)(y) == Yes
  def hasMan (x: Int, y: Int): Boolean = man.at (x, y)
  def hasStar (x: Int, y: Int): Boolean = star.at (x, y)
  val height: Int = boulders.head.size
  def inRange (x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
  def knowBoulder (x: Int, y: Int): Boolean = x <= 0 || x >= width - 1 || y < 0 || y >= height || boulders (x - 1)(y) != Unspecified
  def knowFloor (x: Int, y: Int): Boolean = y < 0 || y >= height - 1 || x  < 0 || x >= width || floors (x)(y) != Unspecified
  def knowLeftWall (x: Int, y: Int): Boolean = x <= 0 || x >= width || y < 0 || y >= height || walls (x - 1)(y) != Unspecified
  val width: Int = boulders.size + 2
}

object Puzzle {
  def apply (width: Int, height: Int, man: Position, exit: Position, star: Position): Puzzle = {
    def empty (across: Int, down: Int): List[List[Setting]] = List.fill[Setting] (across, down) (Unspecified)
    new Puzzle (man, exit, star, empty (width - 1, height), empty (width, height - 1), empty (width - 2, height))
  }

  /** Returns a puzzle with all Unspecified walls, floors and boulders set to No, where these settings will be interpreted
    * as equivalent when trying to solve it. */
  def toFullyDefined (puzzle: Puzzle): Puzzle = {
    def defined (setting: Setting): Setting = if (setting == Yes) Yes else No
    def allDefined (settings: List[List[Setting]]): List[List[Setting]] = settings.map (_.map (defined))
    puzzle.copy (walls = allDefined (puzzle.walls), floors = allDefined (puzzle.floors), boulders = allDefined (puzzle.boulders))
  }

  /** Sets whether there is a boulder at the given position of the puzzle if possible, or returns None if not. */
  def setBoulder (puzzle: Puzzle, x: Int, y: Int, present: Boolean): Option[Puzzle] =
  // If already set, check that this is in range and set as requested.
    if (puzzle.knowBoulder (x, y))
      if (puzzle.inRange (x, y) && puzzle.hasBoulder (x, y) == present) Some (puzzle) else None
    else
      // If the request is to place a boulder, check it doesn't clash with the man, exit or star
      if (present && (puzzle.hasMan (x, y) || puzzle.hasExit (x, y) || puzzle.hasStar (x, y))) None
      // Finally, check that putting the boulder here does not cause it to be trapped in a corner
      else checkCornering (puzzle.copy (boulders = updated (puzzle.boulders, x - 1, y, Setting (present))), x, y)

  /** Sets whether there is a boulder at each of the given positions of the puzzle if possible, or returns None if not. */
  def setBoulders (puzzle: Puzzle, positions: List[Position], present: Boolean): Option[Puzzle] = positions match {
    case Nil => Some (puzzle)
    case head :: tail => setBoulder (puzzle, head.x, head.y, present).flatMap (setBoulders (_, tail, present))
  }

  def setFloor (puzzle: Puzzle, x: Int, y: Int, present: Boolean): Option[Puzzle] =
    if (puzzle.knowFloor (x, y))
      if (puzzle.inRange (x, y) && puzzle.hasFloor (x, y) == present) Some (puzzle) else None
    else
      checkTrap (puzzle.copy (floors = updated (puzzle.floors, x, y, Setting (present))))

  def setLeftWall (puzzle: Puzzle, x: Int, y: Int, present: Boolean): Option[Puzzle] =
    if (puzzle.knowLeftWall (x, y))
      if (puzzle.inRange (x, y) && puzzle.hasLeftWall (x, y) == present) Some (puzzle) else None
    else
      checkCornering (puzzle.copy (walls = updated (puzzle.walls, x - 1, y, Setting (present))), x, y).
        flatMap (checkCornering (_, x - 1, y)).flatMap (checkTrap)

  def toCode (puzzle: Puzzle): String = {
    import puzzle._
    width + "," + height + ";" + man.x + "," + man.y + ";" + exit.x + "," + exit.y + ";" +
      star.x + "," + star.y + ";" +
      (0 until height).flatMap (y => (0 until width).map (x =>
        if (hasFloor (x, y)) if (hasLeftWall (x, y)) 'F' else if (hasBoulder (x, y)) 'E' else 'D'
        else if (hasLeftWall (x, y)) 'C' else if (hasBoulder (x, y)) 'B' else 'A'
      )).mkString
  }

  def toString (puzzle: Puzzle)(implicit config: SolvingCommand): String = {
    import puzzle._
    val outer = "+" + (for (_ <- 0 until width) yield "-+").mkString
    def within (y: Int) = "|" + (for (x <- 0 until width) yield {
      (if (hasBoulder (x, y)) "O" else if (hasMan (x, y)) "M" else if (hasExit (x, y)) "X" else if (hasStar (x, y)) "*" else " ") + (if (hasLeftWall (x + 1, y)) "|" else " ")
    }).mkString
    def under (y: Int) = "+" + (for (x <- 0 until width) yield {(if (hasFloor (x, y)) "-" else " ") + "+"}).mkString
    val grid = outer + "\n" + (for (y <- 0 until height) yield {within (y) + "\n" + under (y) + "\n"}).mkString
    createSolver (timeOutFromNow (config.maxSolveTime))(puzzle) match {
      case Some (solution) => s"$grid\nlength:${Metrics.length (solution)}, weaving:${Metrics.weaving (puzzle, solution)}\n$solution\n"
      case None => s"$grid\nNo solution found\n"
    }
  }

  /** Checks whether the given puzzle has a boulder at the given position trapped against a wall, exit or another boulder,
    * returning None if so or the puzzle if not. */
  private def checkCornering (puzzle: Puzzle, x: Int, y: Int): Option[Puzzle] =
    if (puzzle.hasBoulder (x, y) && (puzzle.hasLeftWall (x, y) || puzzle.hasExit (x - 1, y) ||
      puzzle.hasLeftWall (x + 1, y) || puzzle.hasExit (x + 1, y) || puzzle.hasBoulder (x - 1, y) ||
      puzzle.hasBoulder (x + 1, y)))
      None
    else Some (puzzle)

  /** Checks whether the man is trivially prevented from reaching the exit by the walls and floors of the puzzle,
    * ignoring gravity and boulders. */
  private def checkTrap (puzzle: Puzzle): Option[Puzzle] = {
    import puzzle._
    // Boolean array of whether each position is accessible by the man, initially all false
    val accessible = ArrayBuffer.fill (width, height) (false)
    // Mark accessibility moving in all directions from the given position
    def markFrom (x: Int, y: Int): Unit =
      if (!accessible (x)(y)) {     // Ignore if this position is already known to be accessible
        accessible (x)(y) = true    // Mark this position as accessible, then in each unblocked direction
        if (!hasLeftWall (x, y)) markFrom (x - 1, y)
        if (!hasLeftWall (x + 1, y)) markFrom (x + 1, y)
        if (!hasFloor (x, y - 1)) markFrom (x, y - 1)
        if (!hasFloor (x, y)) markFrom (x, y + 1)
      }
    // Mark all accessible positions starting from the man's starting position
    markFrom (man.x, man.y)
    // Once marked, check that the exit and star are accessible, return the puzzle if so
    if (accessible (exit.x)(exit.y) && accessible (star.x)(star.y)) Some (puzzle) else None
  }

  def fromCode (code: String): Option[Puzzle] = {
    def coordinate (point: String): Position = {
      val coordinates = point.split (",")
      Position (coordinates (0).toInt, coordinates (1).toInt)
    }
    val parts = code.split (";")
    val area = coordinate (parts (0))
    val width = area.x
    val height = area.y
    val man = coordinate (parts (1))
    val exit = coordinate (parts (2))
    val star = coordinate (parts (3))
    var puzzle: Option[Puzzle] = Some (Puzzle (width, height, man, exit, star))
    val cells = if (parts.length == 4) parts (3) else parts (4)
    for (y <- 0 until height; x <- 0 until width) {
      puzzle = cells (y * width + x) match {
        case 'B' => puzzle.flatMap (setBoulder (_, x, y, present = true))
        case 'C' => puzzle.flatMap (setLeftWall (_, x, y, present = true))
        case 'D' => puzzle.flatMap (setFloor (_, x, y, present = true))
        case 'E' => puzzle.flatMap (setBoulder (_, x, y, present = true)).flatMap (setFloor (_, x, y, present = true))
        case 'F' => puzzle.flatMap (setFloor (_, x, y, present = true)).flatMap (setLeftWall (_, x, y, present = true))
        case _ => puzzle
      }
    }
    puzzle
  }
}
