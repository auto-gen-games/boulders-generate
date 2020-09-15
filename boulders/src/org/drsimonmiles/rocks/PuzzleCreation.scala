package org.drsimonmiles.rocks

import org.drsimonmiles.agg.PuzzleRefiner.generate
import org.drsimonmiles.itapprox.{Both, Choice, Decision}
import org.drsimonmiles.itapprox.Decision.{firstBiasedOr, only}
import org.drsimonmiles.rocks.Game.{move, perform}
import org.drsimonmiles.rocks.Metrics.length
import org.drsimonmiles.rocks.Puzzle.setBoulders
import org.drsimonmiles.rocks.Solve.solve
import org.drsimonmiles.util.{Logger, Measure}
import org.drsimonmiles.util.TimeOut.timeOutFromNow
import scala.collection.mutable.ArrayBuffer
import scala.util.Random.{nextInt => randomInt}

object PuzzleCreation {
  /** Generate a random puzzle and return it if successful. */
  def createPuzzle (width: Int, height: Int, hopelessLength: Int, timePerAttempt: Long, minLength: Int)(implicit config: CreateCommand): Option[(Puzzle, List[Move])] = {
    // Create the initial grid with man, star and exit placed
    val initialPuzzle = createInitialGrid (width, height)
    // The initial choices regard how to ensure the man does not fall and neither man nor star is squashed at the start
    val initialChoices = Nil ++ supportChoice (initialPuzzle.man, width, height) ++ notSquashedChoice (initialPuzzle.man) ++ notSquashedChoice (initialPuzzle.star)
    // Time limit on attempting to generate this puzzle
    val terminate = timeOutFromNow (timePerAttempt)
    val solver = createSolver (terminate)
    val acceptableLength = acceptablyHard (minLength)(_, _)

    // Generate a puzzle using puzzle refinement, and perform a final check that the solution is acceptably hard
    generate (initialPuzzle, initialChoices, hopelessLength, terminate, consequences, solver, acceptableLength, harder,
      Puzzle.toFullyDefined, Game.apply, makeHarder, couldEnable).
      flatMap (puzzle => createSolver (timeOutFromNow (timePerAttempt))(puzzle).map (solution => (puzzle, solution))).
      filter (puzzleSolution => acceptableLength (puzzleSolution._1, puzzleSolution._2))
  }

  /** Generate the initial grid, with a man, exit and star set. */
  def createInitialGrid (width: Int, height: Int): Puzzle = {
    // First choose the position of the man (in left half of grid, to avoid generating symmetrical puzzles), exit and star
    val man = randomPosition (minX = 0, minY = 0, maxX = Math.round (width.toFloat / 2), maxY = height, disallowed = Nil)
    val exit = randomPosition (minX = 0, minY = 0, maxX = width, maxY = height, disallowed = List (man))
    val star = randomPosition (minX = 0, minY = 0, maxX = width, maxY = height, disallowed = List (man, exit))
    // The initial puzzle has no boulders at the same positions as the man, exit or star
    setBoulders (Puzzle (width, height, man, exit, star), List (exit, man, star), false).get
  }

  /** Generate a random position within the given bounds (minX and minY inclusive, maxX and maxY exclusive) and
    *  excluding disallowed positions. */
  def randomPosition (minX: Int, minY: Int, maxX: Int, maxY: Int, disallowed: List[Position]): Position = {
    val position = Position (minX + randomInt (maxX - minX), minY + randomInt (maxY - minY))
    if (disallowed.contains (position)) randomPosition (minX, minY, maxX, maxY, disallowed)
    else position
  }

  /** Define a solver for a puzzle with a timeout based on the current time */
  def createSolver (timeOut: () => Boolean): Puzzle => Option[List[Move]] = {
    puzzle: Puzzle => solve (Game (puzzle))(timeOut)
  }

  /** Create the choice, if any, that needs to be made on how the given man start position should be supported. */
  def supportChoice (man: Position, width: Int, height: Int)(implicit config: CreateCommand): Option[Choice[Puzzle]] =
  // If the man is on the lowest level, there are no choices required to be made
    if (man.y == height - 1) None
    // If the man is at the extreme left or right, only a floor can support him (no boulders can start cornered)
    else if (man.x == 0 || man.x == width - 1) Some (Choice (SetFloor (man.x, man.y, true)))
    // If anywhere else on the grid, then there is a choice of a floor or boulder or both beneath the man
    else Some (firstBiasedOr (config.biasAgainstBoulders, SetFloor (man.x, man.y, true), SetBoulder (man.x, man.y + 1, true)))

  /** Create the choice, if any, that needs to be made to ensure the given starting position is not squashed by a boulder. */
  def notSquashedChoice (position: Position)(implicit config: CreateCommand): Option[Choice[Puzzle]] =
  // If the man is at the top of the grid, he can't be squashed
    if (position.y == 0) None
    // Else choose between the man having a ceiling or no boulder or both above him
    else Some (firstBiasedOr (config.biasAgainstBoulders, SetFloor (position.x, position.y - 1, true), SetBoulder (position.x, position.y - 1, false)))

  /** Return the consequential choices that need to be made following a particular decision */
  def consequences (decision: Decision[Puzzle], puzzle: Puzzle)(implicit config: CreateCommand): List[Choice[Puzzle]] = decision match {
    // The consequences of placing a boulder are that we need to decide what is under that boulder: floor, boulder or both
    case SetBoulder (x, y, present) if present && !puzzle.hasFloor (x, y) =>
      Logger.log (s"")
      List (only (SetBoulder (x - 1, y, false)), only (SetBoulder (x + 1, y, false)), only (SetLeftWall (x, y, false)),
        only (SetLeftWall (x + 1, y, false)), firstBiasedOr (config.biasAgainstBoulders, SetFloor (x, y, true), SetBoulder (x, y + 1, true)))
    // Any decision to make two decisions has combined consequences of both
    case Both (decisionA, decisionB) =>
      consequences (decisionA, puzzle) ::: consequences (decisionB, puzzle)
    // Other decisions do not require choices to be made
    case _ => Nil
  }

  /** Returns true if the decision made could have changed the solution to the resulting puzzle */
  def changesSolution (decision: Decision[Puzzle], puzzle: Puzzle): Boolean = decision match {
    case SetBoulder (x, y, present) => present
    case SetFloor (x, y, present) => present
    case SetLeftWall (x, y, present) => present
    case Both (decisionA, decisionB) => changesSolution (decisionA, puzzle) || changesSolution (decisionB, puzzle)
  }

  /** Returns true if the given potential decision has already been made on the puzzle */
  def decided (decision: Decision[Puzzle], puzzle: Puzzle): Boolean = decision match {
    case SetBoulder (x, y, present) => puzzle.knowBoulder (x, y)
    case SetFloor (x, y, present) => puzzle.knowFloor (x, y)
    case SetLeftWall (x, y, present) => puzzle.knowLeftWall (x, y)
    case Both (decisionA, decisionB) => decided (decisionA, puzzle) && decided (decisionB, puzzle)
  }

  /** Returns the first of the given decisions that regards something undecided in the puzzle, or None if none. */
  def firstUndecided (puzzle: Puzzle, decisions: List[Decision[Puzzle]]): Option[Decision[Puzzle]] = decisions match {
    case Nil => None
    case head :: tail => Some (head).filter (!decided (_, puzzle)).orElse (firstUndecided (puzzle, tail))
  }

  /** Returns the first choice which could block a given move in a given game state, if any */
  def couldBlockMove (game: Game, nextMove: Move): Option[Decision[Puzzle]] = {
    import game._
    firstUndecided (puzzle, nextMove match {
      case WalkLeft =>
        if (hasBoulder (x - 1, y)) List (SetLeftWall (x, y, true), SetLeftWall (x - 1, y, true))
        else List (SetLeftWall (x, y, true), SetBoulder (x - 1, y, true))
      case WalkRight =>
        if (hasBoulder (x + 1, y)) List (SetLeftWall (x + 1, y, true), SetLeftWall (x + 2, y, true))
        else List (SetLeftWall (x + 1, y, true), SetBoulder (x + 1, y, true))
      case Jump => List (SetFloor (x, y - 1, true))
      case Fall => List (SetFloor (x, y, true), SetBoulder (x, y + 1, true))
      case JumpLeft =>
        if (hasBoulder (x - 1, y - 1)) List (SetFloor (x, y - 1, true), SetLeftWall (x, y - 1, true), SetLeftWall (x - 1, y - 1, true))
        else List (SetFloor (x, y - 1, true), SetLeftWall (x, y - 1, true), SetBoulder (x - 1, y - 1, true))
      case JumpRight =>
        if (hasBoulder (x + 1, y - 1)) List (SetFloor (x, y - 1, true), SetLeftWall (x + 1, y - 1, true), SetLeftWall (x + 2, y - 1, true))
        else List (SetFloor (x, y - 1, true), SetLeftWall (x + 1, y - 1, true), SetBoulder (x + 1, y - 1, true))
    })
  }

  /** Returns the first decision which could block any move on a path from a game state, if any */
  def couldBlock (game: Game, path: List[Move]): Option[Decision[Puzzle]] = path match {
    case Nil => None
    case head :: tail => couldBlockMove (game, head).orElse (couldBlock (move (game, head), tail))
  }

  def makeHarder (puzzle: Puzzle, solution: List[Move]): Option[Decision[Puzzle]] =
    couldBlock (Game (puzzle), solution)

  /** For the given list of moves and the game states they move into, try to find a decision that could add more
    *  options to the man's movement on the grid. */
  def firstEnablerIn (states: List[Move], enact: Move => Option[Game]): Option[Decision[Puzzle]] = states match {
    case Nil => None
    case move :: others =>
      val toState = enact (move)
      (move, toState) match {
        // If the move cannot be enacted, try the rest
        case (_, None) => firstEnablerIn (others, enact)
        // If falling, then try putting a floor to prevent the fall (if not already decided no floor)
        case (Fall, Some (g)) if !g.puzzle.knowFloor (g.x, g.y - 1) => Some (SetFloor (g.x, g.y - 1, true))
        // If moving left or right, then try putting a boulder to push (if not already decided no boulder)
        case (WalkLeft, Some (g)) if !g.puzzle.knowBoulder (g.x, g.y) => Some (SetBoulder (g.x, g.y, true))
        case (WalkRight, Some (g)) if !g.puzzle.knowBoulder (g.x, g.y) => Some (SetBoulder (g.x, g.y, true))
        // Otherwise, see if there is an enabler for the next move
        case _ => firstEnablerIn (others, enact)
      }
  }

  // Order of actions to try if the man is above and left of the star/exit
  val aboveLeftActions = List (Fall, WalkRight, JumpRight, WalkLeft, JumpLeft)
  // Order of actions to try if the man is above and right of the star/exit
  val aboveRightActions = List (Fall, WalkLeft, JumpLeft, WalkRight, JumpRight)
  // Order of actions to try if the man is below and left of the star/exit
  val belowLeftActions = List (Fall, JumpRight, JumpLeft, WalkRight, WalkLeft)
  // Order of actions to try if the man is below and right of the star/exit
  val belowRightActions = List (Fall, JumpLeft, JumpRight, WalkLeft, WalkRight)

  /** In the given game state, what order of actions would make sense to try given the relative position of the man, star and exit. */
  def orderedActions (game: Game): List[Move] = {
    // Target the star if present or the exit if not
    val target = game.star.getOrElse (game.exit)
    // Return the appropriate ordered list of actions given the direction to the target
    if (game.x < target.x) if (game.y < target.y) aboveLeftActions else belowLeftActions
    else if (game.y < target.y) aboveRightActions else belowRightActions
  }

  /** Tries to find a decision that may possibly make an unsolveable puzzle solveable, by seeing what supportive
    *  floors or boulders could help in paths reachable from the given game states, excluding those already tried. */
  def couldEnable (games: ArrayBuffer[Game], tried: ArrayBuffer[Game])(implicit config: CreateCommand): Option[Decision[Puzzle]] =
    if (games.isEmpty) None
    else {
      val game = games.head
      val actions = orderedActions (game)
      // Given those actions, find the first change to the puzzle that might enable more of the grid to be reachable
      val enabler = Measure.measure ("firstEnable", firstEnablerIn (actions, m => perform (game, m)))
      // If there is an enabler, return it
      if (enabler.isDefined) enabler
      // Otherwise, take all the game states the above actions reached, exclude those already tried, and look for enablers from each
      else {
        tried += game
        games.remove (0)
        games ++= actions.flatMap (move => perform (game, move)).filterNot (tried.contains)
        couldEnable (games, tried)
      }
    }

  /** Determine whether the difficulty of a given puzzle is acceptable, as determined by the number of non-fall
    *  moves in the solution (length) and the number of repeat visits to positions required (non-linearity/weaving)
    * Ignored for now (always true). */
  def acceptablyHard (minLength: Int)(puzzle: Puzzle, solution: List[Move]): Boolean =
    length (solution) >= minLength // length (solution) >= minLength && weaving (puzzle, solution) >= minWeaving

  /**
    * Determines whether the first given puzzle is harder than the second, according to the number of non-fall moves
    *  in the solution.
    */
  def harder (puzzleA: (Puzzle, List[Move]), puzzleB: (Puzzle, List[Move])): Boolean =
    puzzleA._2.length > puzzleB._2.length

}
