package org.drsimonmiles.agg

import org.drsimonmiles.itapprox.{Choice, Decision}
import org.drsimonmiles.itapprox.ControlledWander.wander
import org.drsimonmiles.util.GeneratedMap
import org.drsimonmiles.util.Measure.measure

object PuzzleRefiner {
  /**
    * Generate a puzzle by iterative exploratory refinement.
    *
    * @param initialPuzzle The opening puzzle state from which to refine to a better puzzle.
    * @param initialChoices The choices to be made from the opening puzzle state.
    * @param hopelessLength How many steps to follow a choice path without any improvement to the puzzle before abandoning it
    * @param terminate Returns true when a timeout has expired and the current best solution should be returned.
    * @param followOn Returns the decisions that must be made on the given puzzle following the given decision.
    * @param improve Returns choices on the given solved puzzle that are non-compulsory but could improve it.
    * @param solve Attempts to solve the given puzzle, returning a solution if there is one.
    * @param acceptable Returns whether the given puzzle and its solution are acceptable to be returned.
    * @param better Returns whether the first given solved puzzle is better than the second.
    * @tparam Puzzle The type of a puzzle.
    * @tparam Solution A solution to a puzzle.
    * @return The refined puzzle found, if any.
    */
  def generate[Puzzle, Solution] (initialPuzzle: Puzzle,
                                  initialChoices: List[Choice[Puzzle]],
                                  hopelessLength: Int,
                                  terminate: () => Boolean,
                                  followOn: (Decision[Puzzle], Puzzle) => List[Choice[Puzzle]],
                                  improve: (Puzzle, Option[Solution]) => Option[Choice[Puzzle]],
                                  solve: Puzzle => Option[Solution],
                                  acceptable: (Puzzle, Solution) => Boolean,
                                  better: ((Puzzle, Solution), (Puzzle, Solution)) => Boolean): Option[Puzzle] = {
    val solution = bufferedSolve (solve)
    def acceptablePuzzle (puzzle: Puzzle): Boolean =
      org.drsimonmiles.util.Logger.record (org.drsimonmiles.util.Logger.probe (puzzle, solution (org.drsimonmiles.util.Logger.trace (puzzle))).exists (measure ("PR.acceptable", acceptable (puzzle, _))))
      //solution (puzzle).exists (measure ("PR.acceptable", acceptable (puzzle, _)))
    def improvePuzzle (puzzle: Puzzle) = improve (puzzle, solution (puzzle))
    val bestPuzzle = betterIfSolveable (solve, better)
    val looksHopeless = hopelessByLength (hopelessLength, solution, better)

    measure ("wander", wander[Puzzle] (initialPuzzle, initialChoices)(terminate)(followOn)(acceptablePuzzle)(improvePuzzle)(bestPuzzle)(looksHopeless))
  }

  def bufferedSolve[Puzzle, Solution] (solve: Puzzle => Option[Solution]): Puzzle => Option[Solution] = {
    val buffer = new GeneratedMap[Puzzle, Option[Solution]](solve)
    puzzle: Puzzle => buffer.apply (puzzle)
  }

  /**
    * Creates a function that returns false (meaning: keep exploring) only if the given path of puzzle refinements is
    * less than hopelessLength or if there has been an improvement in the last hopelessLength steps of puzzle refinement.
    * The puzzle refinement path is assumed to be ordered from most recent first.
    *
    * @param hopelessLength The minimum refinement path length before considering stopping
    * @param solve Returns a solution for the puzzle, if any exists
    * @param better Returns true if the first solvable puzzle is better than the second
    */
  def hopelessByLength[Puzzle, Solution] (hopelessLength: Int, solve: Puzzle => Option[Solution],
                                          better: ((Puzzle, Solution), (Puzzle, Solution)) => Boolean): List[Puzzle] => Boolean =
    (path: List[Puzzle]) =>
      // If the path length is at least hopelessLength, consider returning true (hopeless) else return false
      if (path.length >= hopelessLength) {
        // Get the most recent hopelessLength steps in the path, ordered from oldest first
        val recent = path.take (hopelessLength).reverse
        // Check whether the oldest step in the most recent path has a solution
        solve (recent.head) match {
          // If it does have a solution, return true (hopeless) only if every more recent puzzle either has no solution
          //  or the solution is no better than the oldest one
          case Some (solution) =>
            recent.tail.forall (r => {
              val moreRecentSolution = solve (r)
              moreRecentSolution.isEmpty || !better ((r, moreRecentSolution.get), (recent.head, solution))
            })
          // If it does not have a solution, return true (hopeless) if none of the recent puzzles have a solution
          case None =>
            recent.tail.forall (solve (_).isEmpty)
        }
      } else false

  /** Creates a function to determine whether one puzzle is better than another, testing first whether each has a solution,
    * and then comparing the solved puzzles only if both do. */
  def betterIfSolveable[Puzzle, Solution] (solve: Puzzle => Option[Solution],
                                           better: ((Puzzle, Solution), (Puzzle, Solution)) => Boolean): (Puzzle, Puzzle) => Puzzle =
    (puzzle1: Puzzle, puzzle2: Puzzle) => (solve (puzzle1), solve (puzzle2)) match {
      case (None, None) => puzzle1
      case (Some (_), None) => puzzle1
      case (None, Some (_)) => puzzle2
      case (Some (solution1), Some (solution2)) =>
        if (better ((puzzle1, solution1), (puzzle2, solution2))) puzzle1 else puzzle2
  }
}
