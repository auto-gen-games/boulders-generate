package org.drsimonmiles.agg

import org.drsimonmiles.itapprox.{Choice, Decision}
import org.drsimonmiles.itapprox.ControlledWander.wander
import org.drsimonmiles.itapprox.Decision.preferredDecision
import org.drsimonmiles.util.GeneratedMap
import org.drsimonmiles.util.Measure.measure
import scala.collection.mutable.ArrayBuffer

object PuzzleRefiner {
  /**
    * Generate a puzzle by iterative exploratory refinement.
    *
    * @param initialPuzzle The opening puzzle state from which to refine to a better puzzle.
    * @param initialChoices The choices to be made from the opening puzzle state.
    * @param hopelessLength How many steps to follow a choice path without any improvement to the puzzle before abandoning it
    * @param terminate Returns true when a timeout has expired and the current best solution should be returned.
    * @param followOn Returns the decisions that must be made on the given puzzle following the given decision.
    * @param solve Attempts to solve the given puzzle, returning a solution if there is one.
    * @param acceptable Returns whether the given puzzle and its solution are acceptable to be returned.
    * @param better Returns whether the first given solved puzzle is better than the second.
    * @param canonical Gets the canonical form of a puzzle, where all puzzles with the same canonical form have the same possible game states and solutions.
    * @param initialState Returns the opening game state of a puzzle.
    * @param makeHarder For a given puzzle and solution, try to find a decision that could make the puzzle require a harder solution.
    * @param makePossible Given an exploration state (game states tried and to try next) of an impossible puzzle, try to find a decision that could make the puzzle possible.
    * @tparam Puzzle The type of a puzzle.
    * @tparam Game A state in playing a puzzle.
    * @tparam Solution A solution to a puzzle.
    * @return The refined puzzle found, if any.
    */
  def generate[Puzzle, Game, Solution] (initialPuzzle: Puzzle,
                                        initialChoices: List[Choice[Puzzle]],
                                        hopelessLength: Int,
                                        terminate: () => Boolean,
                                        followOn: (Decision[Puzzle], Puzzle) => List[Choice[Puzzle]],
                                        solve: Puzzle => Option[Solution],
                                        acceptable: (Puzzle, Solution) => Boolean,
                                        better: ((Puzzle, Solution), (Puzzle, Solution)) => Boolean,
                                        canonical: Puzzle => Puzzle,
                                        initialState: Puzzle => Game,
                                        makeHarder: (Puzzle, Solution) => Option[Decision[Puzzle]],
                                        makePossible: (ArrayBuffer[Game], ArrayBuffer[Game]) => Option[Decision[Puzzle]]): Option[Puzzle] = {
    // Returns the solution of a puzzle, buffering to not need to re-solve solved puzzles
    val solution = bufferedSolve[Puzzle, Solution] (puzzle => solve (canonical (puzzle)))
    // Records what states are tried and are to try next per puzzle
    val exploreBuffer = new ExploreBuffer[Puzzle, Game] (canonical, initialState)

    /** Determines whether the given puzzle is acceptable. */
    def acceptablePuzzle (puzzle: Puzzle): Boolean = solution (puzzle).exists (acceptable (puzzle, _))
    /** Return a choice that could improve the given puzzle with the given solution (if any) */
    def improvePuzzle (puzzle: Puzzle): Option[Choice[Puzzle]] = solution (puzzle) match {
      // For a puzzle with a solution, try to make it harder by blocking the path
      case Some (path) => makeHarder (puzzle, path).map (preferredDecision)
      // For a puzzle without a solution, try to make it possible by placing an enabling element somewhere reachable
      case None => makePossible (exploreBuffer.toTryEnableStates (puzzle), exploreBuffer.triedEnableStates (puzzle)).map (preferredDecision)
    }

    val bestPuzzle = betterIfSolveable (solve, better)
    val looksHopeless = hopelessByLength (hopelessLength, solution, better)

    measure ("wander", wander[Puzzle] (initialPuzzle, initialChoices)(terminate)(followOn)(acceptablePuzzle)(improvePuzzle)(bestPuzzle)(looksHopeless))
  }

  /** Wraps the given solve method so that an existing solution is looked up before a new one is generated. */
  def bufferedSolve[Puzzle, Solution] (solve: Puzzle => Option[Solution]): Puzzle => Option[Solution] = {
    val buffer = new GeneratedMap[Puzzle, Option[Solution]] (solve)
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
