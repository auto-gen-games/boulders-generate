package org.drsimonmiles.agg

import org.drsimonmiles.util.GeneratedMap
import scala.collection.mutable.ArrayBuffer

/**
  * Stores the game states of puzzles that are yet to be explored and are already explored when looking for a state
  * which meets some criteria. This is useful for when exploration is not done in one go (one function call) and so
  * the exploration state needs to be saved in between. The exploration is keyed on a canonical version of a puzzle,
  * ensuring equivalent puzzles with different undecided parts aren't treated differently.
  *
  * @param canonical Gets the canonical form of a puzzle.
  * @param initialStates When a puzzle hasn't been explored before, returns the first states to try.
  */
class CreateBuffer[Puzzle, Game] (canonical: Puzzle => Puzzle, initialStates: Puzzle => TraversableOnce[Game]) {
  private val toTryEnableStatesBuffer = new GeneratedMap[Puzzle, ArrayBuffer[Game]] (puzzle =>
    ArrayBuffer[Game] ().++= (initialStates (canonical (puzzle))))
  //private val toTryEnableStatesBuffer = new GeneratedMap[Puzzle, ArrayBuffer[Game]] (puzzle => ArrayBuffer[Game] (Game (puzzle)))
  private val triedEnableStatesBuffer = new GeneratedMap[Puzzle, ArrayBuffer[Game]] (_ => ArrayBuffer[Game] ())

  def toTryEnableStates (puzzle: Puzzle): ArrayBuffer[Game] =
    //toTryEnableStatesBuffer.getOrGenerate (Puzzle.toFullyDefined (puzzle), puzzle)
    toTryEnableStatesBuffer.getOrGenerate (canonical (puzzle), puzzle)

  def triedEnableStates (puzzle: Puzzle): ArrayBuffer[Game] =
    //triedEnableStatesBuffer.getOrGenerate (Puzzle.toFullyDefined (puzzle), puzzle)
    triedEnableStatesBuffer.getOrGenerate (canonical (puzzle), puzzle)
}
