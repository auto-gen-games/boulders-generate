package org.drsimonmiles.rocks

import org.drsimonmiles.util.GeneratedMap
import scala.collection.mutable.ArrayBuffer

class CreateBuffer {
  private val toTryEnableStatesBuffer = new GeneratedMap[Puzzle, ArrayBuffer[Game]] (puzzle => ArrayBuffer[Game] (Game (puzzle)))
  private val triedEnableStatesBuffer = new GeneratedMap[Puzzle, ArrayBuffer[Game]] (_ => ArrayBuffer[Game] ())

  def toTryEnableStates (puzzle: Puzzle): ArrayBuffer[Game] =
    toTryEnableStatesBuffer.getOrGenerate (Puzzle.toFullyDefined (puzzle), puzzle)

  def triedEnableStates (puzzle: Puzzle): ArrayBuffer[Game] =
    triedEnableStatesBuffer.getOrGenerate (Puzzle.toFullyDefined (puzzle), puzzle)
}
