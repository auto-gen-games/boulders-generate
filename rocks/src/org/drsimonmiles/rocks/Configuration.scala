package org.drsimonmiles.rocks

object Configuration {
  // The smallest width puzzle
  val startWidth = 5
  // The smallest height puzzle
  val startHeight = 5
  // Number of puzzles created for each width x height
  val numberToCreatePerSize = 10
  // When deciding whether to place a boulder in refining a puzzle, this is the bias against doing so
  // (to avoid puzzles filled with boulders): 0.0 means no bias, 1.0 means always first try not placing a boulder
  val biasAgainstBoulders = 0.5
  // Directory where puzzles are stored
  val puzzlesDirectory = "puzzles"
  // When time-out occurs
  val maxSolveTime = 10000l
  // Whether performance measuring is turned on
  var measuring = false
  // Whether logging is turned on
  var logging = false
  // Can be set to true to stop after generating all the puzzles for the current area
  var appTerminated = false
}
