package org.drsimonmiles.rocks

import java.io.File

object Configuration {
  val startWidth = 6
  val startHeight = 6
  val numberToCreatePerSize = 1
  val numberOfCores = 1
  val puzzlesDirectory = "puzzles"
  val maxSolveTime = 10000l
  var measuring = false
  var logging = false
  var appTerminated = false
}
