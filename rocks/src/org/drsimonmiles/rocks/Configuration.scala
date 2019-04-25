package org.drsimonmiles.rocks

import java.io.File

object Configuration {
  val width = 20
  val height = 5
  val addStar = true
  val minLength = 15   // 55
  val minWeaving = 7    // 100
  val hopelessLength = width * height * 10
  val timePerAttempt = 120000  // 25000
  val puzzlesDirectory = "puzzles"
  val puzzlesFileName = s"puzzles-$width-by-$height.txt"
  val numberToCreate = 4
  val numberOfCores = 4

  val puzzlesFile = new File (new File (puzzlesDirectory), puzzlesFileName)
  var appTerminated = false
}
