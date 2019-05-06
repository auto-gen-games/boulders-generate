package org.drsimonmiles.rocks

import java.io.File

import org.drsimonmiles.rocks.Configuration._
import org.drsimonmiles.rocks.IO.addToFile
import org.drsimonmiles.rocks.PuzzleCreation.createPuzzle
import org.drsimonmiles.util.{Logger, Measure}

object Create extends App {
  println ("Running")

  if (args.length > 0 && args(0) == "show") {
    val puzzlesFile = new File (args (1))
    println (IO.load (puzzlesFile).map (p => Puzzle.toString (p)).mkString ("\n\n"))
    System.exit (0)
  }

  if (args.length > 0 && args(0) == "trial") {
    Configuration.logging = true
    Configuration.measuring = true
    val puzzle = createPuzzle (startWidth, startHeight, startWidth * startHeight * 10, startWidth * startHeight * 2000l, 0)
    if (puzzle.isDefined) println (Puzzle.toString (puzzle.get._1)) else println ("No puzzle found")
    Logger.saveLog ()
    println (Measure.toString)
    System.exit (0)
  }

  // Keep generating puzzles, using all the cores in parallel
  var height = startHeight
  var minLengthForHeight = 0
  while (!appTerminated) {
    var minLengthForArea = minLengthForHeight
    minLengthForHeight = minLengthForHeight.max ((for (width <- startWidth to height * 2) yield {
      println (s"Generating for $width x $height (min length: $minLengthForArea)")
      var created = 0
      val puzzlesFileName = s"puzzles-$width-by-$height.txt"
      val puzzlesFile = new File (new File (puzzlesDirectory), puzzlesFileName)
      var nextMinLength = minLengthForArea
      while (created < numberToCreatePerSize) {
          print (".")
          val newPuzzle = createPuzzle (width, height, width * height * 10, width * height * 5000l, minLengthForArea)
          if (newPuzzle.isDefined) {
            this.synchronized (print ("+"))
            nextMinLength = nextMinLength.max (Metrics.length (newPuzzle.get._2))
          }
          newPuzzle.foreach (puzzleSolution => addToFile (puzzlesFile, puzzleSolution._1))
          created += newPuzzle.size
        }
      minLengthForArea = nextMinLength
      minLengthForArea
    }).min)
    height += 1
  }
}
